use std::u32;
use parity_wasm::elements::{Instruction, BlockType, ValueType, TableElementType, Func, FuncBody};
use super::{DEFAULT_MEMORY_INDEX, DEFAULT_TABLE_INDEX, Error};

use context::ModuleContext;
use util::Locals;
use stack::StackWithLimit;

/// Maximum number of entries in value stack per function.
const DEFAULT_VALUE_STACK_LIMIT: usize = 16384;
/// Maximum number of entries in frame stack per function.
const DEFAULT_FRAME_STACK_LIMIT: usize = 16384;

/// Control stack frame.
#[derive(Debug, Clone)]
struct BlockFrame {
	/// Frame type.
	frame_type: BlockFrameType,
	/// A signature, which is a block signature type indicating the number and types of result values of the region.
	block_type: BlockType,
	/// A label for reference to block instruction.
	begin_position: usize,
	/// A limit integer value, which is an index into the value stack indicating where to reset it to on a branch to that label.
	value_stack_len: usize,
	/// Boolean which signals whether value stack became polymorphic. Value stack starts in non-polymorphic state and
	/// becomes polymorphic only after an instruction that never passes control further is executed,
	/// i.e. `unreachable`, `br` (but not `br_if`!), etc.
	polymorphic_stack: bool,
}

/// Type of block frame.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BlockFrameType {
	/// Usual block frame.
	///
	/// Can be used for an implicit function block.
	Block {
		end_label: LabelId,
	},
	/// Loop frame (branching to the beginning of block).
	Loop {
		header: LabelId,
	},
	/// True-subblock of if expression.
	IfTrue {
		/// If jump happens inside the if-true block then control will
		/// land on this label.
		end_label: LabelId,

		/// If the condition of the `if` statement is unsatisfied, control
		/// will land on this label. This label might point to `else` block if it
		/// exists. Otherwise it equal to `end_label`.
		if_not: LabelId,
	},
	/// False-subblock of if expression.
	IfFalse {
		end_label: LabelId,
	}
}

impl BlockFrameType {
	/// Returns a label which should be used as a branch destination.
	fn br_destination(&self) -> LabelId {
		match *self {
			BlockFrameType::Block { end_label } => end_label,
			BlockFrameType::Loop { header } => header,
			BlockFrameType::IfTrue { end_label, .. } => end_label,
			BlockFrameType::IfFalse { end_label } => end_label,
		}
	}

	fn is_loop(&self) -> bool {
		match *self {
			BlockFrameType::Loop { .. } => true,
			_ => false,
		}
	}
}

/// Value type on the stack.
#[derive(Debug, Clone, Copy)]
enum StackValueType {
	/// Any value type.
	Any,
	/// Concrete value type.
	Specific(ValueType),
}

impl StackValueType {
	fn is_any(&self) -> bool {
		match self {
			&StackValueType::Any => true,
			_ => false,
		}
	}

	fn value_type(&self) -> ValueType {
		match self {
			&StackValueType::Any => unreachable!("must be checked by caller"),
			&StackValueType::Specific(value_type) => value_type,
		}
	}
}

impl From<ValueType> for StackValueType {
	fn from(value_type: ValueType) -> Self {
		StackValueType::Specific(value_type)
	}
}

impl PartialEq<StackValueType> for StackValueType {
	fn eq(&self, other: &StackValueType) -> bool {
		if self.is_any() || other.is_any() {
			true
		} else {
			self.value_type() == other.value_type()
		}
	}
}

impl PartialEq<ValueType> for StackValueType {
	fn eq(&self, other: &ValueType) -> bool {
		if self.is_any() {
			true
		} else {
			self.value_type() == *other
		}
	}
}

impl PartialEq<StackValueType> for ValueType {
	fn eq(&self, other: &StackValueType) -> bool {
		other == self
	}
}

/// Instruction outcome.
#[derive(Debug, Clone)]
enum Outcome {
	/// Continue with next instruction.
	NextInstruction,
	/// Unreachable instruction reached.
	Unreachable,
}

pub struct FunctionReader;

impl FunctionReader {
	pub fn read_function(
		module: &ModuleContext,
		func: &Func,
		body: &FuncBody,
	) -> Result<(), Error> {
		let (params, result_ty) = module.require_function_type(func.type_ref())?;

		let mut context = FunctionValidationContext::new(
			&module,
			Locals::new(params, body.locals())?,
			DEFAULT_VALUE_STACK_LIMIT,
			DEFAULT_FRAME_STACK_LIMIT,
			result_ty,
		);

		let end_label = context.new_label();
		push_label(
			BlockFrameType::Block {
				end_label,
			},
			result_ty,
			context.position,
			&context.value_stack,
			&mut context.frame_stack,
		)?;
		FunctionReader::read_function_body(&mut context, body.code().elements())?;

		assert!(context.frame_stack.is_empty());

		Ok(())
	}

	fn read_function_body(context: &mut FunctionValidationContext, body: &[Instruction]) -> Result<(), Error> {
		let body_len = body.len();
		if body_len == 0 {
			return Err(Error("Non-empty function body expected".into()));
		}

		loop {
			let instruction = &body[context.position];

			let outcome = FunctionReader::read_instruction(context, instruction)
				.map_err(|err| Error(format!("At instruction {:?}(@{}): {}", instruction, context.position, err)))?;

			match outcome {
				Outcome::NextInstruction => (),
				Outcome::Unreachable => make_top_frame_polymorphic(
					&mut context.value_stack,
					&mut context.frame_stack
				),
			}

			context.position += 1;
			if context.position == body_len {
				return Ok(());
			}
		}
	}

	fn read_instruction(context: &mut FunctionValidationContext, instruction: &Instruction) -> Result<Outcome, Error> {
		use self::Instruction::*;
		match *instruction {
			// Nop instruction doesn't do anything. It is safe to just skip it.
			Nop => {},

			Unreachable => {
				return Ok(Outcome::Unreachable);
			}

			Block(block_type) => {
				let end_label = context.new_label();
				push_label(
					BlockFrameType::Block {
						end_label
					},
					block_type,
					context.position,
					&context.value_stack,
					&mut context.frame_stack,
				)?;
			}
			Loop(block_type) => {
				// Resolve loop header right away.
				let header = context.new_label();

				push_label(
					BlockFrameType::Loop {
						header,
					},
					block_type,
					context.position,
					&context.value_stack,
					&mut context.frame_stack,
				)?;
			}
			If(block_type) => {
				// `if_not` will be resolved whenever `End` or `Else` operator will be met.
				// `end_label` will always be resolved at `End`.
				let if_not = context.new_label();
				let end_label = context.new_label();

				pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;
				push_label(
					BlockFrameType::IfTrue {
						if_not,
						end_label,
					},
					block_type,
					context.position,
					&context.value_stack,
					&mut context.frame_stack,
				)?;
			}
			Else => {
				let (block_type, _if_not, end_label) = {
					let top_frame = top_label(
						&context.frame_stack,
					);

					let (if_not, end_label) = match top_frame.frame_type {
						BlockFrameType::IfTrue { if_not, end_label } => (if_not, end_label),
						_ => return Err(Error("Misplaced else instruction".into())),
					};
					(top_frame.block_type, if_not, end_label)
				};

				// First, we need to finish if-true block: add a jump from the end of the if-true block
				// to the "end_label" (it will be resolved at End).
				// Resolve `if_not` to here so when if condition is unsatisfied control flow
				// will jump to this label.

				// Then, we pop the current label. It discards all values that pushed in the current
				// frame.
				pop_label(
					&mut context.value_stack,
					&mut context.frame_stack
				)?;
				push_label(
					BlockFrameType::IfFalse {
						end_label,
					},
					block_type,
					context.position,
					&context.value_stack,
					&mut context.frame_stack,
				)?;
			}
			End => {
				let (frame_type, block_type) = {
					let top = top_label(&context.frame_stack);
					(top.frame_type, top.block_type)
				};

				if let BlockFrameType::IfTrue { .. } = frame_type {
					// A `if` without an `else` can't return a result.
					if block_type != BlockType::NoResult {
						return Err(
							Error(
								format!(
									"If block without else required to have NoResult block type. But it has {:?} type",
									block_type
								)
							)
						);
					}

					// Resolve `if_not` label. If the `if's` condition doesn't hold the control will jump
					// to here.
				}

				if context.frame_stack.len() == 1 {
					// We are about to close the last frame. Insert
					// an explicit return.

					// Check the return type.
					if let BlockType::Value(value_type) = context.return_type()? {
						tee_value(
							&mut context.value_stack,
							&context.frame_stack,
							value_type.into()
						)?;
					}

					// Emit the return instruction.
					let _drop_keep = drop_keep_return(
						&context.locals,
						&context.value_stack,
						&context.frame_stack,
					);
				}

				pop_label(&mut context.value_stack, &mut context.frame_stack)?;

				// Push the result value.
				if let BlockType::Value(value_type) = block_type {
					push_value(&mut context.value_stack, value_type.into())?;
				}
			}
			Br(depth) => {
				Validator::validate_br(context, depth)?;

				let _target = require_target(
					depth,
					&context.value_stack,
					&context.frame_stack,
				);

				return Ok(Outcome::Unreachable);
			}
			BrIf(depth) => {
				Validator::validate_br_if(context, depth)?;

				let _target = require_target(
					depth,
					&context.value_stack,
					&context.frame_stack,
				);
			}
			BrTable(ref table, default) => {
				Validator::validate_br_table(context, table, default)?;

				for depth in table.iter() {
					let _target = require_target(
						*depth,
						&context.value_stack,
						&context.frame_stack,
					);
				}
				let _default_target = require_target(
					default,
					&context.value_stack,
					&context.frame_stack,
				);

				return Ok(Outcome::Unreachable);
			}
			Return => {
				if let BlockType::Value(value_type) = context.return_type()? {
					tee_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
				}

				let _drop_keep = drop_keep_return(
					&context.locals,
					&context.value_stack,
					&context.frame_stack,
				);

				return Ok(Outcome::Unreachable);
			}

			Call(index) => {
				Validator::validate_call(context, index)?;
			}
			CallIndirect(index, _reserved) => {
				Validator::validate_call_indirect(context, index)?;
			}

			Drop => {
				Validator::validate_drop(context)?;
			}
			Select => {
				Validator::validate_select(context)?;
			}

			GetLocal(index) => {
				Validator::validate_get_local(context, index)?;
			}
			SetLocal(index) => {
				Validator::validate_set_local(context, index)?;
			}
			TeeLocal(index) => {
				Validator::validate_tee_local(context, index)?;
			}
			GetGlobal(index) => {
				Validator::validate_get_global(context, index)?;
			}
			SetGlobal(index) => {
				Validator::validate_set_global(context, index)?;
			}

			I32Load(align, _offset) => {
				Validator::validate_load(context, align, 4, ValueType::I32)?;
			}
			I64Load(align, _offset) => {
				Validator::validate_load(context, align, 8, ValueType::I64)?;
			}
			F32Load(align, _offset) => {
				Validator::validate_load(context, align, 4, ValueType::F32)?;
			}
			F64Load(align, _offset) => {
				Validator::validate_load(context, align, 8, ValueType::F64)?;
			}
			I32Load8S(align, _offset) => {
				Validator::validate_load(context, align, 1, ValueType::I32)?;
			}
			I32Load8U(align, _offset) => {
				Validator::validate_load(context, align, 1, ValueType::I32)?;
			}
			I32Load16S(align, _offset) => {
				Validator::validate_load(context, align, 2, ValueType::I32)?;
			}
			I32Load16U(align, _offset) => {
				Validator::validate_load(context, align, 2, ValueType::I32)?;
			}
			I64Load8S(align, _offset) => {
				Validator::validate_load(context, align, 1, ValueType::I64)?;
			}
			I64Load8U(align, _offset) => {
				Validator::validate_load(context, align, 1, ValueType::I64)?;
			}
			I64Load16S(align, _offset) => {
				Validator::validate_load(context, align, 2, ValueType::I64)?;
			}
			I64Load16U(align, _offset) => {
				Validator::validate_load(context, align, 2, ValueType::I64)?;
			}
			I64Load32S(align, _offset) => {
				Validator::validate_load(context, align, 4, ValueType::I64)?;
			}
			I64Load32U(align, _offset) => {
				Validator::validate_load(context, align, 4, ValueType::I64)?;
			}

			I32Store(align, _offset) => {
				Validator::validate_store(context, align, 4, ValueType::I32)?;
			}
			I64Store(align, _offset) => {
				Validator::validate_store(context, align, 8, ValueType::I64)?;
			}
			F32Store(align, _offset) => {
				Validator::validate_store(context, align, 4, ValueType::F32)?;
			}
			F64Store(align, _offset) => {
				Validator::validate_store(context, align, 8, ValueType::F64)?;
			}
			I32Store8(align, _offset) => {
				Validator::validate_store(context, align, 1, ValueType::I32)?;
			}
			I32Store16(align, _offset) => {
				Validator::validate_store(context, align, 2, ValueType::I32)?;
			}
			I64Store8(align, _offset) => {
				Validator::validate_store(context, align, 1, ValueType::I64)?;
			}
			I64Store16(align, _offset) => {
				Validator::validate_store(context, align, 2, ValueType::I64)?;
			}
			I64Store32(align, _offset) => {
				Validator::validate_store(context, align, 4, ValueType::I64)?;
			}

			CurrentMemory(_) => {
				Validator::validate_current_memory(context)?;
			}
			GrowMemory(_) => {
				Validator::validate_grow_memory(context)?;
			}

			I32Const(_) => {
				Validator::validate_const(context, ValueType::I32)?;
			}
			I64Const(_) => {
				Validator::validate_const(context, ValueType::I64)?;
			}
			F32Const(_) => {
				Validator::validate_const(context, ValueType::F32)?;
			}
			F64Const(_) => {
				Validator::validate_const(context, ValueType::F64)?;
			}

			I32Eqz => {
				Validator::validate_testop(context, ValueType::I32)?;
			}
			I32Eq => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32Ne => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32LtS => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32LtU => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32GtS => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32GtU => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32LeS => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32LeU => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32GeS => {
				Validator::validate_relop(context, ValueType::I32)?;
			}
			I32GeU => {
				Validator::validate_relop(context, ValueType::I32)?;
			}

			I64Eqz => {
				Validator::validate_testop(context, ValueType::I64)?;
			}
			I64Eq => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64Ne => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64LtS => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64LtU => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64GtS => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64GtU => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64LeS => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64LeU => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64GeS => {
				Validator::validate_relop(context, ValueType::I64)?;
			}
			I64GeU => {
				Validator::validate_relop(context, ValueType::I64)?;
			}

			F32Eq => {
				Validator::validate_relop(context, ValueType::F32)?;
			}
			F32Ne => {
				Validator::validate_relop(context, ValueType::F32)?;
			}
			F32Lt => {
				Validator::validate_relop(context, ValueType::F32)?;
			}
			F32Gt => {
				Validator::validate_relop(context, ValueType::F32)?;
			}
			F32Le => {
				Validator::validate_relop(context, ValueType::F32)?;
			}
			F32Ge => {
				Validator::validate_relop(context, ValueType::F32)?;
			}

			F64Eq => {
				Validator::validate_relop(context, ValueType::F64)?;
			}
			F64Ne => {
				Validator::validate_relop(context, ValueType::F64)?;
			}
			F64Lt => {
				Validator::validate_relop(context, ValueType::F64)?;
			}
			F64Gt => {
				Validator::validate_relop(context, ValueType::F64)?;
			}
			F64Le => {
				Validator::validate_relop(context, ValueType::F64)?;
			}
			F64Ge => {
				Validator::validate_relop(context, ValueType::F64)?;
			}

			I32Clz => {
				Validator::validate_unop(context, ValueType::I32)?;
			}
			I32Ctz => {
				Validator::validate_unop(context, ValueType::I32)?;
			}
			I32Popcnt => {
				Validator::validate_unop(context, ValueType::I32)?;
			}
			I32Add => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32Sub => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32Mul => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32DivS => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32DivU => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32RemS => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32RemU => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32And => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32Or => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32Xor => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32Shl => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32ShrS => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32ShrU => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32Rotl => {
				Validator::validate_binop(context, ValueType::I32)?;
			}
			I32Rotr => {
				Validator::validate_binop(context, ValueType::I32)?;
			}

			I64Clz => {
				Validator::validate_unop(context, ValueType::I64)?;
			}
			I64Ctz => {
				Validator::validate_unop(context, ValueType::I64)?;
			}
			I64Popcnt => {
				Validator::validate_unop(context, ValueType::I64)?;
			}
			I64Add => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64Sub => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64Mul => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64DivS => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64DivU => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64RemS => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64RemU => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64And => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64Or => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64Xor => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64Shl => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64ShrS => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64ShrU => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64Rotl => {
				Validator::validate_binop(context, ValueType::I64)?;
			}
			I64Rotr => {
				Validator::validate_binop(context, ValueType::I64)?;
			}

			F32Abs => {
				Validator::validate_unop(context, ValueType::F32)?;
			}
			F32Neg => {
				Validator::validate_unop(context, ValueType::F32)?;
			}
			F32Ceil => {
				Validator::validate_unop(context, ValueType::F32)?;
			}
			F32Floor => {
				Validator::validate_unop(context, ValueType::F32)?;
			}
			F32Trunc => {
				Validator::validate_unop(context, ValueType::F32)?;
			}
			F32Nearest => {
				Validator::validate_unop(context, ValueType::F32)?;
			}
			F32Sqrt => {
				Validator::validate_unop(context, ValueType::F32)?;
			}
			F32Add => {
				Validator::validate_binop(context, ValueType::F32)?;
			}
			F32Sub => {
				Validator::validate_binop(context, ValueType::F32)?;
			}
			F32Mul => {
				Validator::validate_binop(context, ValueType::F32)?;
			}
			F32Div => {
				Validator::validate_binop(context, ValueType::F32)?;
			}
			F32Min => {
				Validator::validate_binop(context, ValueType::F32)?;
			}
			F32Max => {
				Validator::validate_binop(context, ValueType::F32)?;
			}
			F32Copysign => {
				Validator::validate_binop(context, ValueType::F32)?;
			}

			F64Abs => {
				Validator::validate_unop(context, ValueType::F64)?;
			}
			F64Neg => {
				Validator::validate_unop(context, ValueType::F64)?;
			}
			F64Ceil => {
				Validator::validate_unop(context, ValueType::F64)?;
			}
			F64Floor => {
				Validator::validate_unop(context, ValueType::F64)?;
			}
			F64Trunc => {
				Validator::validate_unop(context, ValueType::F64)?;
			}
			F64Nearest => {
				Validator::validate_unop(context, ValueType::F64)?;
			}
			F64Sqrt => {
				Validator::validate_unop(context, ValueType::F64)?;
			}
			F64Add => {
				Validator::validate_binop(context, ValueType::F64)?;
			}
			F64Sub => {
				Validator::validate_binop(context, ValueType::F64)?;
			}
			F64Mul => {
				Validator::validate_binop(context, ValueType::F64)?;
			}
			F64Div => {
				Validator::validate_binop(context, ValueType::F64)?;
			}
			F64Min => {
				Validator::validate_binop(context, ValueType::F64)?;
			}
			F64Max => {
				Validator::validate_binop(context, ValueType::F64)?;
			}
			F64Copysign => {
				Validator::validate_binop(context, ValueType::F64)?;
			}

			I32WrapI64 => {
				Validator::validate_cvtop(context, ValueType::I64, ValueType::I32)?;
			}
			I32TruncSF32 => {
				Validator::validate_cvtop(context, ValueType::F32, ValueType::I32)?;
			}
			I32TruncUF32 => {
				Validator::validate_cvtop(context, ValueType::F32, ValueType::I32)?;
			}
			I32TruncSF64 => {
				Validator::validate_cvtop(context, ValueType::F64, ValueType::I32)?;
			}
			I32TruncUF64 => {
				Validator::validate_cvtop(context, ValueType::F64, ValueType::I32)?;
			}
			I64ExtendSI32 => {
				Validator::validate_cvtop(context, ValueType::I32, ValueType::I64)?;
			}
			I64ExtendUI32 => {
				Validator::validate_cvtop(context, ValueType::I32, ValueType::I64)?;
			}
			I64TruncSF32 => {
				Validator::validate_cvtop(context, ValueType::F32, ValueType::I64)?;
			}
			I64TruncUF32 => {
				Validator::validate_cvtop(context, ValueType::F32, ValueType::I64)?;
			}
			I64TruncSF64 => {
				Validator::validate_cvtop(context, ValueType::F64, ValueType::I64)?;
			}
			I64TruncUF64 => {
				Validator::validate_cvtop(context, ValueType::F64, ValueType::I64)?;
			}
			F32ConvertSI32 => {
				Validator::validate_cvtop(context, ValueType::I32, ValueType::F32)?;
			}
			F32ConvertUI32 => {
				Validator::validate_cvtop(context, ValueType::I32, ValueType::F32)?;
			}
			F32ConvertSI64 => {
				Validator::validate_cvtop(context, ValueType::I64, ValueType::F32)?;
			}
			F32ConvertUI64 => {
				Validator::validate_cvtop(context, ValueType::I64, ValueType::F32)?;
			}
			F32DemoteF64 => {
				Validator::validate_cvtop(context, ValueType::F64, ValueType::F32)?;
			}
			F64ConvertSI32 => {
				Validator::validate_cvtop(context, ValueType::I32, ValueType::F64)?;
			}
			F64ConvertUI32 => {
				Validator::validate_cvtop(context, ValueType::I32, ValueType::F64)?;
			}
			F64ConvertSI64 => {
				Validator::validate_cvtop(context, ValueType::I64, ValueType::F64)?;
			}
			F64ConvertUI64 => {
				Validator::validate_cvtop(context, ValueType::I64, ValueType::F64)?;
			}
			F64PromoteF32 => {
				Validator::validate_cvtop(context, ValueType::F32, ValueType::F64)?;
			}

			I32ReinterpretF32 => {
				Validator::validate_cvtop(context, ValueType::F32, ValueType::I32)?;
			}
			I64ReinterpretF64 => {
				Validator::validate_cvtop(context, ValueType::F64, ValueType::I64)?;
			}
			F32ReinterpretI32 => {
				Validator::validate_cvtop(context, ValueType::I32, ValueType::F32)?;
			}
			F64ReinterpretI64 => {
				Validator::validate_cvtop(context, ValueType::I64, ValueType::F64)?;
			}
		}

		Ok(Outcome::NextInstruction)
	}
}

/// Function validator.
struct Validator;

impl Validator {
	fn validate_const(context: &mut FunctionValidationContext, value_type: ValueType) -> Result<(), Error> {
		push_value(&mut context.value_stack, value_type.into())?;
		Ok(())
	}

	fn validate_unop(context: &mut FunctionValidationContext, value_type: ValueType) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		push_value(&mut context.value_stack, value_type.into())?;
		Ok(())
	}

	fn validate_binop(context: &mut FunctionValidationContext, value_type: ValueType) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		pop_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		push_value(&mut context.value_stack, value_type.into())?;
		Ok(())
	}

	fn validate_testop(context: &mut FunctionValidationContext, value_type: ValueType) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		push_value(&mut context.value_stack, ValueType::I32.into())?;
		Ok(())
	}

	fn validate_relop(context: &mut FunctionValidationContext, value_type: ValueType) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		pop_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		push_value(&mut context.value_stack, ValueType::I32.into())?;
		Ok(())
	}

	fn validate_cvtop(context: &mut FunctionValidationContext, value_type1: ValueType, value_type2: ValueType) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, value_type1.into())?;
		push_value(&mut context.value_stack, value_type2.into())?;
		Ok(())
	}

	fn validate_drop(context: &mut FunctionValidationContext) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, StackValueType::Any)?;
		Ok(())
	}

	fn validate_select(context: &mut FunctionValidationContext) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;
		let select_type = pop_value(&mut context.value_stack, &context.frame_stack, StackValueType::Any)?;
		pop_value(&mut context.value_stack, &context.frame_stack, select_type)?;
		push_value(&mut context.value_stack, select_type)?;
		Ok(())
	}

	fn validate_get_local(context: &mut FunctionValidationContext, index: u32) -> Result<(), Error> {
		let local_type = require_local(&context.locals, index)?;
		push_value(&mut context.value_stack, local_type.into())?;
		Ok(())
	}

	fn validate_set_local(context: &mut FunctionValidationContext, index: u32) -> Result<(), Error> {
		let local_type = require_local(&context.locals, index)?;
		let value_type = pop_value(&mut context.value_stack, &context.frame_stack, StackValueType::Any)?;
		if  StackValueType::from(local_type) != value_type {
			return Err(Error(format!("Trying to update local {} of type {:?} with value of type {:?}", index, local_type, value_type)));
		}
		Ok(())
	}

	fn validate_tee_local(context: &mut FunctionValidationContext, index: u32) -> Result<(), Error> {
		let local_type = require_local(&context.locals, index)?;
		tee_value(&mut context.value_stack, &context.frame_stack, local_type.into())?;
		Ok(())
	}

	fn validate_get_global(context: &mut FunctionValidationContext, index: u32) -> Result<(), Error> {
		let global_type: StackValueType = {
			let global = context.module.require_global(index, None)?;
			global.content_type().into()
		};
		push_value(&mut context.value_stack, global_type)?;
		Ok(())
	}

	fn validate_set_global(context: &mut FunctionValidationContext, index: u32) -> Result<(), Error> {
		let global_type: StackValueType = {
			let global = context.module.require_global(index, Some(true))?;
			global.content_type().into()
		};
		let value_type = pop_value(&mut context.value_stack, &context.frame_stack, StackValueType::Any)?;
		if global_type != value_type {
			return Err(Error(format!("Trying to update global {} of type {:?} with value of type {:?}", index, global_type, value_type)));
		}
		Ok(())
	}

	fn validate_load(context: &mut FunctionValidationContext, align: u32, max_align: u32, value_type: ValueType) -> Result<(), Error> {
		if 1u32.checked_shl(align).unwrap_or(u32::MAX) > max_align {
			return Err(Error(format!("Too large memory alignment 2^{} (expected at most {})", align, max_align)));
		}

		pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;
		context.module.require_memory(DEFAULT_MEMORY_INDEX)?;
		push_value(&mut context.value_stack, value_type.into())?;
		Ok(())
	}

	fn validate_store(context: &mut FunctionValidationContext, align: u32, max_align: u32, value_type: ValueType) -> Result<(), Error> {
		if 1u32.checked_shl(align).unwrap_or(u32::MAX) > max_align {
			return Err(Error(format!("Too large memory alignment 2^{} (expected at most {})", align, max_align)));
		}

		context.module.require_memory(DEFAULT_MEMORY_INDEX)?;
		pop_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;
		Ok(())
	}

	fn validate_br(context: &mut FunctionValidationContext, depth: u32) -> Result<(), Error> {
		let (frame_type, frame_block_type) = {
			let frame = require_label(depth, &context.frame_stack)?;
			(frame.frame_type, frame.block_type)
		};
		if !frame_type.is_loop() {
			if let BlockType::Value(value_type) = frame_block_type {
				tee_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
			}
		}
		Ok(())
	}

	fn validate_br_if(context: &mut FunctionValidationContext, depth: u32) -> Result<(), Error> {
		pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;

		let (frame_type, frame_block_type) = {
			let frame = require_label(depth, &context.frame_stack)?;
			(frame.frame_type, frame.block_type)
		};
		if !frame_type.is_loop() {
			if let BlockType::Value(value_type) = frame_block_type {
				tee_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
			}
		}
		Ok(())
	}

	fn validate_br_table(context: &mut FunctionValidationContext, table: &[u32], default: u32) -> Result<(), Error> {
		let required_block_type: BlockType = {
			let default_block = require_label(default, &context.frame_stack)?;
			let required_block_type = if !default_block.frame_type.is_loop() {
				default_block.block_type
			} else {
				BlockType::NoResult
			};

			for label in table {
				let label_block = require_label(*label, &context.frame_stack)?;
				let label_block_type = if !label_block.frame_type.is_loop() {
					label_block.block_type
				} else {
					BlockType::NoResult
				};
				if required_block_type != label_block_type {
					return Err(
						Error(
							format!(
								"Labels in br_table points to block of different types: {:?} and {:?}",
								required_block_type,
								label_block.block_type
							)
						)
					);
				}
			}
			required_block_type
		};

		pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;
		if let BlockType::Value(value_type) = required_block_type {
			tee_value(&mut context.value_stack, &context.frame_stack, value_type.into())?;
		}

		Ok(())
	}

	fn validate_call(context: &mut FunctionValidationContext, idx: u32) -> Result<(), Error> {
		let (argument_types, return_type) = context.module.require_function(idx)?;
		for argument_type in argument_types.iter().rev() {
			pop_value(&mut context.value_stack, &context.frame_stack, (*argument_type).into())?;
		}
		if let BlockType::Value(value_type) = return_type {
			push_value(&mut context.value_stack, value_type.into())?;
		}
		Ok(())
	}

	fn validate_call_indirect(context: &mut FunctionValidationContext, idx: u32) -> Result<(), Error> {
		{
			let table = context.module.require_table(DEFAULT_TABLE_INDEX)?;
			if table.elem_type() != TableElementType::AnyFunc {
				return Err(Error(format!(
					"Table {} has element type {:?} while `anyfunc` expected",
					idx,
					table.elem_type()
				)));
			}
		}

		pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;
		let (argument_types, return_type) = context.module.require_function_type(idx)?;
		for argument_type in argument_types.iter().rev() {
			pop_value(&mut context.value_stack, &context.frame_stack, (*argument_type).into())?;
		}
		if let BlockType::Value(value_type) = return_type {
			push_value(&mut context.value_stack, value_type.into())?;
		}
		Ok(())
	}

	fn validate_current_memory(context: &mut FunctionValidationContext) -> Result<(), Error> {
		context.module.require_memory(DEFAULT_MEMORY_INDEX)?;
		push_value(&mut context.value_stack, ValueType::I32.into())?;
		Ok(())
	}

	fn validate_grow_memory(context: &mut FunctionValidationContext) -> Result<(), Error> {
		context.module.require_memory(DEFAULT_MEMORY_INDEX)?;
		pop_value(&mut context.value_stack, &context.frame_stack, ValueType::I32.into())?;
		push_value(&mut context.value_stack, ValueType::I32.into())?;
		Ok(())
	}
}

/// Function validation context.
struct FunctionValidationContext<'a> {
	/// Wasm module
	module: &'a ModuleContext,
	/// Current instruction position.
	position: usize,
	/// Local variables.
	locals: Locals<'a>,
	/// Value stack.
	value_stack: StackWithLimit<StackValueType>,
	/// Frame stack.
	frame_stack: StackWithLimit<BlockFrame>,
	/// Function return type.
	return_type: BlockType,
        /// Label creation.
        label_idx: usize,
}

impl<'a> FunctionValidationContext<'a> {
	fn new(
		module: &'a ModuleContext,
		locals: Locals<'a>,
		value_stack_limit: usize,
		frame_stack_limit: usize,
		return_type: BlockType,
	) -> Self {
		FunctionValidationContext {
			module: module,
			position: 0,
			locals: locals,
			value_stack: StackWithLimit::with_limit(value_stack_limit),
			frame_stack: StackWithLimit::with_limit(frame_stack_limit),
			return_type: return_type,
                        label_idx: 0,
		}
	}

	fn return_type(&self) -> Result<BlockType, Error> {
		Ok(self.return_type)
	}

        fn new_label(&mut self) -> LabelId {
            let id = LabelId(self.label_idx);
            self.label_idx += 1;
            id
        }
}

fn make_top_frame_polymorphic(
	value_stack: &mut StackWithLimit<StackValueType>,
	frame_stack: &mut StackWithLimit<BlockFrame>,
) {
	let frame = frame_stack.top_mut().expect("make_top_frame_polymorphic is called with empty frame stack");
	value_stack.resize(frame.value_stack_len, StackValueType::Any);
	frame.polymorphic_stack = true;
}

fn push_value(
	value_stack: &mut StackWithLimit<StackValueType>,
	value_type: StackValueType,
) -> Result<(), Error> {
	Ok(value_stack.push(value_type.into())?)
}

// TODO: Rename value_type -> expected_value_ty
fn pop_value(
	value_stack: &mut StackWithLimit<StackValueType>,
	frame_stack: &StackWithLimit<BlockFrame>,
	value_type: StackValueType,
) -> Result<StackValueType, Error> {
	let (is_stack_polymorphic, label_value_stack_len) = {
		let frame = top_label(frame_stack);
		(frame.polymorphic_stack, frame.value_stack_len)
	};
	let stack_is_empty = value_stack.len() == label_value_stack_len;
	let actual_value = if stack_is_empty && is_stack_polymorphic {
		StackValueType::Any
	} else {
		let value_stack_min = frame_stack
			.top()
			.expect("at least 1 topmost block")
			.value_stack_len;
		if value_stack.len() <= value_stack_min {
			return Err(Error("Trying to access parent frame stack values.".into()));
		}
		value_stack.pop()?
	};
	match actual_value {
		StackValueType::Specific(stack_value_type) if stack_value_type == value_type => {
			Ok(actual_value)
		}
		StackValueType::Any => Ok(actual_value),
		stack_value_type @ _ => Err(Error(format!(
			"Expected value of type {:?} on top of stack. Got {:?}",
			value_type, stack_value_type
		))),
	}
}

fn tee_value(
	value_stack: &mut StackWithLimit<StackValueType>,
	frame_stack: &StackWithLimit<BlockFrame>,
	value_type: StackValueType,
) -> Result<(), Error> {
	let _ = pop_value(value_stack, frame_stack, value_type)?;
	push_value(value_stack, value_type)?;
	Ok(())
}

fn push_label(
	frame_type: BlockFrameType,
	block_type: BlockType,
	position: usize,
	value_stack: &StackWithLimit<StackValueType>,
	frame_stack: &mut StackWithLimit<BlockFrame>,
) -> Result<(), Error> {
	Ok(frame_stack.push(BlockFrame {
		frame_type: frame_type,
		block_type: block_type,
		begin_position: position,
		value_stack_len: value_stack.len(),
		polymorphic_stack: false,
	})?)
}

// TODO: Refactor
fn pop_label(
	value_stack: &mut StackWithLimit<StackValueType>,
	frame_stack: &mut StackWithLimit<BlockFrame>,
) -> Result<(), Error> {
	// Don't pop frame yet. This is essential since we still might pop values from the value stack
	// and this in turn requires current frame to check whether or not we've reached
	// unreachable.
	let block_type = frame_stack.top()?.block_type;
	match block_type {
		BlockType::NoResult => (),
		BlockType::Value(required_value_type) => {
			let _ = pop_value(value_stack, frame_stack, StackValueType::Specific(required_value_type))?;
		}
	}

	let frame = frame_stack.pop()?;
	if value_stack.len() != frame.value_stack_len {
		return Err(Error(format!(
			"Unexpected stack height {}, expected {}",
			value_stack.len(),
			frame.value_stack_len
		)));
	}

	Ok(())
}

fn top_label(frame_stack: &StackWithLimit<BlockFrame>) -> &BlockFrame {
	frame_stack.top()
		.expect("this function can't be called with empty frame stack")
}

fn require_label(
	depth: u32,
	frame_stack: &StackWithLimit<BlockFrame>,
) -> Result<&BlockFrame, Error> {
	Ok(frame_stack.get(depth as usize)?)
}


/// Should we keep a value before "discarding" a stack frame?
///
/// Note that this is a `enum` since Wasm doesn't support multiple return
/// values at the moment.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Keep {
	None,
	/// Pop one value from the yet-to-be-discarded stack frame to the
	/// current stack frame.
	Single,
}

/// Specifies how many values we should keep and how many we should drop.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct DropKeep {
	pub drop: u32,
	pub keep: Keep,
}


fn require_target(
	depth: u32,
	value_stack: &StackWithLimit<StackValueType>,
	frame_stack: &StackWithLimit<BlockFrame>,
) -> Target {
	let is_stack_polymorphic = top_label(frame_stack)
		.polymorphic_stack;
	let frame =
		require_label(depth, frame_stack).expect("require_target called with a bogus depth");

	// Find out how many values we need to keep (copy to the new stack location after the drop).
	let keep: Keep = match (frame.frame_type, frame.block_type) {
		// A loop doesn't take a value upon a branch. It can return value
		// only via reaching it's closing `End` operator.
		(BlockFrameType::Loop { .. }, _) => Keep::None,

		(_, BlockType::Value(_)) => Keep::Single,
		(_, BlockType::NoResult) => Keep::None,
	};

	// Find out how many values we need to discard.
	let drop = if is_stack_polymorphic {
		// Polymorphic stack is a weird state. Fortunately, it always about the code that
		// will not be executed, so we don't bother and return 0 here.
		0
	} else {
		let value_stack_height = value_stack.len();
		assert!(
			value_stack_height >= frame.value_stack_len,
			"Stack underflow detected: value stack height ({}) is lower than minimum stack len ({})",
			value_stack_height,
			frame.value_stack_len,
		);
		assert!(
			(value_stack_height as u32 - frame.value_stack_len as u32) >= keep as u32,
			"Stack underflow detected: asked to keep {:?} values, but there are only {}",
			keep,
			value_stack_height as u32 - frame.value_stack_len as u32,
		);
		(value_stack_height as u32 - frame.value_stack_len as u32) - keep as u32
	};

	Target {
		label: frame.frame_type.br_destination(),
		drop_keep: DropKeep { drop, keep },
	}
}

fn drop_keep_return(
	locals: &Locals,
	value_stack: &StackWithLimit<StackValueType>,
	frame_stack: &StackWithLimit<BlockFrame>,
) -> DropKeep {
	assert!(
		!frame_stack.is_empty(),
		"drop_keep_return can't be called with the frame stack empty"
	);

	let deepest = (frame_stack.len() - 1) as u32;
	let mut drop_keep = require_target(deepest, value_stack, frame_stack).drop_keep;

	// Drop all local variables and parameters upon exit.
	drop_keep.drop += locals.count();

	drop_keep
}

fn require_local(locals: &Locals, idx: u32) -> Result<ValueType, Error> {
	Ok(locals.type_of_local(idx)?)
}


/// The target of a branch instruction.
///
/// It references a `LabelId` instead of exact instruction address. This is handy
/// for emitting code right away with labels resolved later.
#[derive(Clone)]
struct Target {
	label: LabelId,
	drop_keep: DropKeep,
}

/// Identifier of a label.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct LabelId(usize);
