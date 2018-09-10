#![cfg(test)]

use std::fs::File;

use parity_wasm::{deserialize_buffer, SerializationError};
use pwasm_validation::{validate_module, Error as ValidationError};
use wabt::script::{self, Command, CommandKind, ScriptParser};

#[derive(Debug)]
enum Error {
    Script(script::Error),
    Serialization(SerializationError),
    Validation(ValidationError),
}

impl From<SerializationError> for Error {
    fn from(e: SerializationError) -> Error {
        Error::Serialization(e)
    }
}

impl From<ValidationError> for Error {
    fn from(e: ValidationError) -> Error {
        Error::Validation(e)
    }
}

impl From<script::Error> for Error {
    fn from(e: script::Error) -> Error {
        Error::Script(e)
    }
}

fn load_module(wasm: &[u8]) -> Result<(), Error> {
    let module = deserialize_buffer(wasm)?;
    let _validated_module = validate_module(module)?;
    Ok(())
}

pub fn spec(name: &str) {
    println!("running test: {}", name);
    try_spec(name).expect("Failed to run spec");
}

fn try_spec(name: &str) -> Result<(), Error> {
    let spec_script_path = format!("tests/spec/testsuite/{}.wast", name);

    use std::io::Read;
    let mut spec_source = Vec::new();
    let mut spec_file = File::open(&spec_script_path).expect("Can't open file");
    spec_file
        .read_to_end(&mut spec_source)
        .expect("Can't read file");

    let mut parser: ScriptParser = ScriptParser::from_source_and_name(&spec_source, &format!("{}.wast", name))
        .expect("Can't read spec script");

    while let Some(Command { kind, line }) = parser.next()? {
        println!("Running spec cmd {}: {:?}", line, kind);

        match kind {
            CommandKind::Module { module, .. }
            | CommandKind::AssertUninstantiable { module, .. }
            | CommandKind::AssertUnlinkable { module, .. } => {
                load_module(&module.into_vec()).expect("Failed to load module");
            }


            CommandKind::AssertInvalid { module, .. } => {
                match load_module(&module.into_vec()) {
                    Ok(_) => panic!("Expected invalid module definition, got some module!"),
                    Err(Error::Serialization(_)) // Some invalid modules are caught in deserialization
                    | Err(Error::Validation(_)) => {},
                    Err(e) => panic!("Expected validation error, got {:?}", e),
                }
            }

            CommandKind::AssertMalformed { module, .. } => {
                match load_module(&module.into_vec()) {
                    Ok(_) => panic!("Expected malformed module definition, got some module!"),
                    Err(Error::Serialization(_)) => {},
                    Err(e) => panic!("Expected serialization error, got {:?}", e),
                }
            }

            CommandKind::AssertReturn { .. }
            | CommandKind::AssertReturnCanonicalNan { .. }
            | CommandKind::AssertReturnArithmeticNan { .. }
            | CommandKind::AssertExhaustion { .. }
            | CommandKind::AssertTrap { .. }
            | CommandKind::Register { .. }
            | CommandKind::PerformAction(_) => {}

        }
    }

    Ok(())
}
