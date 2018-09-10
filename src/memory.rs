
use memory_units::{Pages};

/// Maximal number of pages.
const LINEAR_MEMORY_MAX_PAGES: Pages = Pages(65536);



pub fn validate_memory(initial: Pages, maximum: Option<Pages>) -> Result<(), String> {
	if initial > LINEAR_MEMORY_MAX_PAGES {
		return Err(format!("initial memory size must be at most {} pages", LINEAR_MEMORY_MAX_PAGES.0));
	}
	if let Some(maximum) = maximum {
		if initial > maximum {
			return Err(format!(
				"maximum limit {} is less than minimum {}",
				maximum.0,
				initial.0,
			));
		}

		if maximum > LINEAR_MEMORY_MAX_PAGES {
			return Err(format!("maximum memory size must be at most {} pages", LINEAR_MEMORY_MAX_PAGES.0));
		}
	}
	Ok(())
}
