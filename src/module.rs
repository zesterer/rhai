use std::fmt;
use std::fs::File;
use std::path::Path;
use std::sync::Mutex;
use std::error::Error;
use std::io::{Read};
use engine::{Scope, Engine};

pub struct Module {
    pub name: String,
    pub scope: Mutex<Scope>,
    pub script: String,
    pub engine: Engine,
    pub is_erroneous: bool,
    pub is_executed: bool,
}

// TODO - better errors
#[derive(Debug)]
pub enum ModuleError {
    FileAccessError,
    CouldntOpenFile,
    //EvaluationError(EvalAltResult),
    InvalidFilename,
}

impl Error for ModuleError {
    fn description(&self) -> &str {
        use ModuleError::*;

        match *self {
            FileAccessError => "error accessing file",
            CouldntOpenFile => "couldn't open file",
            //EvaluationError(_) => "error while evaluating",
            InvalidFilename => "invalid filename",
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl fmt::Display for ModuleError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Module {
    pub fn new() -> Module {
        Module {
            name: String::new(),
            scope: Mutex::new(Scope::new()),
            script: String::new(),
            engine: Engine::new(),
            is_erroneous: false,
            is_executed: false,
        }
    }

    // todo proper errors
    pub fn import<P: AsRef<Path>>(path: P) -> Result<Module, ModuleError> {
        let scope = Scope::new();
        let engine = Engine::new();
        let mut script = String::new();
        let name = if let Some(s) = path
            .as_ref()
            .to_str()
            .and_then(|x| Some(x.to_string()))
        {
            s
        } else { return Err(ModuleError::InvalidFilename) };

        match File::open(path) {
            Ok(mut f) => if f.read_to_string(&mut script).is_err() {
                return Err(ModuleError::FileAccessError);
            }
            Err(_) => return Err(ModuleError::CouldntOpenFile),
        }

        Ok(Module {
            name,
            scope: Mutex::new(scope),
            script,
            engine,
            is_erroneous: false,
            is_executed: false,
        })
    }

    pub fn exec(&mut self, parent: &Engine) {
        if let Some(reg) = parent.module_register {
            println!("register for module");
            reg(&mut self.engine);
        }

        if self.engine.consume_with_scope(&mut *self.scope.lock().unwrap(), &self.script).is_err() {
            self.is_erroneous = true;
        }

        self.is_executed = true;
    }
}

pub fn rhai_import(s: String) -> Module {
    match Module::import(&s) {
        Ok(m) => m,
        Err(e) => {
            println!("error: {}", e);
            let mut m = Module::new();
            m.is_erroneous = true;
            m
        }
    }
}
