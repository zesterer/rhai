use std::fmt;
use std::fs::File;
use std::io::{Read};
use std::path::Path;
use std::error::Error;
use engine::{Scope, Engine};
use std::sync::{Mutex, Arc, RwLock};

/// Contains a Rhai module
/// This struct is usually wrapped in Module
#[derive(Clone)]
pub struct Module {
    /// Filename of the script (what was passed to `import`)
    pub name: String,
    /// Scope associated to Engine local to the module
    pub scope: Arc<Mutex<Scope>>,
    /// Contents of the script, used for execution, which can (in future) be optionally lazy
    pub script: String,
    /// Engine local to the module
    pub engine: Arc<RwLock<Engine>>,
    /// `true` if there was an error during execution
    pub is_erroneous: bool,
    /// `true` if module was already executed
    pub is_executed: bool,
}

/// An enum containing errors produced during loading and execution of a module
// TODO - better errors
#[derive(Debug)]
pub enum ModuleError {
    /// Unable to open file
    CouldntOpenFile,
    /// Able to access file, but cannot read it
    FileAccessError,
    //EvaluationError(EvalAltResult),
    /// Filename is not a valid string
    InvalidFilename,
}

// TODO - wrap Module in ArcMutex
//pub struct Module(Arc<Mutex<InnerModule>>);

impl Error for ModuleError {
    fn description(&self) -> &str {
        use ModuleError::*;

        match *self {
            FileAccessError => "error accessing file",
            CouldntOpenFile => "couldn't open file",
            //EvaluationError(_) => "error while evaluating", - TODO
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
    /// create a new Module
    pub fn new() -> Module {
        Module {
            name: String::new(),
            scope: Arc::new(Mutex::new(Scope::new())),
            script: String::new(),
            engine: Arc::new(RwLock::new(Engine::new())),
            is_erroneous: false,
            is_executed: false,
        }
    }

    /// manually import a module from path
    // todo proper errors
    pub fn import<P: AsRef<Path>>(path: P) -> Result<Module, ModuleError> {
        let scope = Scope::new();
        let engine = Arc::new(RwLock::new(Engine::new()));
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
            scope: Arc::new(Mutex::new(scope)),
            script,
            engine,
            is_erroneous: false,
            is_executed: false,
        })
    }

    /// execute a module
    pub fn exec(&mut self, parent: &Engine) {
        let mut engine = (*self.engine).write().unwrap();
        if let Some(reg) = parent.module_register {
            reg(&mut *engine);
        }

        if engine.consume_with_scope(&mut *self.scope.lock().unwrap(), &self.script).is_err() {
            self.is_erroneous = true;
        }

        self.is_executed = true;
    }
}

/// This function is registered to every Rhai engine.
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
