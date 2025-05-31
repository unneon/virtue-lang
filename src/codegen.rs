pub mod c;
pub mod llvm;
pub mod qbe;

#[derive(Eq, Hash, PartialEq)]
pub enum Backend {
    C,
    Llvm,
    Qbe,
}

pub const ALL_BACKENDS: [Backend; 3] = [Backend::C, Backend::Llvm, Backend::Qbe];
