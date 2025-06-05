#[cfg(feature = "c")]
pub mod c;
#[cfg(feature = "llvm")]
pub mod llvm;
#[cfg(feature = "qbe")]
pub mod qbe;

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum Backend {
    #[cfg(feature = "c")]
    C,
    #[cfg(feature = "llvm")]
    Llvm,
    #[cfg(feature = "qbe")]
    Qbe,
}

pub const ALL_BACKENDS: &[Backend] = &[
    #[cfg(feature = "c")]
    Backend::C,
    #[cfg(feature = "llvm")]
    Backend::Llvm,
    #[cfg(feature = "qbe")]
    Backend::Qbe,
];
