use extendr_api::prelude::*;

#[extendr]
struct ArDoc(yrs::Doc);

#[extendr]
impl ArDoc {
    fn new() -> Self {
        ArDoc(yrs::Doc::new())
    }

    fn client_id(&self) -> yrs::block::ClientID {
        self.0.client_id()
    }
}

/// Return string `"Hello world!"` to R.
/// @export
#[extendr]
fn hello_world() -> &'static str {
    "Hello world!"
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod yar;
    impl ArDoc;
}
