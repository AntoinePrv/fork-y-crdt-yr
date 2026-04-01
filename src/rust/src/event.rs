use std::ops::Deref;

use extendr_api::prelude::*;
use yrs::Observable as YObservable;

use crate::utils;
use crate::{Origin, Transaction};

pub(crate) trait ExtendrObservable<E> {
    fn observe(&self, f: Function, key: &Robj) -> Result<(), Error>;

    fn unobserve(&self, key: &Robj) -> Result<(), Error>;
}

impl<T, Event, YEvent> ExtendrObservable<Event> for T
where
    T: Deref,
    T::Target: YObservable<Event = YEvent> + Sized,
    Event: utils::ExtendrRef<YEvent>,
    // Implied bound from the above not propagated
    ExternalPtr<Event>: utils::lifetime::Owner<YEvent>,
    // Implied bound from YObservable::observe
    yrs::types::Event: AsRef<YEvent>,
{
    fn observe(&self, f: Function, key: &Robj) -> Result<(), Error>
where {
        if f.formals().map(|g| g.len()).unwrap_or(0) != 2 {
            return Err(Error::Other(
                "Callback expect exactly two parameters: transaction and event".into(),
            ));
        }

        self.deref().observe_with(
            Origin::new(key)?,
            move |trans: &yrs::TransactionMut<'_>,
                  event: &<<Self as Deref>::Target as YObservable>::Event| {
                // Converting to Robj first as the converter will set the class symbol attribute,
                // otherwise it will only be seen as an `externalptr` from R.
                let event = Event::guard(event);
                let mut trans: Robj = Transaction::from_ref(trans).into();
                let result = f.call(pairlist!(trans.clone(), event.get().clone().into_robj()));
                TryInto::<&mut Transaction>::try_into(&mut trans)
                    .unwrap()
                    .unlock();
                // TODO Either take an on_error, or store it somewhere
                result.unwrap();
            },
        );

        Ok(())
    }

    fn unobserve(&self, key: &Robj) -> Result<(), Error> {
        self.deref().unobserve(Origin::new(key)?);
        Ok(())
    }
}
