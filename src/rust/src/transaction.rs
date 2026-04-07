use extendr_api::prelude::*;
use yrs::updates::decoder::Decode as YDecode;
use yrs::{ReadTxn as YReadTxn, Transact as YTransact};

use crate::type_conversion::IntoExtendr;
use crate::utils;
use crate::{Doc, Snapshot, StateVector};

/// A reference to a readable transaction, for dispatch in the [`try_read`] macro.
pub(crate) enum DynTransactionRef<'a> {
    Read(&'a yrs::Transaction<'static>),
    Write(&'a yrs::TransactionMut<'static>),
}

macro_rules! try_read {
    ($txn:expr, $t:ident => $body:expr) => {
        $txn.try_dyn().map(|txn| match txn {
            crate::transaction::DynTransactionRef::Read($t) => $body,
            crate::transaction::DynTransactionRef::Write($t) => $body,
        })
    };
}

pub(crate) use try_read;

// Perhaps we could have two different bindings of Transaction and TransactionMut
// with the same API and use a macro to bind YTransact trait methods.
#[allow(clippy::large_enum_variant)]
pub enum OwnedTransaction<'doc> {
    Read(yrs::Transaction<'doc>),
    Write(yrs::TransactionMut<'doc>),
}

// Fields drop in declaration order: the transaction drops before the owner Robj,
// so the Doc is still alive when the transaction releases its borrow/lock.
struct OwnedYokedTransaction {
    transaction: OwnedTransaction<'static>,
    // Keeps the Doc alive while the transaction is alive.
    #[allow(dead_code)]
    owner: Robj,
}

pub struct RefTransaction<'doc>(pub(crate) &'doc yrs::TransactionMut<'doc>);

enum TransactionOrigin {
    Owned(OwnedYokedTransaction),
    Ref(RefTransaction<'static>),
}

// TODO this is unsound: the lifetime is still accessible from safe code.
// Either move to raw pointer + unsafe, or consider using yoke crate for this purpose.
#[extendr]
pub struct Transaction {
    transaction: Option<TransactionOrigin>,
}

pub(crate) trait ExtendrTransaction {
    fn try_dyn(&self) -> Result<DynTransactionRef<'_>, Error>;
    fn try_write(&self) -> Result<&yrs::TransactionMut<'static>, Error>;
    fn try_write_mut(&mut self) -> Result<&mut yrs::TransactionMut<'static>, Error>;

    fn is_mutable(&self) -> Result<bool, Error> {
        Ok(self.try_write().is_ok())
    }

    fn origin(&self) -> Result<Robj, Error> {
        match self.try_write() {
            Ok(trans) => match trans.origin() {
                Some(o) => Ok(Origin(o.clone()).into()),
                None => Ok(r!(NULL)),
            },
            Err(_) => Ok(r!(NULL)),
        }
    }

    fn commit(&mut self) -> Result<(), Error> {
        self.try_write_mut().map(|trans| trans.commit())
    }

    fn state_vector(&self) -> Result<StateVector, Error> {
        try_read!(self, t => t.state_vector().into())
    }

    fn encode_diff_v1(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        try_read!(self, t => t.encode_diff_v1(state_vector.as_ref()))
    }

    fn encode_diff_v2(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        try_read!(self, t => t.encode_diff_v2(state_vector.as_ref()))
    }

    fn encode_state_as_update_v1(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        try_read!(self, t => t.encode_state_as_update_v1(state_vector.as_ref()))
    }

    fn encode_state_as_update_v2(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        try_read!(self, t => t.encode_state_as_update_v2(state_vector.as_ref()))
    }

    fn apply_update_v1(&mut self, data: &[u8]) -> Result<(), Error> {
        let trans = self.try_write_mut()?;
        let update = yrs::Update::decode_v1(data).extendr()?;
        trans.apply_update(update).extendr()
    }

    fn apply_update_v2(&mut self, data: &[u8]) -> Result<(), Error> {
        let trans = self.try_write_mut()?;
        let update = yrs::Update::decode_v2(data).extendr()?;
        trans.apply_update(update).extendr()
    }

    fn snapshot(&self) -> Result<Snapshot, Error> {
        try_read!(self, t => t.snapshot().into())
    }
}

impl Transaction {
    pub(crate) fn from_ref(transaction: &yrs::TransactionMut<'_>) -> Self {
        let transaction = RefTransaction(transaction);
        // TODO Safety: None, unlock must be called while the original ref is valid
        let transaction = unsafe {
            std::mem::transmute::<RefTransaction<'_>, RefTransaction<'static>>(transaction)
        };
        Transaction {
            transaction: Some(TransactionOrigin::Ref(transaction)),
        }
    }
}

impl ExtendrTransaction for Transaction {
    fn try_dyn(&self) -> Result<DynTransactionRef<'_>, Error> {
        match &self.transaction {
            Some(TransactionOrigin::Owned(t)) => match &t.transaction {
                OwnedTransaction::Read(t) => Ok(DynTransactionRef::Read(t)),
                OwnedTransaction::Write(t) => Ok(DynTransactionRef::Write(t)),
            },
            Some(TransactionOrigin::Ref(t)) => Ok(DynTransactionRef::Write(t.0)),
            None => Err(Error::Other("Transaction was dropped".into())),
        }
    }

    fn try_write(&self) -> Result<&yrs::TransactionMut<'static>, Error> {
        match &self.transaction {
            Some(TransactionOrigin::Owned(t)) => match &t.transaction {
                OwnedTransaction::Write(t) => Ok(t),
                OwnedTransaction::Read(_) => Err(Error::Other("Transaction is readonly".into())),
            },
            Some(TransactionOrigin::Ref(t)) => Ok(t.0),
            None => Err(Error::Other("Transaction was dropped".into())),
        }
    }

    fn try_write_mut(&mut self) -> Result<&mut yrs::TransactionMut<'static>, Error> {
        match &mut self.transaction {
            Some(TransactionOrigin::Owned(t)) => match &mut t.transaction {
                OwnedTransaction::Write(t) => Ok(t),
                OwnedTransaction::Read(_) => Err(Error::Other("Transaction is readonly".into())),
            },
            Some(TransactionOrigin::Ref(_)) => Err(Error::Other("Transaction is readonly".into())),
            None => Err(Error::Other("Transaction was dropped".into())),
        }
    }
}

#[extendr]
impl Transaction {
    pub fn lock(
        doc: ExternalPtr<Doc>,
        #[extendr(default = "FALSE")] mutable: bool,
        #[extendr(default = "NULL")] origin: Nullable<&Origin>,
    ) -> Self {
        let doc_inner: &yrs::Doc = (*doc).as_ref();
        let transaction = match (mutable, origin) {
            (true, Nullable::NotNull(o)) => {
                OwnedTransaction::Write(doc_inner.transact_mut_with(o.0.clone()))
            }
            (true, Nullable::Null) => OwnedTransaction::Write(doc_inner.transact_mut()),
            (false, _) => OwnedTransaction::Read(doc_inner.transact()),
        };

        // Safety: Doc lives in R memory and is kept alive by the `owner` Robj in
        // OwnedInnerTransaction. R's GC is non-moving, so the pointer inside the transaction
        // remains valid as long as the Doc is not freed. The Robj prevents collection via
        // R_PreserveObject semantics. The transaction field drops before the owner (declaration
        // order), so the Doc is still alive when the transaction releases its lock.
        let transaction = unsafe {
            std::mem::transmute::<OwnedTransaction<'_>, OwnedTransaction<'static>>(transaction)
        };
        Transaction {
            transaction: Some(TransactionOrigin::Owned(OwnedYokedTransaction {
                transaction,
                owner: doc.into(),
            })),
        }
    }

    pub fn is_mutable(&self) -> Result<bool, Error> {
        ExtendrTransaction::is_mutable(self)
    }

    pub fn origin(&self) -> Result<Robj, Error> {
        ExtendrTransaction::origin(self)
    }

    pub fn commit(&mut self) -> Result<(), Error> {
        ExtendrTransaction::commit(self)
    }

    pub fn unlock(&mut self) {
        self.transaction = None;
    }

    pub fn state_vector(&self) -> Result<StateVector, Error> {
        ExtendrTransaction::state_vector(self)
    }

    pub fn encode_diff_v1(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        ExtendrTransaction::encode_diff_v1(self, state_vector)
    }

    pub fn encode_diff_v2(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        ExtendrTransaction::encode_diff_v2(self, state_vector)
    }

    pub fn encode_state_as_update_v1(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        ExtendrTransaction::encode_state_as_update_v1(self, state_vector)
    }

    pub fn encode_state_as_update_v2(&self, state_vector: &StateVector) -> Result<Vec<u8>, Error> {
        ExtendrTransaction::encode_state_as_update_v2(self, state_vector)
    }

    pub fn apply_update_v1(&mut self, data: &[u8]) -> Result<(), Error> {
        ExtendrTransaction::apply_update_v1(self, data)
    }

    pub fn apply_update_v2(&mut self, data: &[u8]) -> Result<(), Error> {
        ExtendrTransaction::apply_update_v2(self, data)
    }

    pub fn snapshot(&self) -> Result<Snapshot, Error> {
        ExtendrTransaction::snapshot(self)
    }
}

utils::extendr_struct!(#[extendr] pub Origin(yrs::Origin));

#[extendr]
impl Origin {
    pub fn new(data: &Robj) -> Result<Self, Error> {
        if let Ok(origin) = TryInto::<&Origin>::try_into(data) {
            Ok(Self(origin.0.clone()))
        } else if let Ok(n) = TryInto::<i64>::try_into(data) {
            Ok(Self(n.into()))
        } else if let Ok(n) = TryInto::<u64>::try_into(data) {
            Ok(Self(n.into()))
        } else if let Ok(b) = TryInto::<&[u8]>::try_into(data) {
            Ok(Self(b.into()))
        } else if let Ok(s) = TryInto::<&str>::try_into(data) {
            Ok(Self(s.into()))
        } else {
            Err(Error::Other("Invalid bytes for Origin".into()))
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }

    pub fn not_equal(&self, other: &Self) -> bool {
        self.0.ne(&other.0)
    }

    pub fn less_than(&self, other: &Self) -> bool {
        self.0.lt(&other.0)
    }

    pub fn less_than_equal(&self, other: &Self) -> bool {
        self.0.le(&other.0)
    }

    pub fn greater_than(&self, other: &Self) -> bool {
        self.0.gt(&other.0)
    }

    pub fn greater_than_equal(&self, other: &Self) -> bool {
        self.0.ge(&other.0)
    }

    pub fn to_string(&self) -> String {
        self.0.to_string()
    }

    pub fn to_bytes(&self) -> &[u8] {
        self.0.as_ref()
    }

    pub fn to_hex(&self) -> String {
        const HEX: &[u8; 16] = b"0123456789abcdef";

        self.0
            .as_ref()
            .iter()
            .flat_map(|&b| {
                [
                    HEX[(b >> 4) as usize] as char,
                    HEX[(b & 0x0f) as usize] as char,
                ]
            })
            .collect()
    }
}

impl From<Origin> for yrs::Origin {
    fn from(value: Origin) -> Self {
        value.0
    }
}

extendr_module! {
    mod transaction;
    impl Transaction;
    impl Origin;
}
