//from fa2 lib
#import "errors.mligo" "Errors"
#import "storage.mligo" "Storage"


let confirm_new_admin (store : Storage.t) : Storage.t =
  match store.pending_admin with
  | None -> (failwith Errors.false_admin : Storage.t)
  | Some pending ->
    if Tezos.get_sender() = pending
    then { store with
      pending_admin = (None : address option);
      admin = Tezos.get_sender();
    }
    else (failwith Errors.false_admin : Storage.t)
  

let fail_if_not_admin (store : Storage.t) : unit =
  if Tezos.get_sender() <> store.admin
  then failwith Errors.false_admin
  else unit

//let is_admin (store : Storage.t) : bool = Tezos.get_sender() = store.admin

let fail_if_paused (store : Storage.t) : unit =
  if(store.paused)
  then failwith Errors.paused
  else unit

let set_admin (new_admin, store : address * Storage.t) : Storage.t =
  let _ = fail_if_not_admin store in
  { store with pending_admin = Some new_admin; }

let pause (paused, store: bool * Storage.t) : Storage.t =
  let _ = fail_if_not_admin store in
  { store with paused = paused; }


