#import "errors.mligo" "Errors"
#import "swaps.mligo" "Swaps"
#import "admin.mligo" "Admin"
#import "transfer.mligo" "Transfer"
#import "storage.mligo" "Storage"

type currency_param = Storage.currency_param
type update_currency = Storage.update_currency

type storage = Storage.t

type parameter = 
    | Swap of Swaps.t
    | Unswap of nat
    | Collect of nat
    | Update_fa2s of address
    | Update_fee of nat
    | Update_currencies of update_currency
    | Set_admin of address
    | Confirm_admin  
    | Pause of bool

type return = operation list * storage

let swap(param, store : Swaps.t * Storage.t) : return =
    let _fail_if_paused : unit = Admin.fail_if_paused(store) in
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let _check_fa2 : unit = assert_with_error (Set.mem param.fa2 store.fa2s) Errors.false_fa2 in
    let currency : currency_param =
    match Map.find_opt param.currency store.currencies with 
    | None -> failwith(Errors.false_currency)
    | Some (currency) -> currency
    in
    let _check_swap_amount : unit = assert_with_error (param.amount > 0n) Errors.false_amount in
    let _check_price : unit = assert_with_error (param.price > currency.min) Errors.false_price in
    let _check_royalties : unit = assert_with_error (param.royalties <= 250n) Errors.false_royalties in
    let transfer : Transfer.t = {from_ = Tezos.get_sender(); txs = [{to_= Tezos.get_self_address(); amount = param.amount; token_id = param.token_id}]} in
    let op = Transfer.fa2_transfer (param.fa2, transfer) in
    let new_storage : Storage.t = {
        store with swaps = Big_map.update store.next_id (Some(param)) store.swaps;
        next_id=store.next_id + 1n;
    } in
    let event : operation  = Tezos.emit "%swap_event" param in
    event :: [op], new_storage

let unswap(swap_id, store : nat * Storage.t) : return =
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let sender = Tezos.get_sender() in 
    let swap : Swaps.t =
    match Big_map.find_opt swap_id store.swaps with 
    | None -> failwith(Errors.false_swap)
    | Some (swap) -> swap
    in
    let _check_swap_amount : unit = assert_with_error (swap.amount > 0n) Errors.zero_swap in
    let _check_issuer : unit = assert_with_error (sender = swap.issuer) Errors.false_issuer in
    let transfer : Transfer.t = {from_ = Tezos.get_self_address(); txs = [{to_= sender; amount = swap.amount; token_id = swap.token_id}]} in
    let op = Transfer.fa2_transfer (swap.fa2,transfer) in
    let swap : Swaps.t = {swap with amount = abs(swap.amount - 1n)} in
    let new_storage : Storage.t = {store with swaps = Big_map.update swap_id (Some(swap)) store.swaps} in
    let event : operation = Tezos.emit "%unswap_event" swap_id in
    event :: [op], new_storage

let collect(swap_id, store : nat * Storage.t) : return =
    let _fail_if_paused : unit = Admin.fail_if_paused(store) in
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let swap : Swaps.t =
    match Big_map.find_opt swap_id store.swaps with 
    | None -> failwith(Errors.false_swap)
    | Some (swap) -> swap
    in
    let _check_swap_amount : unit = assert_with_error (swap.amount > 0n) Errors.zero_swap in
    let currency : Storage.currency_param =
    match Map.find_opt swap.currency store.currencies with 
    | None -> failwith(Errors.false_currency)
    | Some (c) -> c
    in
    let sender = Tezos.get_sender() in
    let royalties : nat = (swap.price * swap.royalties)/1000n in
    let fee : nat = (swap.price * store.fee)/1000n in
    let ops: operation list  =
        if (currency.is_fa2) then 
            [Transfer.fa2_transfer
            (swap.currency, 
            {from_ = sender;
             txs = [{to_= swap.issuer;
             amount = abs(swap.price - (royalties+fee));
            token_id = currency.token_id}]});
            Transfer.fa2_transfer
            (swap.currency, 
            {from_ = sender;
             txs = [{to_= swap.creator;
             amount = royalties;
            token_id = currency.token_id}]});
            Transfer.fa2_transfer
            (swap.currency,
            {from_ = sender;
             txs = [{to_= store.admin;
             amount = fee;
            token_id = currency.token_id}]})
            ] 
        else
            [Transfer.fa12_transfer
                (swap.currency,
                {from = sender;
                to_= swap.issuer;
                value = abs(swap.price - (royalties+fee))});
                Transfer.fa12_transfer
                (swap.currency,
                {from = sender;
                to_= swap.creator;
                value = royalties}); 
                Transfer.fa12_transfer
                (swap.currency,
                {from = sender;
                to_= store.admin;
                value = fee})
            ] 
            in
    let transfer : Transfer.t = {from_ = Tezos.get_self_address(); txs = [{to_= sender; amount = 1n; token_id = swap.token_id}]} in
    let ops : operation list = Transfer.fa2_transfer (swap.fa2,transfer) :: ops in
    let swap : Swaps.t = {swap with amount = abs(swap.amount - 1n)} in
    let new_storage : Storage.t = {store with swaps =
    Big_map.update swap_id (Some(swap)) store.swaps} in
    let event : operation = Tezos.emit "%collect_event" {sender; swap_id} in
    event :: ops, new_storage

let update_fee (new_fee, store : nat *  Storage.t) : return =
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let _is_admin : unit = Admin.fail_if_not_admin(store) in 
    let _check_fee : unit = assert_with_error (new_fee < 250n) Errors.false_fa2 in
    let new_storage : Storage.t = {store with fee = new_fee} in
    let event : operation = Tezos.emit "%update_fee_event" new_fee in
    [event], new_storage

let update_fa2s(contract, store : address *  Storage.t) : return =
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let _is_admin : unit = Admin.fail_if_not_admin(store) in 
    let exists : bool = Set.mem contract store.fa2s in
    let new_storage : Storage.t = { store with fa2s = 
        if exists then Set.remove contract store.fa2s 
          else Set.add contract store.fa2s } in
    let event : operation = Tezos.emit "%update_fa2s_event" contract in
    [event], new_storage

let update_currencies(p, store : update_currency * Storage.t) : return =
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let _is_admin : unit = Admin.fail_if_not_admin(store) in 
    let new_storage : Storage.t = match p with
    | Add_currency (c,d) -> Storage.add_currency(c, d, store)
    | Remove_currency c -> Storage.remove_currency(c, store)
        in
    let event : operation = Tezos.emit "%update_currencies_event" p in
    [event], new_storage

let set_admin (a, store : address *  Storage.t) : return = 
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let _is_admin : unit = Admin.fail_if_not_admin(store) in 
    let new_storage : Storage.t = Admin.set_admin (a, store) in
    let event : operation = Tezos.emit "%set_admin_event" a in
    [event], new_storage

let confirm_admin (store : Storage.t) : return = 
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let _is_admin : unit = Admin.fail_if_not_admin(store) in 
    let new_storage : Storage.t = Admin.confirm_new_admin(store) in
    let event : operation = Tezos.emit "%confirm_admin_event" new_storage.admin in
    [event], new_storage

let pause(p, store : bool *  Storage.t) : return = 
    let _fail_if_tez : unit = if Tezos.get_amount() > 0tz then failwith(Errors.includes_tez) in 
    let _is_admin : unit = Admin.fail_if_not_admin(store) in 
    let new_storage : Storage.t = Admin.pause (p, store) in
    let event : operation =  Tezos.emit "%pause_event"  p in
    [event], new_storage

    
let main (ep : parameter) (store : storage) : return =
    match ep with
    | Swap(s) -> swap(s, store)
    | Unswap(u) -> unswap(u, store)
    | Collect (c) -> collect(c, store)
    | Update_fa2s(u) -> update_fa2s(u, store)
    | Update_fee(f) -> update_fee(f, store)
    | Update_currencies(p) -> update_currencies(p, store)
    | Set_admin(a) -> set_admin (a, store) 
    | Confirm_admin(_a) ->  confirm_admin(store) 
    | Pause(p) -> pause(p, store)



[@view] let view_swap(swap_id, store: nat * storage) : Swaps.t =
    match Big_map.find_opt swap_id store.swaps with
    | Some (swap) -> swap
    | None -> (failwith(Errors.false_swap) : Swaps.t)


[@view] let view_is_swap(swap_id, store : nat * storage) : bool =
      match Big_map.find_opt swap_id store.swaps with
    | Some (_swap) -> true
    | None -> false

[@view] let view_admin(_, store : unit * storage) : address =
    store.admin

[@view] let view_fee(_, store : unit * storage) : nat =
     store.fee

[@view] let view_is_fa2(contract, store : address * storage) : bool = 
    Set.mem contract store.fa2s

[@view] let view_is_currency(contract, store : address * storage) : bool = 
    match Map.find_opt contract store.currencies with
    | Some(_contract) -> true
    | None -> false

    //curency min view?