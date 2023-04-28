#import "swaps.mligo" "Swaps"


type currency_param = {
    is_fa2: bool;
    min: nat;
    token_id: nat;
}

type update_currency = 
    | Add_currency of address * currency_param
    | Remove_currency of address

type t = {
    fa2s : address set;
    swaps : (nat, Swaps.t) big_map;
    currencies : (address, currency_param) map;
    metadata: (string, bytes) big_map;
    next_id : nat;
    admin: address;
    pending_admin: address option;
    fee: nat;
    paused: bool;
}

let add_currency (contract, details, store : address * currency_param * t) : t =  
     {store with currencies = 
        Map.update contract (Some(details)) store.currencies}

let remove_currency (contract, store : address * t) : t =  
     {store with currencies = 
        Map.remove contract store.currencies}