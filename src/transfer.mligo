#import "errors.mligo" "Errors"

type tx_param = 
[@layout:comb]{
    to_       : address;
    token_id  : nat;
    amount    : nat;
}

type t =
[@layout:comb]
{
    from_   : address;
    txs     : tx_param list;
}

type t_fa12 = 
[@layout:comb]
{ 
   from : address; 
   [@annot:to]to_: address; 
   value: nat; 
}

let fa2_transfer (fa2, t : address * t) : operation  =
  let fa2_entry : ((t list) contract) option = 
    Tezos.get_entrypoint_opt "%transfer" fa2 in
  match fa2_entry with
  | None -> (failwith Errors.transfer_fail : operation)
  | Some c -> Tezos.transaction [t] 0mutez c

let fa12_transfer (fa12, t_fa12 : address * t_fa12) : operation  =
  let fa12_entry : ((t_fa12) contract) option = 
    Tezos.get_entrypoint_opt "%transfer"  fa12 in
  match fa12_entry with
  | None -> (failwith Errors.transfer_fail : operation)
  | Some c -> Tezos.transaction t_fa12 0mutez c

