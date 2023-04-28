#import "errors.mligo" "Errors"

type t = {
  issuer : address;
  fa2 : address;
  token_id : nat;
  amount : nat;
  price : nat;
  currency : address;
  royalties : nat;
  creator : address; 
}

