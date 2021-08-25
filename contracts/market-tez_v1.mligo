
(* FILE "fa2_interface.mligo" *)

#if ! FA2_INTERFACE
#define FA2_INTERFACE

type token_id = nat

type transfer_destination =
[@layout:comb]
{
  to_ : address;
  token_id : token_id;
  amount : nat;
}

type transfer =
[@layout:comb]
{
  from_ : address;
  txs : transfer_destination list;
}

type balance_of_request =
[@layout:comb]
{
  owner : address;
  token_id : token_id;
}

type balance_of_response =
[@layout:comb]
{
  request : balance_of_request;
  balance : nat;
}

type balance_of_param =
[@layout:comb]
{
  requests : balance_of_request list;
  callback : (balance_of_response list) contract;
}

type operator_param =
[@layout:comb]
{
  owner : address;
  operator : address;
  token_id: token_id;
}

type update_operator =
[@layout:comb]
  | Add_operator of operator_param
  | Remove_operator of operator_param

(*
type token_metadata =
[@layout:comb]
{
  token_id : token_id;
  symbol : string;
  name : string;
  decimals : nat;
  extras : (string, string) map;
}
*)

type token_metadata =
[@layout:comb]
  {
    token_id: token_id;
    token_info: ((string, bytes) map);
  }

type token_metadata_param =
[@layout:comb]
{
  token_ids : token_id list;
  handler : (token_metadata list) -> unit;
}

(*
One of the options to make token metadata discoverable is to declare
`token_metadata : token_metadata_storage` field inside the FA2 contract storage
*)
type token_metadata_storage = (token_id, token_metadata) big_map


type fa2_entry_points =
  | Transfer of transfer list
  | Balance_of of balance_of_param
  | Update_operators of update_operator list
  (* | Token_metadata_registry of address contract *)

type fa2_token_metadata =
  | Token_metadata of token_metadata_param

(* permission policy definition *)

type operator_transfer_policy =
  [@layout:comb]
  | No_transfer
  | Owner_transfer
  | Owner_or_operator_transfer

type owner_hook_policy =
  [@layout:comb]
  | Owner_no_hook
  | Optional_owner_hook
  | Required_owner_hook

type custom_permission_policy =
[@layout:comb]
{
  tag : string;
  config_api: address option;
}

type permissions_descriptor =
[@layout:comb]
{
  operator : operator_transfer_policy;
  receiver : owner_hook_policy;
  sender : owner_hook_policy;
  custom : custom_permission_policy option;
}

(* permissions descriptor entrypoint
type fa2_entry_points_custom =
  ...
  | Permissions_descriptor of permissions_descriptor contract
*)


type transfer_destination_descriptor =
[@layout:comb]
{
  to_ : address option;
  token_id : token_id;
  amount : nat;
}

type transfer_descriptor =
[@layout:comb]
{
  from_ : address option;
  txs : transfer_destination_descriptor list
}

type transfer_descriptor_param =
[@layout:comb]
{
  batch : transfer_descriptor list;
  operator : address;
}

(*
Entrypoints for sender/receiver hooks
type fa2_token_receiver =
  ...
  | Tokens_received of transfer_descriptor_param
type fa2_token_sender =
  ...
  | Tokens_sent of transfer_descriptor_param
*)

#endif

(* FILE "pauseable_admin_option.mligo" *)

(*
  One of the possible implementations of admin API for FA2 contract.
  The admin API can change an admin address using two step confirmation pattern and
  pause/unpause the contract. Only current admin can initiate those operations.
  Other entry points may guard their access using helper functions
  `fail_if_not_admin` and `fail_if_paused`.
*)

#if !PAUSEABLE_ADMIN
#define PAUSEABLE_ADMIN

(* `pauseable_admin` entry points *)
type pauseable_admin =
  | Set_admin of address
  | Confirm_admin of unit
  | Pause of bool


type pauseable_admin_storage_record = {
  admin : address;
  pending_admin : address option;
  paused : bool;
}

type pauseable_admin_storage = pauseable_admin_storage_record option

let confirm_new_admin (storage : pauseable_admin_storage) : pauseable_admin_storage =
  match storage with
    | Some s ->
        ( match s.pending_admin with
          | None -> (failwith "NO_PENDING_ADMIN" : pauseable_admin_storage)
          | Some pending ->
            if Tezos.sender = pending
            then (Some ({s with
              pending_admin = (None : address option);
              admin = Tezos.sender;
            } : pauseable_admin_storage_record))
            else (failwith "NOT_A_PENDING_ADMIN" : pauseable_admin_storage))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

(*Only fails if admin is enabled and sender is not admin*)
let fail_if_not_admin (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith "NOT_AN_ADMIN"
        else unit
    | None -> unit

(*Only fails if admin is enabled and sender is not admin*)
let fail_if_not_admin_ext (storage, extra_msg : pauseable_admin_storage * string) : unit =
  match storage with
    | Some a ->
        if Tezos.sender <> a.admin
        then failwith ("NOT_AN_ADMIN" ^  "_"  ^ extra_msg)
        else unit
    | None -> unit

(*Only callable by admin*)
let set_admin (new_admin, storage : address * pauseable_admin_storage) : pauseable_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with pending_admin = Some new_admin; } : pauseable_admin_storage_record))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

(*Only callable by admin*)
let pause (paused, storage: bool * pauseable_admin_storage) : pauseable_admin_storage =
  let u = fail_if_not_admin storage in
  match storage with
    | Some s ->
        (Some ({ s with paused = paused; } : pauseable_admin_storage_record ))
    | None -> (failwith "NO_ADMIN_CAPABILITIES_CONFIGURED" : pauseable_admin_storage)

let fail_if_paused (storage : pauseable_admin_storage) : unit =
  match storage with
    | Some a ->
        if a.paused
        then failwith "PAUSED"
        else unit
    | None -> unit

let pauseable_admin (param, storage : pauseable_admin *pauseable_admin_storage)
    : (operation list) * (pauseable_admin_storage) =
  match param with
  | Set_admin new_admin ->
      let new_s = set_admin (new_admin, storage) in
      (([] : operation list), new_s)

  | Confirm_admin u ->
      let new_s = confirm_new_admin storage in
      (([]: operation list), new_s)

  | Pause paused ->
      let new_s = pause (paused, storage) in
      (([]: operation list), new_s)

#endif


(* FILE "common.mligo" *)


#if !COMMON
#define COMMON

(*TYPES*)

type sale_id = nat

type fee_data = 
  [@layout:comb]
  {
    fee_address : address;
    fee_percent : nat;
  }

type fa2_tokens =
  [@layout:comb]
  {
    token_id : token_id;
    amount : nat;
  }
type tokens =
  [@layout:comb]
  {
    fa2_address : address;
    fa2_batch : (fa2_tokens list);
  }

type global_token_id =
  [@layout:comb]
  {
      fa2_address : address;
      token_id : token_id;
  }

(*MATH*) 

(*In English auction it is necessary to use ceiling so that bid is guaranteed to be raised*)
let ceil_div_nat (numerator, denominator : nat * nat) : nat = abs ((- numerator) / (int denominator))

let percent_of_bid_nat (percent, bid : nat * nat) : nat = 
  (ceil_div_nat (bid *  percent, 100n))

let ceil_div_tez (tz_qty, nat_qty : tez * nat) : tez = 
  let ediv1 : (tez * tez) option = ediv tz_qty nat_qty in 
  match ediv1 with 
    | None -> (failwith "DIVISION_BY_ZERO"  : tez) 
    | Some e -> 
       let (quotient, remainder) = e in
       if remainder > 0mutez then (quotient + 1mutez) else quotient

let percent_of_bid_tez (percent, bid : nat * tez) : tez = 
  (ceil_div_tez (bid *  percent, 100n))

(*In Fixed Price sale normal division is used to calculate fee*)

let percent_of_price_tez (percent, price : nat * tez) : tez = 
  ((price * percent)/ 100n) 

let percent_of_price_nat (percent, price : nat * nat) : nat = 
  ((price * percent)/ 100n) 

(*HELPERS*)

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let address_to_contract_transfer_entrypoint(add : address) : ((transfer list) contract) =
  let c : (transfer list) contract option = Tezos.get_entrypoint_opt "%transfer" add in
  match c with
    None -> (failwith "Invalid FA2 Address" : (transfer list) contract)
  | Some c ->  c

let resolve_contract (add : address) : unit contract =
  match ((Tezos.get_contract_opt add) : (unit contract) option) with
      None -> (failwith "Return address does not resolve to contract" : unit contract)
    | Some c -> c

let transfer_fa2(fa2_address, token_id, amount_, from, to_: address * token_id * nat * address * address): operation =
  let fa2_transfer : ((transfer list) contract) option =
      Tezos.get_entrypoint_opt "%transfer"  fa2_address in
  let transfer_op = match fa2_transfer with
  | None -> (failwith "CANNOT_INVOKE_FA2_TRANSFER" : operation)
  | Some c ->
    let tx = {
      from_ = from;
      txs= [{
        to_ = to_;
        token_id = token_id;
        amount = amount_;
    }]} in
    Tezos.transaction [tx] 0mutez c
 in transfer_op

#endif


(* FILE "fixed_price_sale_market_tez.mligo" *)

type sale_data_tez =
[@layout:comb]
{
  sale_token: global_token_id;
  price: tez;
  amount: nat;
}

type sale_tez =
[@layout:comb]
{
  seller: address;
  sale_data : sale_data_tez;
}

type storage =
[@layout:comb]
{
  admin: pauseable_admin_storage;
  sales: (sale_id, sale_tez) big_map;
  next_sale_id : sale_id;
}

type market_entry_points =
  | Sell of sale_data_tez
  | Buy of sale_id
  | Cancel of sale_id
  | Admin of pauseable_admin

let transfer_tez (qty, to_ : tez * address) : operation =
  let destination = (match (Tezos.get_contract_opt to_ : unit contract option) with
    | None -> (failwith "ADDRESS_DOES_NOT_RESOLVE" : unit contract)
    | Some acc -> acc) in 
  Tezos.transaction () qty destination

let buy_token(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let sale : sale_tez = (match Big_map.find_opt sale_id storage.sales with
  | None -> (failwith "NO_SALE": sale_tez)
  | Some s -> s) in 
  let sale_price : tez = sale.sale_data.price in
  let sale_token_address : address = sale.sale_data.sale_token.fa2_address in 
  let sale_token_id : nat = sale.sale_data.sale_token.token_id in 
  let amount_ : nat = sale.sale_data.amount in 
  let seller : address = sale.seller in 
  let amountError : unit =
    if Tezos.amount <> sale_price
    then ([%Michelson ({| { FAILWITH } |} : string * tez * tez -> unit)] ("WRONG_TEZ_PRICE", sale_price, Tezos.amount) : unit)
    else () in
  let tx_nft = transfer_fa2(sale_token_address, sale_token_id, 1n, Tezos.self_address, Tezos.sender) in
  let oplist : operation list = [tx_nft] in 

  let oplist = 
    (if sale_price <> 0mutez 
     then 
       let tx_price = transfer_tez(sale_price, seller) in
       tx_price :: oplist 
     else oplist) in 

  let new_sales : (sale_id, sale_tez) big_map = 
    if sale.sale_data.amount <= 1n 
    then Big_map.remove sale_id storage.sales
    else Big_map.update sale_id (Some {sale with sale_data.amount = abs (amount_ - 1n)}) storage.sales in
  let new_s = { storage with sales = new_sales } in
  oplist, new_s

let tez_stuck_guard(entrypoint: string) : string = "DON'T TRANSFER TEZ TO THIS ENTRYPOINT (" ^ entrypoint ^ ")"

let deposit_for_sale(sale_data, storage: sale_data_tez * storage) : (operation list * storage) =
    let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "SELL") else () in
    let sale_price : tez = sale_data.price in
    let sale_token_address : address = sale_data.sale_token.fa2_address in 
    let sale_token_id : nat = sale_data.sale_token.token_id in 
    let amount_ : nat = sale_data.amount in 
    let transfer_op = 
      transfer_fa2 (sale_token_address, sale_token_id, amount_, Tezos.sender, Tezos.self_address) in
    let sale = { seller = Tezos.sender; sale_data = sale_data } in
    let sale_id : sale_id = storage.next_sale_id in 
    let new_s = { storage with sales = Big_map.add sale_id sale storage.sales; 
      next_sale_id =  sale_id + 1n; } in
    [transfer_op], new_s

let cancel_sale(sale_id, storage: sale_id * storage) : (operation list * storage) =
  let u : unit = if Tezos.amount <> 0tez then failwith (tez_stuck_guard "CANCEL") else () in
  match Big_map.find_opt sale_id storage.sales with
    | None -> (failwith "NO_SALE" : (operation list * storage))
    | Some sale ->  let sale_token_address = sale.sale_data.sale_token.fa2_address in 
                    let sale_token_id = sale.sale_data.sale_token.token_id in 
                    let amount_ = sale.sale_data.amount in 
                    let seller = sale.seller in 
                    let is_seller = Tezos.sender = seller in
                    let v : unit = if is_seller then ()
                      else fail_if_not_admin_ext (storage.admin, "OR_A_SELLER") in
                    let tx_nft_back_op = transfer_fa2(sale_token_address, sale_token_id, amount_, Tezos.self_address, seller) in
                    [tx_nft_back_op], {storage with sales = Big_map.remove sale_id storage.sales }

let fixed_price_sale_tez_main (p, storage : market_entry_points * storage) : operation list * storage = match p with
  | Sell sale ->
     let u : unit = fail_if_paused(storage.admin) in

#if !CANCEL_ONLY_ADMIN
     let w : unit = fail_if_not_admin(storage.admin) in
#endif 
     deposit_for_sale(sale, storage)
  | Buy sale_id ->
     let u : unit = fail_if_paused(storage.admin) in

     buy_token(sale_id, storage)
  | Cancel sale_id ->
     let u : unit = fail_if_paused(storage.admin) in
     cancel_sale(sale_id,storage)
  | Admin a ->
     let ops, admin = pauseable_admin(a, storage.admin) in
     let new_storage = { storage with admin = admin; } in
     ops, new_storage


let sample_storage : storage =
  {
    admin = (None: pauseable_admin_storage );
    sales = (Big_map.empty : (sale_id, sale_tez) big_map);
    next_sale_id = 0n;
  }

(*VIEWS*)
let rec activeSalesHelper (active_sales, sale_id, s : (sale_tez list) * sale_id * storage) 
  : (sale_tez list) = 
  (if sale_id >= s.next_sale_id 
  then active_sales
  else ( match (Big_map.find_opt sale_id s.sales) with 
    | Some sale -> activeSalesHelper((sale :: active_sales), sale_id + 1n, s)
    | None -> activeSalesHelper(active_sales, sale_id + 1n, s)))

let getActiveSales (initial_sale_id , s : sale_id * storage) : (sale_tez list) = 
  (activeSalesHelper (([] : sale_tez list), initial_sale_id,  s))
 