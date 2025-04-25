(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * M Language Server
 *)

open Yojson.Safe
open Yojson.Safe.Util

(** Message Types **)

type obj = Req of request | Resp of response | Notif of notification
and request = { id : int; method_ : string; params : Yojson.Safe.t }
and response = { id : int; result : Yojson.Safe.t }
and notification = { method_ : string; params_ : Yojson.Safe.t }

(** yojson to obj **)

let request_of_yojson yojson =
  let id = yojson |> member "id" |> to_int in
  let method_ = yojson |> member "method" |> to_string in
  let params = yojson |> member "params" in
  { id; method_; params }

let response_of_yojson yojson =
  let id = yojson |> member "id" |> to_int in
  let result = yojson |> member "result" in
  { id; result }

let notification_of_yojson yojson =
  let method_ = yojson |> member "method" |> to_string in
  let params_ = yojson |> member "params" in
  { method_; params_ }

let obj_of_yojson yojson =
  let is_null = fun x -> x = `Null in
  let is_resp = is_null (member "method" yojson) in
  let is_notif = is_null (member "id" yojson) in

  if is_resp then Resp (response_of_yojson yojson)
  else if is_notif then Notif (notification_of_yojson yojson)
  else Req (request_of_yojson yojson)

(** obj to yojson **)

let yojson_of_request { id; method_; params } =
  `Assoc [ ("id", `Int id); ("method", `String method_); ("params", params) ]

let yojson_of_response { id; result } =
  `Assoc [ ("id", `Int id); ("result", result) ]

let yojson_of_notification { method_; params_ } =
  `Assoc [ ("method", `String method_); ("params", params_) ]

(** Log handler **)

let current_id = ref (-1)

let id_of_obj = function
  | Req req -> Some req.id
  | Resp resp -> Some resp.id
  | Notif _ -> None

let make_prefix = function
  | Req _ -> "[Request]"
  | Resp _ -> "[Response]"
  | Notif _ -> "[Notification]"

let init_log () =
  let mode = [ Open_creat; Open_trunc ] in
  let oc = open_out_gen mode 0o664 "log.txt" in
  close_out oc

let output_log pref msg id =
  let mode = [ Open_creat; Open_append ] in
  let oc = open_out_gen mode 0o664 "log.txt" in
  Printf.fprintf oc "%s\n%s\n\n" pref msg;
  close_out oc;

  match id with Some x -> current_id := x | None -> ()

(** Input handler **)

let receive str =
  let yojson = from_string str in
  let obj = obj_of_yojson yojson in
  (* let prefix = make_prefix obj in let msg = pretty_to_string ~std:true yojson
     in let id = id_of_obj obj in

     output_log prefix msg id; *)
  obj

(** Output handler **)

let output_json msg =
  let size = String.length msg in
  Printf.printf "Content-Length: %d\r\n\r\n%s" size msg

let send obj =
  (* let prefix = make_prefix obj in *)
  let yojson =
    match obj with
    | Req x -> yojson_of_request x
    | Resp x -> yojson_of_response x
    | Notif x -> yojson_of_notification x
  in

  let msg = pretty_to_string ~std:true yojson in
  (* let id = id_of_obj obj in *)

  output_json msg
(* output_log prefix msg id *)
