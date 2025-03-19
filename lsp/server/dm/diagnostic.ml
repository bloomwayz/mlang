(*
 * M Language Server
 * Diagnostic Provider
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Ppx_yojson_conv_lib.Yojson_conv
open Yojson.Safe.Util
open Protocol
open Document
open Range
open Inference

module DiagnosticReport = struct
  type t = (report option[@yojson.option])
  and report = { kind : string; items : item list }

  and item = { range : Range.t; severity : int; message : string }
  [@@deriving yojson]

  let create ~range ~message : t =
    let kind = "full" in
    let severity = 1 in
    let item = { range; severity; message } in
    let items = [ item ] in
    Some { kind; items }
end

let get_rng_msg : States.pstate -> (Range.t * string) option = function
  | Ast ast -> (
      match check_top ast with
      | ty -> None
      | exception _ -> Some (Range.from_tuples (0, 0) (0, 0), "Type error"))
  | Fail (msg, ln, _) ->
      let rng = Range.from_tuples (ln, 0) (ln, 100) in
      Some (rng, msg)

let compute params =
  let uri = get_uri params in
  let pstate =
    match findp uri with Some x -> x | None -> failwith "Lookup failure"
  in
  match get_rng_msg pstate with
  | Some (range, message) -> DiagnosticReport.create ~range ~message
  | None -> None

let push id params =
  let result_ = compute params in
  let result = DiagnosticReport.yojson_of_t result_ in
  send (Resp { id; result })
