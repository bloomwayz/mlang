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

let compute params =
  let uri = get_uri params in
  match findt uri with
  | Checked _ -> None
  | Typerr message ->
    let range = Range.from_tuples (0, 0) (0, 0) in
    DiagnosticReport.create ~range ~message
  | Otherr (message, range) -> DiagnosticReport.create ~range ~message

let push id params =
  let result_ = compute params in
  let result = DiagnosticReport.yojson_of_t result_ in
  send (Resp { id; result })
