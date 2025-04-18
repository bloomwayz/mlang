(*
 * M Language Server
 * CodeLens Provider
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Ppx_yojson_conv_lib.Yojson_conv
open Yojson.Safe.Util
open Protocol
open Document
open Range
open Inference

module HoverResult = struct
  type t = result option [@@yojson.option]
  and result = { contents : contents; range : Range.t }
  and contents = { kind : string; value : string } [@@deriving yojson]

  let markup value = "```ocaml\n" ^ value ^ "\n```"

  let create ~value ~range : t =
    let kind = "markdown" in
    let value = markup value in
    let contents = { kind; value } in
    Some { contents; range }
end

let infer_sub2 (st : States.state) (curr_pos : Position.t) =
  let subexp_opt =
    match st.parsedState with
    | Ast exp -> subexp_at_pos exp curr_pos
    | Fail _ -> None
  in
  (match st.typeState with
  | Checked env -> (
    match subexp_opt with
    | Some subexp ->
      let value = tystr_of_exp env subexp in
      let range = Range.from_location subexp.loc in
      Some (value, range)
    | None -> None)
  | _ -> None)

let compute params =
  let uri = get_uri params in
  let curr_pos = params |> member "position" |> Position.t_of_yojson in
  let st =
    match finds uri with Some st -> st | None -> failwith "Lookup failure"
  in
  match st.parsedState with
  | Ast ast -> (
      match infer_sub2 st curr_pos with
      | Some (value, range) ->
          (* let value = undisclose value in *)
          HoverResult.create ~value ~range
      | None -> None)
  | Fail _ -> None

let run id params =
  let result_ = compute params in
  let result = HoverResult.yojson_of_t result_ in
  send (Resp { id; result })
