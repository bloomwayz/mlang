(*
 * M Language Server
 * Main Loop
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Dm

let read_header () =
  let header = input_line stdin in
  let header_len = String.length header in

  let is_readable =
    header != "\r" && header_len >= 16
    && String.sub header 0 16 = "Content-Length: "
  in

  if is_readable then
    Some (String.sub header 16 (header_len - 16) |> String.trim |> int_of_string)
  else failwith "Header reading failure"

let read_content len =
  let _ = input_line stdin in
  really_input_string stdin len

let dispatch (obj : Protocol.obj) =
  let on_request id method_ params =
    match method_ with
    | "initialize" -> Init.run id params
    | "textDocument/hover" -> Hover.run id params
    | "textDocument/codeLens" -> CodeLens.run id params
    | "textDocument/diagnostic" -> Diagnostic.push id params
    | "textDocument/semanticTokens/full" -> Tokens.run id params
    | "textDocument/documentSymbol" -> Symbols.run id params
    (* | "textDocument/completion" -> Completion.run id params *)
    (* | "textDocument/foldingRange" -> () *)
    | unknown_method -> ()
  in

  let on_notification method_ params =
    match method_ with
    | "initialized" -> ()
    | "textDocument/didOpen" -> Document.on_did_open params
    | "textDocument/didChange" -> Document.on_did_change params
    | "textDocument/didClose" -> Document.on_did_close params
    | _ -> ()
    (* | unknown_method -> failwith ("unknown method: " ^ unknown_method) *)
  in

  match obj with
  | Req req -> on_request req.id req.method_ req.params
  | Notif notif -> on_notification notif.method_ notif.params_
  | _ -> ()

let rec loop () =
  match read_header () with
  | Some len ->
      let content = read_content len in
      let obj = Protocol.receive content in
      dispatch obj;
      flush_all ();
      loop ()
  | _ -> failwith "wtf?"

let () =
  (* Protocol.init_log (); *)
  loop ()
