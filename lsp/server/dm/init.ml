(*
 * M Language Server
 * Initializer
 *
 * 2025 Junyoung Park <jypark@ropas.snu.ac.kr>
 *)

open Ppx_yojson_conv_lib.Yojson_conv
open Yojson.Safe.Util
open Protocol

module ServerCapabilities = struct
  type t = {
    textDocumentSync : textDocumentSyncOptions;
    completionProvider : completionOptions;
    hoverProvider : bool;
    codeLensProvider : codeLensOptions;
    foldingRangeProvider : bool;
    semanticTokensProvider : semanticTokensOptions;
    diagnosticProvider : diagnosticOptions;
    documentSymbolProvider : bool;
  }
  [@@deriving yojson]

  and textDocumentSyncOptions = { openClose : bool; change : int }

  and completionOptions = {
    triggerCharacters : string list;
    resolveProvider : bool;
  }

  and codeLensOptions = { resolveProvider : bool }

  and semanticTokensOptions = {
    legend : semanticTokensLegend;
    range : bool;
    full : bool;
  }

  and semanticTokensLegend = {
    tokenTypes : string list;
    tokenModifiers : string list;
  }

  and diagnosticOptions = {
    interFileDependencies : bool;
    workspaceDiagnostics : bool;
  }

  let create_textDocumentSync ~(openClose : bool) ~(change : int) :
      textDocumentSyncOptions =
    { openClose; change }

  let create_completionOptions ~(triggerCharacters : string list)
      ~(resolveProvider : bool) : completionOptions =
    { triggerCharacters; resolveProvider }

  let create_codeLensOptions ~(resolveProvider : bool) : codeLensOptions =
    { resolveProvider }

  let create_semanticTokensOptions ~(legend : semanticTokensLegend)
      ~(range : bool) ~(full : bool) : semanticTokensOptions =
    { legend; range; full }

  let create_semanticTokensLegend ~(tokenTypes : string list)
      ~(tokenModifiers : string list) : semanticTokensLegend =
    { tokenTypes; tokenModifiers }

  let create_diagnosticOptions ~(interFileDependencies : bool)
      ~(workspaceDiagnostics : bool) : diagnosticOptions =
    { interFileDependencies; workspaceDiagnostics }

  let create ~(textDocumentSync : textDocumentSyncOptions)
      ~(completionProvider : completionOptions) ~(hoverProvider : bool)
      ~(codeLensProvider : codeLensOptions) ~(foldingRangeProvider : bool)
      ~(semanticTokensProvider : semanticTokensOptions)
      ~(diagnosticProvider : diagnosticOptions)
      ~(documentSymbolProvider : bool) : t =
    {
      textDocumentSync;
      completionProvider;
      hoverProvider;
      codeLensProvider;
      foldingRangeProvider;
      semanticTokensProvider;
      diagnosticProvider;
      documentSymbolProvider
    }
end

module InitializeResult = struct
  type t = { capabilities : ServerCapabilities.t; serverInfo : serverInfo }
  [@@deriving yojson]

  and serverInfo = { name : string; version : string }

  let create_serverInfo ~(name : string) ~(version : string) : serverInfo =
    { name; version }

  let create ~(capabilities : ServerCapabilities.t) ~(serverInfo : serverInfo) :
      t =
    { capabilities; serverInfo }
end

let textDocumentSync =
  ServerCapabilities.create_textDocumentSync ~openClose:true ~change:1

let completionProvider =
  ServerCapabilities.create_completionOptions ~triggerCharacters:[ "." ]
    ~resolveProvider:false

let hoverProvider = true

let codeLensProvider =
  ServerCapabilities.create_codeLensOptions ~resolveProvider:false

let foldingRangeProvider = false

let tokenTypes =
  [
    "enum";
    "parameter";
    "variable";
    "function";
    "keyword";
    "comment";
    "string";
    "number";
    "operator";
  ]

let tokenModifiers = []

let legend =
  ServerCapabilities.create_semanticTokensLegend ~tokenTypes ~tokenModifiers

let semanticTokensProvider =
  ServerCapabilities.create_semanticTokensOptions ~legend ~range:false
    ~full:true

let diagnosticProvider =
  ServerCapabilities.create_diagnosticOptions ~interFileDependencies:false
    ~workspaceDiagnostics:false

let documentSymbolProvider = true

let capabilities =
  ServerCapabilities.create ~textDocumentSync ~completionProvider ~hoverProvider
    ~codeLensProvider ~foldingRangeProvider ~semanticTokensProvider
    ~diagnosticProvider ~documentSymbolProvider

let serverInfo =
  InitializeResult.create_serverInfo ~name:"m-language-server" ~version:"0.0.1"

let initializeResult = InitializeResult.create ~capabilities ~serverInfo

let run id params =
  let result = InitializeResult.yojson_of_t initializeResult in
  send (Resp { id; result })
