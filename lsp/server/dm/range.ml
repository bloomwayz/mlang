(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * M Language Server
 *)

open Yojson.Safe.Util

module Position = struct
  type t = { ln : int; col : int }

  let create ~ln ~col : t = { ln; col }

  let t_of_yojson yojson : t =
    let ln = yojson |> member "line" |> to_int in
    let col = yojson |> member "character" |> to_int in
    create ~ln ~col

  let yojson_of_t t = `Assoc [ ("line", `Int t.ln); ("character", `Int t.col) ]
  let ( < ) p1 p2 = p1.ln < p2.ln || (p1.ln = p2.ln && p1.col < p2.col)
  let ( <= ) p1 p2 = p1 < p2 || (p1.ln = p2.ln && p1.col <= p2.col)
end

module Range = struct
  type t = { start : Position.t; end_ : Position.t }

  let create ~start ~end_ : t = { start; end_ }

  let t_of_yojson yojson : t =
    let start = yojson |> member "start" |> Position.t_of_yojson in
    let end_ = yojson |> member "end" |> Position.t_of_yojson in
    create ~start ~end_

  let yojson_of_t t =
    `Assoc
      [
        ("start", t.start |> Position.yojson_of_t);
        ("end", t.end_ |> Position.yojson_of_t);
      ]

  let from_tuples stup etup =
    create
      ~start:(Position.create ~ln:(fst stup) ~col:(snd stup))
      ~end_:(Position.create ~ln:(fst etup) ~col:(snd etup))

  let from_pos (start_p : Lexing.position) (end_p : Lexing.position) =
    let _, sln, scl = Location.get_pos_info start_p in
    let _, eln, ecl = Location.get_pos_info end_p in
    from_tuples (sln - 1, scl) (eln - 1, ecl)

  let from_location (loc : Location.t) = from_pos loc.loc_start loc.loc_end

  let from_lexbuf (lexbuf : Lexing.lexbuf) =
    let start_p, end_p = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    let _, sln, scl = Location.get_pos_info start_p in
    let _, eln, ecl = Location.get_pos_info end_p in
    from_tuples (sln - 1, scl) (eln - 1, ecl)

  let contains (r1 : t) (r2 : t) = r1.start <= r2.start && r2.end_ <= r1.end_
  let contains_p (r : t) (p : Position.t) = r.start <= p && p < r.end_
end
