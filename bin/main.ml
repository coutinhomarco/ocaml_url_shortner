open Printf
open Hashtbl

let url_table = create 100

let generate_short_url () =
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let length = 6 in
  String.init length (fun _ -> chars.[Random.int (String.length chars)])

let shorten_url long_url =
  let short_url = generate_short_url () in
  add url_table short_url long_url;
  short_url

let get_original_url short_url =
  try
    find url_table short_url
  with Not_found ->
    "URL não encontrada"

let main () =
  Random.self_init ();

  if Array.length Sys.argv <> 2 then
    Printf.printf "Uso: %s <URL_longa>\n" Sys.argv.(0)
  else
    let long_url = Sys.argv.(1) in
    let short_url = shorten_url long_url in
    printf "URL original: %s\n" long_url;
    printf "URL encurtada: %s\n" short_url;
    printf "URL recuperada: %s\n" (get_original_url short_url);
    printf "URL inexistente: %s\n" (get_original_url "abc123")

let _ = main ()