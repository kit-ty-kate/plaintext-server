open Lwt.Infix

let callback store ~password _conn req body =
  store >>= fun store ->
  let uri = Cohttp.Request.uri req in
  let uri = Uri.path uri in
  let uri = String.split_on_char '/' uri in
  Cohttp_lwt.Body.is_empty body >>= function
  | true ->
      Store.find store uri >>= begin function
      | Some body -> Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
      | None -> Cohttp_lwt_unix.Server.respond_not_found ()
      end
  | false ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      let store body =
        Store.set store uri body >>= fun () ->
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"" ()
      in
      begin match password, lazy (String.Split.left ~by:"\n" body) with
      | None, _ -> store body
      | Some password, lazy (Some (password', body)) when String.equal password password' -> store body
      | Some _, _ -> Cohttp_lwt_unix.Server.respond_not_found ()
      end

let callback ~datadir ~password =
  let store = Store.from datadir in
  callback store ~password

let mkdir_p dir =
  let rec aux base = function
    | [] ->
        Lwt.return_unit
    | x::xs ->
        let dir = Filename.concat base x in
        Lwt.catch begin fun () ->
          Lwt_unix.mkdir dir 0o750
        end begin function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        | e -> Lwt.fail e
        end >>= fun () ->
        aux dir xs
  in
  match String.Split.list_cpy ~by:Filename.dir_sep dir with
  | ""::dirs -> aux Filename.dir_sep dirs
  | dirs -> aux Filename.current_dir_name dirs

let main common_name port password workdir =
  let keysdir = Filename.concat workdir "keys" in
  let datadir = Filename.concat workdir "data" in
  let crt = Filename.concat keysdir "certificate.pem" in
  let key = Filename.concat keysdir "key.pem" in
  let callback = callback ~datadir ~password in
  Lwt_main.run begin
    mkdir_p keysdir >>= fun () ->
    mkdir_p datadir >>= fun () ->
    if not (Sys.file_exists crt) || not (Sys.file_exists key) then begin
      match Selfsign.selfsign ~common_name ~certfile:crt ~keyfile:key with
      | `Ok -> ()
      | `Error -> assert false
    end;
    Cohttp_lwt_unix.Server.create
      ~on_exn:(fun _ -> ())
      ~mode:(`TLS (`Crt_file_path crt, `Key_file_path key, `No_password, `Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  end

let term =
  let doc_port = "Local port number to serve and receive files." in
  let doc_cn = "Common name. Usually the server domain name." in
  let doc_password = "Password required to be able to store data." in
  let ($) = Cmdliner.Term.($) in
  Cmdliner.Term.pure main $
  Cmdliner.Arg.(required & opt (some string) None & info ~doc:doc_cn ["cn"]) $
  Cmdliner.Arg.(value & opt int 8080 & info ~doc:doc_port ["p"; "port"]) $
  Cmdliner.Arg.(value & opt (some string) None & info ~doc:doc_password ["password"]) $
  Cmdliner.Arg.(required & pos 0 (some string) None & info ~docv:"WORKDIR" [])

let info =
  Cmdliner.Term.info
    ~doc:"An HTTP server serving and storing plaintext files."
    ~man:[`P "This program takes a work directory where every plaintext files \
              will be stored."]
    ~version:Config.version
    Config.name

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (term, info))
