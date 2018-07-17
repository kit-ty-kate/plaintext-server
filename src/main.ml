open Lwt.Infix

let callback store _conn req body =
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
      Store.set store uri body >>= fun () ->
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"" ()

let callback workdir =
  let store = Store.from workdir in
  callback store

let main port workdir =
  let callback = callback workdir in
  Lwt_main.run begin
    Cohttp_lwt_unix.Server.create
      ~on_exn:(fun _ -> ())
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  end

let term =
  let doc_port = "Local port number to serve and receive files." in
  let ($) = Cmdliner.Term.($) in
  Cmdliner.Term.pure main $
  Cmdliner.Arg.(value & opt int 8080 & info ~doc:doc_port ["p"; "port"]) $
  Cmdliner.Arg.(required & pos 0 (some string) None & info ~docv:"WORKDIR" [])

let info =
  Cmdliner.Term.info
    ~doc:"An HTTP server serving and storing plaintext files."
    ~man:[`P "This program takes a work directory where every plaintext files \
              will be stored."]
    ~version:Config.version
    Config.name

let () = Cmdliner.Term.exit (Cmdliner.Term.eval (term, info))
