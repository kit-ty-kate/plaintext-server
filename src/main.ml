open Lwt.Infix

module Store = Irmin_unix.FS.KV(Irmin.Contents.String)
module Sync = Irmin.Sync(Store)

let store =
  Store.Repo.v (Irmin_fs.config "/tmp/test") >>= Store.empty

let callback _conn req body =
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
      Store.set store uri ~info:(Irmin_unix.info "msg") body >>= fun () ->
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"" ()

let () =
  Lwt_main.run begin
    Cohttp_lwt_unix.Server.create
      ~on_exn:(fun _ -> ())
      ~mode:(`TCP (`Port 8080))
      (Cohttp_lwt_unix.Server.make ~callback ())
  end
