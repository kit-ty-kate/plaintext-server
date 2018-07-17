open Lwt.Infix

type dir = string
type key = string list

type t = string

module Path = struct
  let add_seg base x =
    if String.is_empty x ||
       String.equal x Filename.current_dir_name ||
       String.equal x Filename.parent_dir_name ||
       String.mem ~sub:Filename.dir_sep x then
      failwith "Relative path are forbidden for obvious security reasons"
    else
      Filename.concat base x

  let to_list x =
    String.split ~by:Filename.dir_sep x

  let dirname x =
    String.concat Filename.dir_sep (List.rev (List.tl (List.rev (to_list x))))
end

let mkdir_p dir =
  let rec aux base = function
    | [] ->
        Lwt.return_unit
    | x::xs ->
        let dir = Path.add_seg base x in
        Lwt.catch begin fun () ->
          Lwt_unix.mkdir dir 0o750
        end begin function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        | e -> Lwt.fail e
        end >>= fun () ->
        aux dir xs
  in
  match Path.to_list dir with
  | ""::dirs -> aux Filename.dir_sep dirs
  | dirs -> aux Filename.current_dir_name dirs

let from workdir =
  mkdir_p workdir >|= fun () ->
  workdir

let get_path_from_key workdir k =
  List.fold_left Path.add_seg workdir k

let find workdir k =
  let path = get_path_from_key workdir k in
  Lwt.catch begin fun () ->
    Lwt_io.with_file
      ~flags:Unix.[O_RDONLY]
      ~mode:Lwt_io.Input
      path
      (Lwt_io.read ?count:None) >>=
    Lwt.return_some
  end begin function
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_none
  | e -> Lwt.fail e
  end

let set workdir k x =
  let path = get_path_from_key workdir k in
  mkdir_p (Path.dirname path) >>= fun () ->
  Lwt_io.with_file
    ~flags:Unix.[O_WRONLY; O_CREAT; O_TRUNC]
    ~mode:Lwt_io.Output
    path
    (fun c -> Lwt_io.write c x)
