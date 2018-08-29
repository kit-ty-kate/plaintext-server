(* Copyright (c) 2015, Mindy Preston meetup@yomimono.org *)

let translate_error dest = function
  | (Unix.EACCES) ->
      Error (Printf.sprintf "Permission denied writing %s" dest)
  | (Unix.EISDIR) ->
      Error (Printf.sprintf "%s already exists and is a directory" dest)
  | (Unix.ENOENT) ->
      Error (Printf.sprintf "Part of the path %s doesn't exist" dest)
  | (Unix.ENOSPC) -> Error "No space left on device"
  | (Unix.EROFS) ->
      Error (Printf.sprintf "%s is on a read-only filesystem" dest)
  | e -> Error (Unix.error_message e)

let make_dates days =
  let seconds = days * 24 * 60 * 60 in
  let start = Ptime_clock.now () in
  match Ptime.(add_span start @@ Span.of_int_s seconds) with
  | Some expire -> Some (start, expire)
  | None -> None

let extensions subject_pubkey auth_pubkey names =
  let subject_key_id =
    let cs = X509.key_id subject_pubkey in
    (false, `Subject_key_id cs)
  and authority_key_id =
    let cs = X509.key_id auth_pubkey in
    let x = (Some cs, [], None) in
    (false, `Authority_key_id x)
  and exts =
    let ku = (true, (`Key_usage [ `Digital_signature ; `Key_encipherment ]))
    and bc = (true, `Basic_constraints (false, None))
    in
    [ bc ; ku
    ; (true, (`Ext_key_usage [`Server_auth]))
    ]
  in
  let alts = match names with
    | [] -> []
    | _ -> [ (false, `Subject_alt_name ( List.map (fun x -> `DNS x) names)) ]
  in
  authority_key_id :: subject_key_id :: exts @ alts

let sign days key issuer csr names =
  match make_dates days with
  | None -> Error "Validity period is too long to express - try a shorter one"
  | Some (valid_from, valid_until) ->
      let pubkey = Nocrypto.Rsa.pub_of_priv key in
      let info = X509.CA.info csr in
      let extensions = extensions info.X509.CA.public_key (`RSA pubkey) names in
      let cert = X509.CA.sign ~valid_from ~valid_until ~extensions csr (`RSA key) issuer in
      Ok cert

let write_pem dest pem =
  try
    let fd = Unix.openfile dest [Unix.O_WRONLY; Unix.O_CREAT] 0o600 in
    (* single_write promises either complete failure (resulting in an exception)
       or complete success, so disregard the returned number of bytes written
       and just handle the exceptions *)
    let _written_bytes = Unix.single_write fd (Cstruct.to_bytes pem) 0 (Cstruct.len pem) in
    let () = Unix.close fd in
    Ok ()
  with
  | Unix.Unix_error (e, _, _) -> translate_error dest e

let selfsign ~common_name ~certfile ~keyfile =
  let length = 2048 in
  let days = 365 in
  Nocrypto_entropy_unix.initialize ();
  let privkey = Nocrypto.Rsa.generate length in
  let issuer = [ `CN common_name ] in
  let csr = X509.CA.request issuer (`RSA privkey) in
  match sign days privkey issuer csr [] with
  | Ok cert ->
      let cert_pem = X509.Encoding.Pem.Certificate.to_pem_cstruct1 cert in
      let key_pem = X509.Encoding.Pem.Private_key.to_pem_cstruct1 (`RSA privkey) in
      begin match write_pem certfile cert_pem, write_pem keyfile key_pem with
      | Ok (), Ok () -> `Ok
      | Error str, _
      | _, Error str -> Printf.eprintf "%s\n" str; `Error
      end
  | Error str -> Printf.eprintf "%s\n" str; `Error
