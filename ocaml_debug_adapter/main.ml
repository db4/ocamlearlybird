module Session_set = BatSet.Make (struct
    type t = Session.t
    let compare a b = Session.id a - Session.id b
  end)

let sessions = ref Session_set.empty

let on_connection in_chan out_chan =
  let session = Session.create in_chan out_chan in
  sessions := Session_set.add session !sessions;
  Session.start session;%lwt
  sessions := Session_set.remove session !sessions;
  Lwt.return_unit

let at_exit () =
  Lwt_list.iter_p (fun session ->
    Session.shutdown session
  ) (Session_set.to_list !sessions);%lwt
  Lwt.return_unit

let start = function
  | Some port ->
    let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let%lwt _ = Lwt_io.establish_server_with_client_address addr (
      fun _ (in_chan, out_chan) -> on_connection in_chan out_chan
    ) in
    fst (Lwt.wait ())
  | None -> on_connection Lwt_io.stdin Lwt_io.stdout

let command server (port : int) =
  let server = if server then Some port else None in
  let term_waiter, term_wakener = Lwt.wait () in
  let term_handler = fun signum ->
    Lwt.wakeup_exn term_wakener (Exit)
  in
  Lwt_unix.on_signal Sys.sigint term_handler |> ignore;
  Lwt_unix.on_signal Sys.sigterm term_handler |> ignore;
  Lwt_main.at_exit at_exit;
  Lwt.async_exception_hook := (fun exn ->
    print_endline (Printexc.to_string exn);
  );
  Lwt_main.run (
    try%lwt Lwt.pick [start server; term_waiter]
    with Exit -> Lwt.return_unit
  )

let () =
  Printexc.record_backtrace true;
  let open Cmdliner in
  let server = Arg.(value & flag & info ["server"]) in
  let port = Arg.(value & opt int 4711 & info ["port"]) in
  let doc = "make OCaml debugging less sucks" in
  Term.(exit @@ eval (
    const command $ server $ port,
    info ~version:"%%VERSION%%" ~doc "ocamlearlybird"
  ))
