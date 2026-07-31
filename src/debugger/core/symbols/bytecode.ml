open Ground
open Instruct

type debug_info = (Instruct.debug_event list * string list) list

let seek_section (pos, section_table) name =
  let rec seek_sec pos = function
    | [] -> raise Not_found
    | (name', len) :: rest ->
        let pos = Int64.sub pos (Int64.of_int len) in
        if name' = name then (pos, len) else seek_sec pos rest
  in
  seek_sec pos section_table

[%%if ocaml_version >= (5, 2, 0)]

(* Since 5.2, globals are numbered by [Symtable.Global.t] (compilation units and
   predefined exceptions) rather than by [Ident.t]. *)
let read_global_table ic toc =
  let pos, _ = seek_section toc "SYMB" in
  Lwt_io.set_position ic pos;%lwt
  let module T = struct
    type t = { cnt : int; tbl : int Symtable.Global.Map.t }
  end in
  let%lwt (global_table : T.t) = Lwt_io.read_value ic in
  let ident_of_global global =
    let name = Symtable.Global.name global in
    match global with
    | Symtable.Global.Glob_compunit _ -> Ident.create_persistent name
    | Symtable.Global.Glob_predef _ -> Ident.create_predef name
  in
  Lwt.return
    (global_table.tbl
    |> Symtable.Global.Map.to_seq
    |> Seq.map (fun (global, pos) -> (ident_of_global global, pos))
    |> Ident.Map.of_seq)

[%%else]

let read_global_table ic toc =
  let pos, _ = seek_section toc "SYMB" in
  Lwt_io.set_position ic pos;%lwt
  let module T = struct
    type t = { cnt : int; tbl : int Ident.Map.t }
  end in
  let%lwt (global_table : T.t) = Lwt_io.read_value ic in
  Lwt.return global_table.tbl

[%%endif]

let load_debuginfo file =
  let read_toc ic =
    let%lwt len = Lwt_io.length ic in
    let pos_trailer = Int64.sub len (Int64.of_int 16) in
    Lwt_io.set_position ic pos_trailer;%lwt
    let%lwt num_sections = Lwt_io.BE.read_int ic in
    let%lwt magic =
      Lwt_io.read_string_exactly ic (String.length Config.exec_magic_number)
    in
    if%lwt Lwt.return (magic <> Config.exec_magic_number) then
      Lwt.fail_invalid_arg "Bad magic";%lwt
    let pos_toc = Int64.sub pos_trailer (Int64.of_int (8 * num_sections)) in
    Lwt_io.set_position ic pos_toc;%lwt
    let section_table = ref [] in
    for%lwt i = 1 to num_sections do
      let%lwt name = Lwt_io.read_string_exactly ic 4 in
      let%lwt len = Lwt_io.BE.read_int ic in
      section_table := (name, len) :: !section_table;
      Lwt.return_unit
    done;%lwt
    Lwt.return (pos_toc, !section_table)
  in
  let relocate_event orig ev =
    ev.Instruct.ev_pos <- orig + ev.Instruct.ev_pos;
    match ev.ev_repr with Event_parent repr -> repr := ev.ev_pos | _ -> ()
  in
  let read_eventlists ic toc =
    let pos, _ = seek_section toc "DBUG" in
    Lwt_io.set_position ic pos;%lwt
    let%lwt num_eventlists = Lwt_io.BE.read_int ic in
    let eventlists = ref [] in
    for%lwt i = 1 to num_eventlists do
      let%lwt orig = Lwt_io.BE.read_int ic in
      let%lwt evl = Lwt_io.read_value ic in
      let evl = (evl : Instruct.debug_event list) in
      List.iter (relocate_event orig) evl;
      let%lwt (dirs : string list) = Lwt_io.read_value ic in
      eventlists := (evl, dirs) :: !eventlists;
      Lwt.return ()
    done;%lwt
    Lwt.return (List.rev !eventlists)
  in
  let%lwt ic =
    if Sys.win32 then
      (*
        WINDOWS & OCAML 5 ENVIRONMENT LOGIC

        This conditional block is explicitly required to bypass a deep
        architectural regression that occurs when using Lwt_io on Windows
        under OCaml 5+ environments.

        1. THE ARCHITECTURAL SHIFT:
           In OCaml 4, the Windows `Unix` layer relied on the standard C Runtime
           (CRT) stream descriptors. To natively support Multicore/Domains in
           OCaml 5, the core compiler team completely overhauled the Win32
           backend, stripping out the CRT layers and mapping `Unix.file_descr`
           directly to native asynchronous Windows `HANDLE` objects.

        2. LWT_IO SEEK POINTER MISMATCH:
           `Lwt_io` utilizes aggressive look-ahead buffering. When operations
           like `Lwt_io.length` or `Lwt_io.set_position` perform an OS-level
           seek, Lwt executes a strict internal sanity validation:
              if (actual_os_offset <> expected_logical_buffer_pos) then fail

           Under OCaml 5's new Win32 native `HANDLE` architecture, the physical
           file pointers tracked by the OS frequently drift from Lwt's buffered
           expectations by a few bytes during sequential reads. This pointer
           mismatch forces the assertion to fail, triggering an unexpected and
           fatal `Failure "Lwt_io.length: seek failed"` crash.

        3. EXECUTING IN-MEMORY AS A BYTE-STREAM WORKAROUND:
           To circumvent the broken file-pointer comparison loop, this Windows
           branch handles file resolution entirely in memory. It pulls the file
           metadata synchronously, memory-maps the data structure using
           `Lwt_bytes.map_file`, and generates a virtual stream wrapper using
           `Lwt_io.of_bytes`.

           This transforms the channel's type into an internal memory layout
           (`Type_bytes`). Because all downstream length and seek calculations
           are performed purely on RAM offsets rather than physical Windows
           kernel handles, the bugged validation check is bypassed completely.
      *)
      let%lwt fd = Lwt_unix.openfile file [Unix.O_RDONLY] 0o644 in
      let%lwt stats = Lwt_unix.LargeFile.fstat fd in
      let size = Int64.to_int stats.Unix.LargeFile.st_size in
      let raw_fd = Lwt_unix.unix_file_descr fd in
      let bytes_buffer = Lwt_bytes.map_file ~fd:raw_fd ~shared:false ~size () in
      let chan = Lwt_io.of_bytes ~mode:Lwt_io.input bytes_buffer in
      Lwt.return chan
    else
      Lwt_io.open_file ~mode:Lwt_io.input file
  in
  (let%lwt toc = read_toc ic in
   let%lwt globals = read_global_table ic toc in
   let%lwt eventlists = read_eventlists ic toc in
   Lwt.return (globals, eventlists))
    [%finally Lwt_io.close ic]
