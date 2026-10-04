(* The operator symbol behind each generated function name (`+` for
   `op___0___`), filled by lib/parser.mly as operators get declared, so that
   error messages can show `+` instead of the internal name. *)
let symbol_of_fn : (string, string) Hashtbl.t = Hashtbl.create 16

let register symbol fn = Hashtbl.replace symbol_of_fn fn symbol

let is_digit c = c >= '0' && c <= '9'
let is_ident_char c = is_digit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '\''

let starts_with s i prefix =
  let n = String.length prefix in
  i + n <= String.length s && String.sub s i n = prefix

(* "int__test_struct" -> ["int"; "test_struct"]: overload names join their
   types with a double underscore (see [mkoverloadimpl] in lib/parser.mly). *)
let split_double_underscore s =
  let rec go acc start i =
    if i >= String.length s then List.rev (String.sub s start (i - start) :: acc)
    else if starts_with s i "__" then go (String.sub s start (i - start) :: acc) (i + 2) (i + 2)
    else go acc start (i + 1)
  in
  List.filter (( <> ) "") (go [] 0 0)

(* Rewrites every `op___N___` in [s] into `+`, and an overload's
   `op___N_____ovl__int__float` into `+` for (int, float). Unknown numbers
   are left as they are. *)
let prettify (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec go i =
    if i >= len then ()
    else if starts_with s i "op___" then begin
      let j = ref (i + 5) in
      while !j < len && is_digit s.[!j] do incr j done;
      if !j > i + 5 && starts_with s !j "___" then begin
        let base = String.sub s i (!j + 3 - i) in
        let k = ref (!j + 3) in
        let tys =
          if starts_with s !k "__ovl__" then begin
            let start = !k + 7 in
            k := start;
            while !k < len && is_ident_char s.[!k] do incr k done;
            split_double_underscore (String.sub s start (!k - start))
          end
          else []
        in
        (match Hashtbl.find_opt symbol_of_fn base with
         | Some sym ->
           Buffer.add_string buf (Printf.sprintf "`%s`" sym);
           if tys <> [] then Buffer.add_string buf (Printf.sprintf " for (%s)" (String.concat ", " tys))
         | None -> Buffer.add_string buf (String.sub s i (!k - i)));
        go !k
      end
      else begin
        Buffer.add_char buf s.[i];
        go (i + 1)
      end
    end
    else begin
      Buffer.add_char buf s.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents buf
