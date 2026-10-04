type t = {
  name : string;
  age: int
}

let () =
  let a = { name = "had" ; age = 22 } in
  print_endline a.name