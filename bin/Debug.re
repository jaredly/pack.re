open ReasonCliTools;

switch (Sys.argv) {
| [|_, path|] => {
  let contents = switch (Files.readFile(path)) {
  | None => failwith("no file")
  | Some(x) => x
  };
  let requires = FindRequires.parseContents(path, contents);
  requires |> List.iter(({Types.pos, length, text}) => {
    Printf.printf("Require: %d - len %d -- %s", pos, length, text);
  });
  let state = Types.init();
  let fixed = FixFile.process(state, path, contents, requires, (_, _, _, _) => 1);
  print_endline(contents);
  print_endline(fixed);
}
| _ => print_endline("nope")
}
