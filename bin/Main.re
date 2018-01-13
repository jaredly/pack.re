
let parse = Minimist.parse(~alias=[("h", "help")], ~presence=["help"], ~multi=["rename"], ~strings=["base"]);

let help = {|
# pack.re - a simple js bundler for reason

Usage: pack.re [options] entry-file.js > bundle.js

  --base (default: current directory)
      expected to contain node_modules
  --rename newName=realName (can be defined multiple times)
      maps `require("newName")` to a node_module called "realName"
  -h, --help
      print this help
|};

let fail = (msg) => {
  Printf.eprintf("\n>>> ERROR: %s <<<\n", msg);
  Printf.eprintf("%s", help);
  exit(1);
};

switch (parse(List.tl(Array.to_list(Sys.argv)))) {
| Minimist.Error(err) => fail(Minimist.report(err))
| Ok(opts) =>
if (Minimist.StrSet.mem("help", opts.presence)) {
  print_endline(help); exit(0);
} else switch (opts.rest) {
  | [] => fail("Missing entry file")
  | [entry] => try(Pack.process(
      ~base=?Minimist.get(opts.strings, "base"),
      ~renames=
        List.map(item => switch (Str.split(Str.regexp("="), item)) {
        | [alias, m] => (alias, m)
        | _ => fail("Expected rename argument to be of the form alias=realname")
        }, Minimist.multi(opts.multi, "rename")),
      entry
    ) |> print_endline) {
      | Failure(message) => fail(message)
    }
  | _ => fail("Only one entry file allowed")
}
};
