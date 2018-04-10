
let parse = Minimist.parse(~alias=[("h", "help")],
  ~presence=["help", "external-everything", "just-externals"],
  ~multi=["rename", "extra"],
  ~strings=["base"]
);

let help = {|
# pack.re - a simple js bundler for reason

Usage: pack.re [options] entry-file.js > bundle.js

  --base (default: current directory)
      expected to contain node_modules
  --external-everything
      everything that's not a local source file is treated as an external
      meaning it has to have been loaded by pack.re already on the page.
  --extra
      add another require to be treated as an external
  --just-externals
      This is the inverse of --external-everything. Only package up the exernals.
      With this, you can pass multiple entry-files, and it will produce a bundle
      that contains the exernals for all of them.
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
open Minimist;
if (Minimist.StrSet.mem("help", opts.presence)) {
  print_endline(help); exit(0);
} else {
    switch (opts.rest) {
    | [] => fail("Missing entry file")
    | entries => try(Pack.process(
        ~base=?Minimist.get(opts.strings, "base"),
        ~mode=if (Minimist.StrSet.mem("external-everything", opts.presence)) {
          ExternalEverything
        } else if (Minimist.StrSet.mem("just-externals", opts.presence)) {
          JustExternals
        } else {
          Normal
        },
        ~extraRequires=Minimist.multi(opts.multi, "extra"),
        ~renames=
          List.map(item => switch (Str.split(Str.regexp("="), item)) {
          | [alias, m] => (alias, m)
          | _ => fail("Expected rename argument to be of the form alias=realname")
          }, Minimist.multi(opts.multi, "rename")),
        entries
      ) |> print_endline) {
        | Failure(message) => fail(message)
      }
    }
  }
};
