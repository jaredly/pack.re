
open Types;

let abspath = (path) => {
  let path = if (path.[0] == '.') {
    Filename.concat(Unix.getcwd(), path)
  } else {
    path
  };
  let parts = Str.split(Str.regexp("/"), path);
  let parts = ["", ...parts];
  let rec loop = (items) => switch items {
  | [] => []
  | [".", ...rest] => loop(rest)
  | ["..", ...rest] => ["..", ...loop(rest)]
  | [_, "..", ...rest] => loop(rest)
  | [item, ...rest] => [item, ...loop(rest)]
  };
  let rec chug = (items) => {
    let next = loop(items);
    if (next != items) {
      chug(next)
    } else {
      items
    }
  };
  String.concat(Filename.dir_sep, chug(parts))
};

let unwrap = (message, v) => switch v { | None => failwith(message) | Some(v) => v };

let rec process = (inEntryLand, state, isRelative, path) => {
  let inEntryLand = inEntryLand && isRelative;
  let path = abspath(path);
  if (Hashtbl.mem(state.ids, path)) {
    Hashtbl.find(state.ids, path);
  } else {
    state.nextId = state.nextId + 1;
    let id = state.nextId;
    Hashtbl.add(state.ids, path, id);
    let contents = ReasonCliTools.Files.readFile(path) |> unwrap("Required file not found: " ++ path);
    let requires = FindRequires.parseContents(path, contents);
    /* let requires = []; */
    let fixed = FixFile.process(state, path, contents, requires, process(inEntryLand));
    state.modules = [(id, path, fixed, inEntryLand), ...state.modules];
    id
  }
};

let mapOf = List.fold_left((m, (a, b)) => StrMap.add(a, b, m), StrMap.empty);

let makeRelative = (a, b) => {
  if (String.length(b) > String.length(a)) {
    if (String.sub(b, 0, String.length(a)) == a) {
      let res = String.sub(b, String.length(a), String.length(b) - String.length(a));
      if (res.[0] == '/') {
        "." ++ res
      } else {
        res
      }
    } else {
      b
    }
  } else {
    b
  }
};

let formatBundle = (mode, base, modules, entryIds) => {
  {|
;(function() { // generated by pack.re
let modules = {}
let initializers = {
    |} ++
  (List.map(
    ((id, path, body, _)) => string_of_int(id) ++ ": function(module, exports, require) {" ++  body
     ++ "\n//# sourceURL=" ++ makeRelative(base, path) ++ "\n}",
    modules
  ) |> String.concat(",\n  "))
  ++ {|
};
let nameMap = {
  |} ++
  (List.map(
    ((id, path, body, _)) => "\"" ++ String.escaped(path) ++ "\": " ++ string_of_int(id),
    modules
  ) |> String.concat(",\n  "))
  ++ {|
}
let require = (id) => {
  if (!modules[id]) {
    modules[id] = {exports: {}}
    initializers[id](modules[id], modules[id].exports, require)
  }
  return modules[id].exports
};
  |} ++
  (mode != ExternalEverything ? {|
window.packRequire = (name) => {
  if (nameMap[name]) return require(nameMap[name])
  else throw new Error("Unable to find external: " + name)
}
  |} : "")
  ++ (mode != JustExternals ? String.concat(";", List.map(id => Printf.sprintf("require(%d)", id), entryIds)) : "")
  ++ "})();"
};

let process = (~mode=Normal, ~renames, ~base=Unix.getcwd(), entries) => {
  let state = {
    entries,
    mode,
    alias: mapOf(renames),
    base,
    ids: Hashtbl.create(100),
    nextId: 0,
    modules: []
  };
  let ids = List.map(process(true, state, true), entries);
  let includedModules = state.mode == JustExternals ? List.filter(((_, _, _, inEntryLand)) => inEntryLand, state.modules) : state.modules;
  formatBundle(mode, base, state.modules, ids)
};