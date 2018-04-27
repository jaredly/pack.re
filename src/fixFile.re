
let sliceToEnd = (str, pos) => String.sub(str, pos, String.length(str) - pos);

let rec firstPart = parts => {
  let d = Filename.dirname(parts);
  if (d == ".") {
    parts
  } else if (parts.[0] == '@' && Filename.dirname(d) == ".") {
    parts
  } else {
    firstPart(d)
  }
};

let exists = path => try {Unix.stat(path) |> ignore; true} {
| Unix.Unix_error(Unix.ENOENT, _, _) => false
| Unix.Unix_error(Unix.ENOTDIR, _, _) => false
};

let startsWith = (thing, prefix) => String.sub(thing, 0, String.length(prefix)) == prefix;

let higherNodeModules = (path) => {
  let parts = Str.split(Str.regexp(Filename.dir_sep), path);
  let rec loop = parts => switch parts {
  | ["node_modules", ...rest] => Some(rest)
  | [_, ...rest] => loop(rest)
  | [] => None
  };
  let subs = loop(List.rev(parts));
  switch subs {
  | Some(parts) => Some(String.concat(Filename.dir_sep, List.rev(parts)))
  | None => None
  };
};

let (/+) = Filename.concat;

open ReasonCliTools;

let hasPackageJson = d => Files.isFile(d /+ "package.json");

let commonNodeBuiltins = ["fs", "child_process", "path"];

let rec findNodeModule = (needle, current, root) => {
  let full = current /+ "node_modules" /+ needle;
  if (hasPackageJson(current) && Files.isDirectory(full)) {
    Some(full)
  } else if (current == root) {
    None
  } else {
    findNodeModule(needle, Filename.dirname(current), root)
  }
};

open Types;

open ReasonCliTools;

let (|>>) = (fn, v) => switch v { | None => None | Some(v) => fn(v) };
let unwrap = (message, v) => switch v { | None => failwith(message) | Some(v) => v };

let resolvePackageJsonMain = foundPath => {
  let contents = ReasonCliTools.Files.readFile(Filename.concat(foundPath, "package.json"));
  let data = Json.parse(contents |> unwrap("Unable to read package.json " ++ foundPath));
  switch (data |> Json.get("main")) {
  | Some(Json.String(v)) => Filename.concat(foundPath, v)
  | _ => if (Files.isFile(Filename.concat(foundPath, "index.js"))) {
      Filename.concat(foundPath, "index.js")
    } else {
      failwith("No main found in package.json " ++ foundPath)
    }
  }
};

type resolution =
  | Found(string, option(string))
  | NotFound(string)
  | NoDefault(string)
  | Builtin(string);

let resolve = (state, base, path) => {
  if (String.length(path) == 0) {
    failwith("Invalid require - empty string")
  } else {
    let initial = if (path.[0] == '.') {
      `Good(Filename.concat(Filename.dirname(base), path), None)
    } else if (path.[0] == '/') {
      `Good(path, None) /* absolute folks */
    } else {
      let moduleName = firstPart(path);
      /* print_endline(moduleName ++ ":" ++ path); */
      let rest = moduleName != path ? sliceToEnd(path, String.length(moduleName) + 1) : "";
      let aliasedName = if (StrMap.mem(moduleName, state.alias)) {
        StrMap.find(moduleName, state.alias)
      } else {
        moduleName
      };
      if (aliasedName.[0] == '/') {
        `Good(Filename.concat(aliasedName, rest), None)
      } else {
        if (List.mem(aliasedName, commonNodeBuiltins)) {
          `Builtin(aliasedName)
        } else {
          switch (findNodeModule(aliasedName, Filename.dirname(base), state.base)) {
          | None => `NotFound(aliasedName)
          | Some(base) => `Good(Filename.concat(base, rest), rest == "" ? Some(moduleName) : None);
          };
        }
      }
    };

    switch initial {
    | `Builtin(name) => Builtin(name)
    | `NotFound(name) => NotFound(name)
    | `Good(foundPath, mainOf) => {
      if (Files.isDirectory(foundPath)) {
        if (Files.isFile(Filename.concat(foundPath, "package.json"))) {
          Found(resolvePackageJsonMain(foundPath), mainOf)
        } else if (Files.isFile(Filename.concat(foundPath, "index.js"))) {
          Found(Filename.concat(foundPath, "index.js"), mainOf)
        } else {
          NoDefault(foundPath)
        }
      } else if (Files.isFile(foundPath)) {
        Found(foundPath, mainOf)
      } else if (Files.isFile(foundPath ++ ".js")) {
        Found(foundPath ++ ".js", mainOf)
      } else {
        NotFound(foundPath)
      }
    }
    }
  }
};

Printexc.record_backtrace(true);

let split = (str, string) => Str.split(Str.regexp_string(str), string);


let removeExtraDots = path => Str.global_replace(Str.regexp_string("/./"), "/", path);

[@test [
  (("/a/b/c", "/a/b/d"), "../d"),
  (("/a/b/c", "/a/b/d/e"), "../d/e"),
  (("/a/b/c", "/d/e/f"), "../../../d/e/f"),
  (("/a/b/c", "/a/b/c/d/e"), "./d/e"),
]]
let makeRelative = (base, path) => {
  if (startsWith(path, base)) {
    let baselen = String.length(base);
    let rest = String.sub(path, baselen, String.length(path) - baselen);
    if (rest == "") {
      "./"
    } else if (rest.[0] == '/') {
      "." ++ rest
    } else {
      "./" ++ rest
    }
  } else {
    let rec loop = (bp, pp) => {
      switch (bp, pp) {
      | ([".", ...ra], _) => loop(ra, pp)
      | (_, [".", ...rb]) => loop(bp, rb)
      | ([a, ...ra], [b, ...rb]) when a == b => loop(ra, rb)
      | _ => (bp, pp)
      }
    };
    let (base, path) = loop(split("/", base), split("/", path));
    String.concat("/", (base == [] ? ["."] : List.map((_) => "..", base)) @ path) |> removeExtraDots
  }
};

/* let makeRelative = (a, b) => {
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
}; */


let process = (state, abspath, contents, requires, loop) => {
  let (fixed, _) = requires |> List.fold_left(
    ((contents, offset), {pos, length, text}) => {
      /* print_endline(string_of_int(pos) ++ ":" ++ string_of_int(offset)); */
      let pos = offset + pos;
      /* print_endline(string_of_int(pos)); */
      let pre = String.sub(contents, 0, pos);
      let post = sliceToEnd(contents, pos + length);
      let newText = switch (resolve(state, abspath, text)) {
      | NotFound(text) => failwith("Unable to find " ++ text ++ " from " ++ abspath)
      | NoDefault(text) => failwith("No discernable default file for directory: " ++ text)
      | Builtin(name) => "alert('Shimmed node builtin " ++ name ++ "')"
      | Found(childPath, mainOf) => {
        if (state.mode == ExternalEverything && text.[0] != '.') {
          "window.packRequire(\"" ++ String.escaped(makeRelative(state.base, childPath) |> Str.global_replace(Str.regexp_string("/./"), "/")) ++ "\")"
        } else {
          let childId = loop(state, text.[0] == '.', mainOf, childPath);
          "require(" ++ string_of_int(childId) ++ ")";
        };
      }
      };
      (
        pre ++ newText ++ post,
        offset + (String.length(newText) - length)
      )
    },
    (contents, 0)
  );
  fixed
};
