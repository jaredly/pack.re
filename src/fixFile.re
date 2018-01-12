
let sliceToEnd = (str, pos) => String.sub(str, pos, String.length(str) - pos);

let rec firstPart = parts => {
  let d = Filename.dirname(parts);
  if (d == ".") {
    parts
  } else {
    firstPart(d)
  }
};

let exists = path => try {Unix.stat(path) |> ignore; true} {
| Unix.Unix_error(Unix.ENOENT, _, _) => false
| Unix.Unix_error(Unix.ENOTDIR, _, _) => false
};

let rec findNodeModule = (needle, base) => {
  if (exists(base) && ReasonCliTools.Files.isDirectory(base)) {
    let full = Filename.concat(base, needle);
    if (ReasonCliTools.Files.isDirectory(full)) {
      Some(full)
    } else {
      let names = ReasonCliTools.Files.readDirectory(base);
      let rec loop = names => {
        switch (names) {
        | [name, ...rest] => {
          let child = name.[0] == '@' ? name : Filename.concat(name, "node_modules");
          switch (findNodeModule(needle, Filename.concat(base, child))) {
          | None => loop(rest)
          | Some(x) => Some(x)
          }
        }
        | [] => None
      }
      };
      loop(names)
    }
  } else {
    None
  }
};

open Types;

let resolve = (state, base, path) => {
  if (String.length(path) == 0) {
    failwith("Invalid require - empty string")
  } else {
    if (path.[0] == '.') {
      Filename.concat(Filename.dirname(base), path)
    } else {
      let moduleName = firstPart(path);
      let base = switch (findNodeModule(state.nodeModulesBase, moduleName)) {
      | None => failwith("Node module not found: " ++ moduleName)
      | Some(x) => x
      };
      Filename.concat(Filename.dirname(base), path);
    }
  }
};

let process = (state, abspath, contents, requires, loop) => {
  let (fixed, _) = requires |> List.fold_left(
    ((contents, offset), {pos, length, text}) => {
      let pos = offset + pos;
      let pre = String.sub(contents, 0, pos);
      let post = sliceToEnd(contents, pos + length);
      let childPath = resolve(state, abspath, text);
      let childId = loop(state, childPath);
      let strId = string_of_int(childId);
      (
        pre ++ strId ++ post,
        offset + (String.length(strId) - length)
      )
    },
    (contents, 0)
  );
  fixed
};
