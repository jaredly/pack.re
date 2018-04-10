
type require = {pos: int, length: int, text: string};

module StrMap = Map.Make(String);

type mode = Normal | ExternalEverything | JustExternals;

type state = {
  entries: list(string),
  mode,
  base: string,
  ids: Hashtbl.t(string, int),
  alias: StrMap.t(string),
  mutable nextId: int,
  /* id, path, fixedContents, inEntryLand, mainOf - package name that this is the main import of */
  mutable modules: list((int, string, string, bool, option(string)))
};

let init = () => {
  entries: [],
  mode: Normal,
  base: "",
  ids: Hashtbl.create(100),
  alias: StrMap.empty,
  nextId: 0,
  modules: [],
};