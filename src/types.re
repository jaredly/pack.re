
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
  mutable modules: list((int, string, string, bool))
};
