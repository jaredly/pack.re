
type require = {pos: int, length: int, text: string};

module StrMap = Map.Make(String);

type state = {
  entry: string,
  base: string,
  ids: Hashtbl.t(string, int),
  alias: StrMap.t(string),
  mutable nextId: int,
  mutable modules: list((int, string, string))
};
