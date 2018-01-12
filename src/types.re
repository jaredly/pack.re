
type require = {pos: int, length: int, text: string};

type state = {
  entry: string,
  nodeModulesBase: string,
  ids: Hashtbl.t(string, int),
  mutable nextId: int,
  mutable modules: list((int, string))
};
