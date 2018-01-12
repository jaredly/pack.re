
open Types;

let cat = (filename) => {
  let ic = open_in_bin(filename);
  let len = in_channel_length(ic);
  let buf = Buffer.create(len);
  Buffer.add_channel(buf, ic, len);
  let content = Buffer.contents(buf);
  close_in(ic);
  content
};

let rec process = (state, abspath) => {
  if (Hashtbl.mem(state.ids, abspath)) {
    Hashtbl.find(state.ids, abspath);
  } else {
    state.nextId = state.nextId + 1;
    let id = state.nextId;
    Hashtbl.add(state.ids, abspath, id);
    let contents = cat(abspath);
    let requires = findRequires(contents);
    let fixed = FixFile.process(state, abspath, contents, requires, process);
    state.modules = [(id, fixed), ...state.modules];
    id
  }
};

