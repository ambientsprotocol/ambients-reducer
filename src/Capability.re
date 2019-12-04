type name = Name.t;
type params = list(string);

let string_of_list = Utils.string_of_list;

/* <Name> (<params>, <next>) */
type capability =
  | Create
  /* | In(name, capability)
  | In_(name, capability)
  | Out(name, capability)
  | Out_(name, capability)
  | Open(name, capability)
  | Open_(capability) */
  | Substitute (params, capability)
  | Write (params, capability)
  | Write_ (params, capability)
  | Read (params, capability)
  | Read_ (params, capability)
  | None;

let isEqual (a, b) = a == b

let getNext (capability) = {
  switch capability {
  | Create => None
  | Write (_, x) => x
  | Write_ (_, x) => x
  | Read (_, x) => x
  | Read_ (_, x) => x
  /* | In(_, x) => x
  | In_(_, x) => x
  | Out(_, x) => x
  | Out_(_, x) => x
  | Open(_, x) => x
  | Open_(x) => x */
  | _ => None
  };
};

let toString (capability: capability) = {
  switch capability {
  | Create => "create"
  | Write (x, _) => "write (" ++ string_of_list(x) ++ ")"
  | Write_ (x, _) => "write_ (" ++ string_of_list(x) ++ ")"
  | Read (x, _) => "read (" ++ string_of_list(x) ++ ")"
  | Read_ (x, _) => "read_ (" ++ string_of_list(x) ++ ")"
  /* | In(x, _) => "in " ++ x
  | In_(x, _) => "in_ " ++ x
  | Out(x, _) => "out " ++ x
  | Out_(x, _) => "out_ " ++ x
  | Open(x, _) => "open " ++ x
  | Open_(_) => "open_" */
  | None => "None"
  };
};

let rec treeToString (capability) = {
  switch (getNext(capability)) {
  | None => toString(capability)
  | Create => toString(capability) ++ ".create"
  | x => toString(capability) ++ "." ++ treeToString(x)
  };
};
