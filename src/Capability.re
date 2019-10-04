type name = Name.t;

type capability =
  | Create
  | In(name, capability)
  | In_(name, capability)
  | Out(name, capability)
  | Out_(name, capability)
  | Open(name, capability)
  | Open_(capability)
  | None;

let isEqual (a, b) = a == b

let getNext (capability) = {
  switch capability {
  | Create => None
  | In(_, x) => x
  | In_(_, x) => x
  | Out(_, x) => x
  | Out_(_, x) => x
  | Open(_, x) => x
  | Open_(x) => x
  | _ => None
  };
};

let toString (capability: capability) = {
  switch capability {
  | Create => "create"
  | In(x, _) => "in " ++ x
  | In_(x, _) => "in_ " ++ x
  | Out(x, _) => "out " ++ x
  | Out_(x, _) => "out_ " ++ x
  | Open(x, _) => "open " ++ x
  | Open_(_) => "open_"
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
