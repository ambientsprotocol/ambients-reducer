type name = Name.t;

type t =
  | In(name)
  | In_(name)
  | Out(name)
  | Out_(name)
  | Open(name)
  | Open_
  | None;

let toString (capability: t) = {
  switch capability {
  | In(x) => "in " ++ x
  | In_(x) => "in_ " ++ x
  | Out(x) => "out " ++ x
  | Out_(x) => "out_ " ++ x
  | Open(x) => "open " ++ x
  | Open_ => "open_"
  | None => ""
  };
};
