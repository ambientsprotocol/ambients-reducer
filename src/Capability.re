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

let fromString (s: string): t = {
  switch (Array.to_list(Js.String.split(" ", s))) {
  | ["in", x] => In(x)
  | ["in_", x] => In_(x)
  | ["out", x] => Out(x)
  | ["out_", x] => Out_(x)
  | ["open", x] => Open(x)
  | ["open_"] => Open_
  | _ => None
  };
};
