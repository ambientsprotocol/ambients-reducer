type t =
  | Int(int)
  | String(string)
  | None;

exception NotString;
exception NotInt;

let toString (value) = {
  switch value {
  | String(x) => x
  | _ => raise(NotString)
  };
};

let toInt (value) = {
  switch value {
  | Int(x) => x
  | _ => raise(NotInt)
  };
};
