open Ambient;
type value = Value.t;

let parse (ambient): value = {
  switch (getName(ambient)) {
  | "concat" => {
    let left = getName(firstChild(firstChild(findFirst("left", ambient))));
    let right = getName(firstChild(firstChild(findFirst("right", ambient))));
    String(left ++ right);
  }
  | _ => String(getName(ambient))
  };
};
