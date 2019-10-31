open Ambient;
type value = Value.t;

let parse (ambient): value = {
  switch (getName(ambient)) {
  | "plus" => {
    let left = int_of_string(getName(firstChild(firstChild(findFirst("left", ambient)))));
    let right = int_of_string(getName(firstChild(firstChild(findFirst("right", ambient)))));
    Int(left + right);
  }
  | _ => Int(int_of_string(getName(ambient)))
  };
};
