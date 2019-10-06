let string_of_list (list) = List.fold_left((a, b) => a ++ b, "", list);

let concatListReducer (res, acc) = List.concat([res, acc]);

let isLast (i, list) = (i == List.length(list) - 1);

let mapWithDefault (fn, a, default) = {
  switch (fn(a)) {
  | exception _ => default
  | x => x
  };
};

let toOption (a, list) = switch (a(list)) {
  | exception Not_found => None
  | x => Some(x)
};

let optionToValue (a, someFn, none) = switch (a) {
  | Some(x) => someFn(x)
  | None => none
};

let generateId () = Random.int(10000);
