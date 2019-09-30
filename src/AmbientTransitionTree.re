open Ambient;

type transition('a) = Transition.t('a);
type option('a) =
  | Some('a)
  | None;

let canEnter (a, b) = {
  switch (getNextAction(a), getNextAction(b)) {
  | (In(c), In_(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canExit (a, b) = {
  switch (getNextAction(a), getNextAction(b)) {
  | (Out_(c), Out(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canOpen (a, b) = {
  switch (getNextAction(a), getNextAction(b)) {
  | (Open(c), Open_) => c == getName(b)
  | _ => false
  };
};

let _createTransition (ambient, parent): option(transition(ambient)) = {
  let create (source, target, checkFn, capability) = {
    switch (checkFn(source, target)) {
    | true => Some({Transition.source: source, target, capability})
    | false => None
    };
  };
  let processCapability (name, source, checkFn, transition) = {
    switch (findChild(name, source)) {
    | exception Not_found => None
    | target => create(ambient, target, checkFn, transition)
    };
  };
  switch (getNextAction(ambient)) {
  | In(name) => processCapability(name, parent, canEnter, In(name))
  | Out_(name) => processCapability(name, ambient, canExit, Out_(name))
  | Open(name) => processCapability(name, ambient, canOpen, Open(name))
  | _ => None
  };
};

let rec createRecursive (ambient: ambient): ambient = {
  let children = getChildren(ambient);
  List.fold_left((res, acc: ambient) => {
    let child = createRecursive(acc);
    let updated = _updatedWith(child, getChildren(res)) |> updateChildren(ambient);
    let transition = _createTransition(acc, ambient);
    switch transition {
    | Some(a) => updateTransitions(updated, [a, ...getTransitions(ambient)])
    | None => updated
    };
  }, ambient, children);
};
