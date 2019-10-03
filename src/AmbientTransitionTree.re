open Ambient;

type transition('a) = Transition.t('a);

type option('a) =
  | Some('a)
  | None;

let canEnter (a, b) = {
  switch (getNexActions(a, b)) {
  | (In(c), In_(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canExit (a, b) = {
  switch (getNexActions(a, b)) {
  | (Out_(c), Out(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canOpen (a, b) = {
  switch (getNexActions(a, b)) {
  | (Open(c), Open_) => c == getName(b)
  | _ => false
  };
};

let _createTransitions (ambient, parent): list(transition(ambient)) = {
  let _filterTransitionsReducer (r, a): list(transition(ambient)) = {
    switch a {
    | Some(t) => List.concat([r, [t]])
    | None => r
    };
  };
  let create (source, target, checkIfAllowed, capability) = {
    switch (checkIfAllowed(source, target)) {
    | true => Some({Transition.source: source, target, capability})
    | false => None
    };
  };
  let processCapability (name, source, checkFn, transition) = {
    let processChild (target) = create(ambient, target, checkFn, transition)
    List.map(processChild, findAllChildren(name, source));
  };
  let transitions = switch (getNextAction(ambient)) {
  | In(name) => processCapability(name, parent, canEnter, In(name))
  | Out_(name) => processCapability(name, ambient, canExit, Out_(name))
  | Open(name) => processCapability(name, ambient, canOpen, Open(name))
  | _ => [None]
  };
  List.fold_left(_filterTransitionsReducer, [], transitions);
};

let rec _createRecursive (ambient: ambient): ambient = {
  let create (res, acc: ambient) = {
    let child = _createRecursive(acc);
    let updated = _update(child, getChildren(res)) |> updateChildren(res);
    let transitions = _createTransitions(acc, res);
    List.fold_left((res, acc) => {
      updateTransitions(res, [acc, ...getTransitions(res)])
    }, updated, transitions);
  };
  List.fold_left(create, ambient, getChildren(ambient));
};

let createRecursive (ambient) = {
  let res = _createRecursive(ambient);
  let updated = _update(ambient, getChildren(res)) |> updateChildren(res);
  let transitions = _createTransitions(ambient, ambient);
  List.fold_left((res, acc) => {
    updateTransitions(res, [acc, ...getTransitions(res)]) 
    -> updateChildren(getChildren(res))
  }, updated, transitions);
};