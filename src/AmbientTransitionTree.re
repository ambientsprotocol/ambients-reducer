open Ambient;
open Capability;

type transition('a) = Transition.t('a);

let findMatchingCocap (capability, ambient, target): option(capability) = {
  let hasMatchingCocap (e) = switch (capability, e) {
  | (In(c, _), In_(d, _)) => c == getName(target) && d == getName(ambient)
  | (Out_(c, _), Out(d, _)) => c == getName(target) && d == getName(ambient)
  | (Open(c, _), Open_(_)) => c == getName(target)
  | _ => false
  };
  switch (List.find(hasMatchingCocap, getCapabilities(target))) {
  | exception Not_found => None
  | x => Some(x)
  };
};

let _createTransitions (ambient, parent): list(transition(ambient)) = {
  let _filterTransitionsReducer (r, a): list(transition(ambient)) = {
    switch a {
    | Some(t) => List.concat([r, [t]])
    | None => r
    };
  };
  let processCapability (name, source, parent, capability) = {
    let processChild (target) = {
      let cocapability: option(capability) = findMatchingCocap(capability, source, target);
      switch (cocapability) {
      | Some(x) => Some({Transition.source: source, target, capability, cocapability: x})
      | None => None
      };
    };
    List.map(processChild, findAllChildren(name, parent));
  };
  let findPossibleTransitions (a, p, capability) = {
    switch capability {
    | In(name, x) => processCapability(name, a, p, In(name, x))
    | Out_(name, x) => processCapability(name, a, ambient, Out_(name, x))
    | Open(name, x) => processCapability(name, a, ambient, Open(name, x))
    | Create => [Some({Transition.source: a, target: p, capability: Create, cocapability: Create})]
    | _ => []
    };
  };
  getCapabilities(ambient)
  |> List.map(findPossibleTransitions(ambient, parent))
  |> List.fold_left((res, acc) => List.concat([res, acc]), [])
  |> List.fold_left(_filterTransitionsReducer, []);
};

let rec _createRecursive (ambient: ambient): ambient = {
  let create (res, acc: ambient) = {
    let child = _createRecursive(acc);
    let updated = _update(child, getChildren(res)) |> updateChildren(res);
    let reduce (parent, ambient) = {
      let transitions = _createTransitions(ambient, parent);
      List.fold_left((res, acc) => {
        updateTransitions(res, [acc, ...getTransitions(res)]) 
      }, updated, transitions);
    };
    reduce(res, acc);
  };
  List.fold_left(create, ambient, getChildren(ambient));
};

let createRecursive (ambient) = {
  let child = _createRecursive(ambient);
  let updated = _update(child, getChildren(child)) |> updateChildren(child);
  let reduce (parent, ambient) = {
    let transitions = _createTransitions(ambient, parent);
    List.fold_left((res, acc) => {
      updateTransitions(res, [acc, ...getTransitions(res)]) 
    }, updated, transitions);
  };
  reduce(ambient, ambient);
};