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
  Utils.toOption(List.find(hasMatchingCocap), getCapabilities(target));
};

let _createTransitions (ambient, parent): list(transition(ambient)) = {
  let processCapability (name, source, parent, capability) = {
    let processChild (target) = switch(findMatchingCocap(capability, source, target)) {
      | Some(x) => Some(Transition.create(source, target, capability, x))
      | None => None
    };
    List.map(processChild, findAllChildren(name, parent));
  };
  let findPossibleTransitions (a, p, capability) = {
    switch capability {
    | In(name, x) => processCapability(name, a, p, In(name, x))
    | Out_(name, x) => processCapability(name, a, ambient, Out_(name, x))
    | Open(name, x) => processCapability(name, a, ambient, Open(name, x))
    | Create => [Some(Transition.create(a, p, Create, Create))]
    | _ => []
    };
  };
  let reduceToValue (r, a) = Utils.optionToValue(a, x => List.append([x], r), r);
  getCapabilities(ambient)
  |> List.map(findPossibleTransitions(ambient, parent))
  |> List.fold_left(Utils.concatListReducer, [])
  |> List.fold_left(reduceToValue, []);
};

let _create (parent, ambient, initial) = {
  let reduceTransitions (res, acc) = updateTransitions(res, [acc, ...getTransitions(res)]);
  let transitions = _createTransitions(ambient, parent);
  List.fold_left(reduceTransitions, initial, transitions);
};

let rec _createRecursive (ambient: ambient): ambient = {
  let create (res, acc: ambient) = {
    let child = _createRecursive(acc);
    let updated = _update(child, getChildren(res)) |> updateChildren(res);
    _create(res, acc, updated);
  };
  List.fold_left(create, ambient, getChildren(ambient));
};

let createRecursive (ambient) = {
  let child = _createRecursive(ambient);
  let updated = _update(child, getChildren(child)) |> updateChildren(child);
  _create(ambient, ambient, updated);
};
