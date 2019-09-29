type name = Name.t;
type capability = Capability.t;

type option('a) =
  | Some('a)
  | None;

type op('a) = {
  source: 'a, 
  target: 'a, 
  op: capability
};

type ambient =
  | Ambient(name, list(ambient), list(capability), list(op(ambient)));

let empty (name): ambient = {
  Ambient(name, [], [], []);
};

let create (name, children, capabilities, transitions): ambient = {
  Ambient(name, children, capabilities, transitions);
};

let getName (ambient) = {
  switch ambient {
  | Ambient(name, _, _, _) => name
  };
};

let getChildren (ambient) = {
  switch ambient {
  | Ambient(_, children, _, _) => children
  };
};

let getCapabilities (ambient) = {
  switch ambient {
  | Ambient(_, _, capabilities, _) => capabilities
  };
};

let getTransitions (ambient) = {
  switch ambient {
  | Ambient(_, _, _, transitions) => transitions
  };
};

let getNextAction (ambient) = {
  switch ambient {
  | Ambient(_, _, caps, _) => List.length(caps) > 0 ? List.nth(caps, 0) : None
  };
};

let _updatedWith = (ambient: ambient, list) => {
  let takeLeftIfEqual = (a, b) => getName(a) == getName(b) ? a : b;
  List.map(takeLeftIfEqual(ambient), list);
};

let _updatedWithout = (ambient: ambient, list) => {
  List.filter((e) => getName(e) !== getName(ambient), list)
};

let updateCapabilities (a, capabilities) = {
  Ambient(getName(a), getChildren(a), capabilities, getTransitions(a));
};

let updateChildren (a, children) = {
  Ambient(getName(a), children, getCapabilities(a), getTransitions(a));
};

let updateTransitions (a, transitions) = {
  Ambient(getName(a), getChildren(a), getCapabilities(a), transitions);
};

let updateChild (child, parent) = {
  let children = _updatedWith(child, getChildren(parent));
  updateChildren(parent, children);
};

let removeChild (child, parent) = {
  let children = _updatedWithout(child, getChildren(parent));
  updateChildren(parent, children);
};

let addChildren (children, parent) = {
  let children = List.concat([getChildren(parent), children]);
  updateChildren(parent, children);
};

let addChild (child, parent) = addChildren([child], parent);

let findChild (name: name, parent: ambient) = {
  List.find((a) => switch a {
  | Ambient(n, _, _, _) => n == name
  }, getChildren(parent));
};

let toString (ambient: ambient): string = {
  switch ambient {
  | _ => {
    let name = "[" ++ getName(ambient);
    let caps = getCapabilities(ambient);
    let transitions = getTransitions(ambient);
    let children = List.map((a) => getName(a), getChildren(ambient));
    name ++ "]"
      ++ "\n | nested: " ++ List.fold_left((a, b) => a ++ "[" ++ b ++ "], ", "", children)
      ++ "\n | caps: " ++ List.fold_left((a, b) => a ++ Capability.toString(b) ++ ".", "", caps)
      ++ "\n | transitions: " ++ string_of_int(List.length(transitions))
      ++ "\n"
  };
  };
};

let canEnter (a, b) = {
  let next = getNextAction
  switch (next(a), next(b)) {
  | (In(c), In_(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canOpen (a, b) = {
  let next = getNextAction
  switch (next(a), next(b)) {
  | (Open(c), Open_) => c == getName(b)
  | _ => false
  };
};

/* TODO: canExit */

let inheritChildren (b, a) = addChildren(getChildren(b), a);

let inheritCapabilities (b, a) = {
  List.concat([getCapabilities(a), getCapabilities(b)]) 
  |> updateCapabilities(a);
};

let consumeCapabilities (a, b) = {
  let source = updateCapabilities(a, List.tl(getCapabilities(a)));
  let target = updateCapabilities(b, List.tl(getCapabilities(b)));
  (source, target);
};

let enter (a, b, parent): ambient = {
  let (source, target) = consumeCapabilities(a, b);
  let updated = addChild(source, target);
  parent |> removeChild(source) |> updateChild(updated);
};

/* TODO: exit */

let open_ (a, b, parent): ambient = {
  let (source, target) = consumeCapabilities(a, b);
  let updated = source 
    |> removeChild(target)
    |> inheritChildren(target)
    |> inheritCapabilities(target);  
  parent |> updateChild(updated);
};

let createTransition (ambient, parent): option(op(ambient)) = {
  let transition (source, target, checkFn, op) = {
    switch (checkFn(source, target)) {
    | true => Some({op, source, target})
    | false => None
    };
  };
  let processCapability (name, source, checkFn, op) = {
    switch (findChild(name, source)) {
    | exception Not_found => None
    | target => transition(ambient, target, checkFn, op)
    };
  };
  switch (getNextAction(ambient)) {
  | In(name) => processCapability(name, parent, canEnter, In(name))
  | Open(name) => processCapability(name, ambient, canOpen, Open(name))
  | _ => None
  };
};

let rec createTransitionTreeRecursive (ambient: ambient): ambient = {
  let children = getChildren(ambient);
  List.fold_left((res, acc: ambient) => {
    let child = createTransitionTreeRecursive(acc);
    let updated = _updatedWith(child, getChildren(res)) |> updateChildren(ambient);
    let transition = createTransition(acc, ambient);
    switch transition {
    | Some(a) => updateTransitions(updated, [a, ...getTransitions(ambient)])
    | None => updated
    };
  }, ambient, children);
};

let applyTransition (parent, transition: op(ambient)) = {
  let {source, target, op} = transition;
  switch op {
  | In(_) => enter(source, target, parent)
  | Open(_) => open_(source, target, parent)
  | _ => parent
  };
};

let rec applyTransitionsRecursive (ambient): ambient = {
  let updated1 = List.fold_left((res, child: ambient) => {
    updateChild(applyTransitionsRecursive(child), res);
  }, ambient, getChildren(ambient));
  let updated2 = List.fold_left((res, transition: op(ambient)) => {
    applyTransition(res, transition);
  }, updated1, getTransitions(ambient));
  Ambient(
    getName(updated2), 
    getChildren(updated2), 
    getCapabilities(updated2), 
    []
  );
};

let rec canReduce (ambient) = {
  let hasTransitions (a) = List.length(getTransitions(a)) > 0;
  switch (hasTransitions(ambient)) {
  | true => true
  | false => {
    List.fold_left((res: bool, acc: ambient) => {
      res || (hasTransitions(acc) ? true : canReduce(acc));
    }, false, getChildren(ambient));
  };
  };
};

let rec reduceFully (ambient) = {
  let transitionTree = createTransitionTreeRecursive(ambient);
  switch (canReduce(transitionTree)) {
  | true => applyTransitionsRecursive(transitionTree) |> reduceFully
  | false => ambient
  };
};
