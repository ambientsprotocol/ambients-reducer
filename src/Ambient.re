type name = Name.t;
type capability = Capability.t;
type transition('a) = Transition.t('a);

type option('a) =
  | Some('a)
  | None;

/* Ambient has a name, list of children (nested ambients), list of (unused) capabilities and a list of "transitions", ie. capabilities that can be reduced in the next reduction */
type ambient =
  | Ambient(name, list(ambient), list(capability), list(transition(ambient)));

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

let getNexttransition (ambient) = {
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

let inheritChildren (b, a) = addChildren(getChildren(b), a);
let inheritCapabilities (b, a) = List.concat([getCapabilities(a), getCapabilities(b)]) |> updateCapabilities(a);
let consumeCapability (ambient) = updateCapabilities(ambient, List.tl(getCapabilities(ambient)));
let consumeCapabilities (a, b) = (consumeCapability(a), consumeCapability(b));

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
  switch (getNexttransition(a), getNexttransition(b)) {
  | (In(c), In_(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canExit (a, b) = {
  switch (getNexttransition(a), getNexttransition(b)) {
  | (Out_(c), Out(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canOpen (a, b) = {
  switch (getNexttransition(a), getNexttransition(b)) {
  | (Open(c), Open_) => c == getName(b)
  | _ => false
  };
};

let enter (a, b, parent): ambient = {
  let (source, target) = consumeCapabilities(a, b);
  /* Add the entering ambient to the target ambient */
  let updated = addChild(source, target);
  /* Remove the entering ambient from its parent */
  parent |> removeChild(source) |> updateChild(updated);
};

let exit (a, b, parent): ambient = {
  let (target, source) = consumeCapabilities(a, b);
  /* Add the exiting ambient to its target ambient */
  let updated = target |> removeChild(source);
  /* Remove the entering ambient from its parent */
  parent |> addChild(source) |> updateChild(updated);
};

let open_ (a, b, parent): ambient = {
  let (source, target) = consumeCapabilities(a, b);
  /* Remove the opening ambient from its parent (source),
  inherit the children and capabilities of the opened ambient and
  update the parent of the ambient that opened the target ambient */
  let updated = source 
    |> removeChild(target)
    |> inheritChildren(target)
    |> inheritCapabilities(target);  
  parent |> updateChild(updated);
};

let createTransition (ambient, parent): option(transition(ambient)) = {
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
  switch (getNexttransition(ambient)) {
  | In(name) => processCapability(name, parent, canEnter, In(name))
  | Out_(name) => processCapability(name, ambient, canExit, Out_(name))
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

let applyTransition (parent, transition: transition(ambient)) = {
  let {Transition.source, target, capability} = transition;
  switch capability {
  | In(_) => enter(source, target, parent)
  | Out_(_) => exit(source, target, parent)
  | Open(_) => open_(source, target, parent)
  | _ => parent
  };
};

let rec applyTransitionsRecursive (ambient): ambient = {
  let updated1 = List.fold_left((res, child: ambient) => {
    updateChild(applyTransitionsRecursive(child), res);
  }, ambient, getChildren(ambient));
  let updated2 = List.fold_left((res, transition: transition(ambient)) => {
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

/* Summary of the reduction logic:
- Traverse the ambient structure (tree) and gather all possible reductions (transitions)
- Check if there are any reductions that can be made, if not, the ambient is fully reduced
- If there are reductions to make, recursively traverse the ambient structure (tree) and apply the reductions (transitions)
*/
let rec reduceFully (ambient) = {
  let transitionTree = createTransitionTreeRecursive(ambient);
  switch (canReduce(transitionTree)) {
  | true => applyTransitionsRecursive(transitionTree) |> reduceFully
  | false => ambient
  };
};
