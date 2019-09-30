type name = Name.t;
type capability = Capability.t;
type transition('a) = Transition.t('a);

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
