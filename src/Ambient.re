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
  | Ambient(_, _, caps, _) => {
    List.length(caps) > 0 ? List.nth(caps, 0) : None
  }
  };
};

let getNexActions (a, b) = {
  (getNextAction(a), getNextAction(b));
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

let toString (ambient): string = {
  let name = getName(ambient);
  let capabilities = getCapabilities(ambient);
  let caps = Utils.string_of_list(List.mapi((i, e) => {
    Capability.toString(e) ++ (Utils.isLast(i, capabilities) ? "" : ".");
  }, capabilities));
  name ++ "[" ++ caps ++ "]\n";
}

let treeToString (ambient): string = {
  let rec format (ambient, prefix: string, first, last: bool): string = {
    let prefixCurrent = first ? "" : last ? {js|└─ |js} : {js|├─ |js};
    let result = prefix ++ prefixCurrent ++ toString(ambient);
    let children = getChildren(ambient);
    switch (List.length(children)) {
    | 0 => result
    | _ => {
      let prefixChild = prefix ++ (first ? "" : last ? "   " : {js|"│  "|js});
      let mapped = List.mapi((i, e) => {
        format(e, prefixChild, false, Utils.isLast(i, children))
      }, children);
      result ++ Utils.string_of_list(mapped);
    };
    };
  };
  format(ambient, "", true, true);
}