type name = Name.t;
type capability = Capability.t;
type transition('a) = Transition.t('a);

/* Ambient has:
   - a name
   - list of children (nested ambients)
   - list of (unused) capabilities
   - list of "transitions", ie. capabilities that can be reduced in the next reduction
*/
type ambient =
  | Ambient(int, name, list(ambient), list(capability), list(transition(ambient)), list(ambient));

let empty (id, name): ambient = {
  Ambient(id, name, [], [], [], []);
};

let create (id, name, children, capabilities, transitions, spawn): ambient = {
  Ambient(id, name, children, capabilities, transitions, spawn);
};

let getId (ambient) = {
  switch ambient {
  | Ambient(id, _, _, _, _, _) => id
  };
};

let getName (ambient) = {
  switch ambient {
  | Ambient(_, name, _, _, _, _) => name
  };
};

let getChildren (ambient) = {
  switch ambient {
  | Ambient(_, _, children, _, _, _) => children
  };
};

let getCapabilities (ambient) = {
  switch ambient {
  | Ambient(_, _, _, capabilities, _, _) => capabilities
  };
};

let getTransitions (ambient) = {
  switch ambient {
  | Ambient(_, _, _, _, transitions, _) => transitions
  };
};

let getSpawns (ambient) = {
  switch ambient {
  | Ambient(_, _, _, _, _, spawns) => spawns
  };
};

let getSpawn (name, ambient) = {
  switch ambient {
  | Ambient(_, _, _, _, _, spawns) => List.hd(spawns)
  };
};

let getNextAction (ambient) = {
  switch ambient {
  | Ambient(_, _, _, caps, _, _) => {
    List.length(caps) > 0 ? List.nth(caps, 0) : None
  }
  };
};

let getNexActions (a, b) = {
  (getNextAction(a), getNextAction(b));
};

let isEqual (a, b) = getId(a) == getId(b)

let _addAll = (ambient: ambient, list) => {
  List.concat([getChildren(ambient), list]);
};

let _update = (ambient: ambient, list) => {
  let takeLeftIfEqual = (a, b) => isEqual(a, b) ? a : b;
  List.map(takeLeftIfEqual(ambient), list);
};

let _without = (ambient: ambient, list) => {
  List.filter((e) => !isEqual(e,  ambient), list)
};

let updateChildren (a, children) = {
  Ambient(getId(a), getName(a), children, getCapabilities(a), getTransitions(a), getSpawns(a));
};

let updateCapabilities (a, capabilities) = {
  Ambient(getId(a), getName(a), getChildren(a), capabilities, getTransitions(a), getSpawns(a));
};

let updateTransitions (a, transitions) = {
  Ambient(getId(a), getName(a), getChildren(a), getCapabilities(a), transitions, getSpawns(a));
};

let updateSpawns (a, spawns) = {
  Ambient(getId(a), getName(a), getChildren(a), getCapabilities(a), getTransitions(a), spawns);
};

let updateChild (child, parent) = {
  _update(child, getChildren(parent)) 
  |> updateChildren(parent);
};

let removeChild (child, parent) = {
  _without(child, getChildren(parent)) 
  |> updateChildren(parent);
};

let addChildren (children: list(ambient), parent) = {
  _addAll(parent, children)
  |> updateChildren(parent);
  
};

let addChild (child, parent) = addChildren([child], parent);

let findAllChildren (name: name, parent: ambient) = {
  List.find_all((a) => name == getName(a), getChildren(parent));
};

let findChild (id: int, parent: ambient) = {
  List.find((a) => id == getId(a), getChildren(parent));
};

let rec toString (ambient): string = {
  let name = getName(ambient);
  let capabilities = getCapabilities(ambient);
  let seq = (i, e) => Capability.toString(e) ++ (Utils.isLast(i, capabilities) ? "" : ".");
  let caps = Utils.string_of_list(List.mapi(seq, capabilities));
  name ++ "[" ++ caps ++ "]\n";
}
and
treeToString (ambient): string = {
  let rec format (ambient, prefix: string, first: bool, last: bool): string = {
    let prefixCurrent = first ? "" : last ? {js|└─ |js} : {js|├─ |js};
    let result = prefix ++ prefixCurrent ++ toString(ambient);
    let children = getChildren(ambient);
    switch (List.length(children)) {
    | 0 => result
    | _ => {
      let prefixChild = prefix ++ (first ? "" : last ? "   " : {js|│  |js});
      let f = (i, e) => format(e, prefixChild, false, Utils.isLast(i, children));
      let mapped = List.mapi(f, children);
      result ++ Utils.string_of_list(mapped);
    };
    };
  };
  format(ambient, "", true, true);
}
