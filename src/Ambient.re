type name = Name.t;
type capability = Capability.t;

type option('a) =
  | Some('a)
  | None;

type op('a) = {
  parent: 'a, 
  source: 'a, 
  target: 'a, 
  op: capability
};

type ambient =
  | None
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

let toString (ambient: ambient): string = {
  switch ambient {
  | None => ""
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

let opToString(op: option(op(ambient))) = {
  switch op {
  | Some(a) => "op: " ++ Capability.toString(a.op) ++ ", parent: [" ++ getName(a.parent) ++ "], source: [" ++ getName(a.source) ++ "], target: [" ++ getName(a.target) ++ "]"
  | None => ""
  };
};

let findChild (name: name, parent: ambient) = {
  List.find((a) => switch a {
  | Ambient(n, _, _, _) => n == name
  }, getChildren(parent));
};

let _updatedWith = (ambient: ambient, list) => {
  let takeLeftIfEqual = (a, b) => getName(a) == getName(b) ? a : b;
  List.map(takeLeftIfEqual(ambient), list);
};

let _updatedWithout = (ambient: ambient, list) => {
  List.filter((e) => getName(e) !== getName(ambient), list)
};

let canEnter (a, b) = {
  let nextCocap = getNextAction(b);
  let nextCap = getNextAction(a);
  /* print_string("[" ++ getName(a) ++ "] cap: " ++ Capability.toString(nextCap) ++ "\n"); */
  /* print_string("[" ++ getName(b) ++ "] cocap: " ++ Capability.toString(nextCocap) ++ "\n"); */
  switch (nextCap, nextCocap) {
  | (In(c), In_(d)) => c == getName(b) && d == getName(a)
  | _ => false
  };
};

let canOpen (a, b) = {
  let nextCocap = getNextAction(b);
  let nextCap = getNextAction(a);
  switch (nextCap, nextCocap) {
  | (Open(c), Open_) => c == getName(b)
  | _ => false
  };
};

let rec canReduce (ambient) = {
  let hasTransitions (a) = List.length(getTransitions(a)) > 0;
  switch (hasTransitions(ambient)) {
  | true => true
  | false => {
    List.fold_left((res, acc: ambient) => {
      res || (hasTransitions(acc) ? true : canReduce(acc));
    }, false, getChildren(ambient));
  };
  };
};

let createTranstion (ambient, parent): option(op(ambient)) = {
  let nextCap = getNextAction(ambient);
  /* print_string("reduce: [" ++ getName(ambient) ++ "]\n") */
  /* print_string("parent:\n" ++ toString(parent)) */
  /* print_string("next cap: " ++ Capability.toString(nextCap) ++ "\n") */
  let step = switch nextCap {
  | In(name) => {
    /* print_string(">> in " ++ name ++ "\n"); */
    switch (findChild(name, parent)) {
    | exception Not_found => None
    | target => {
      /* print_string("found target: " ++ getName(target) ++ "\n"); */
      switch (canEnter(ambient, target)) {
      | true => Some({op: In(name), source: ambient, target, parent})
      | false => None
      }
    };
    };
  }
  | Open(name) => {
    /* print_string(">> open " ++ name ++ "\n") */
    switch (findChild(name, ambient)) {
    | exception Not_found => None
    | target => {
      /* print_string("found target: " ++ name ++ "\n"); */
      switch (canOpen(ambient, target)) {
      | true => Some({op: Open(name), source: ambient, target, parent})
      | false => None
      }
    };
    };
  }
  | _ => None
  };
  /* print_string("reduced: [" ++ getName(ambient) ++ "]\n"); */
  /* print_string("found ops: " ++ opToString(step) ++ "\n"); */
  step;
};

let rec createTransitionTreeRecursive (ambient: ambient): ambient = {
  /* print_string(">> getting children for [" ++ getName(ambient) ++ "]\n") */
  let children = getChildren(ambient);
  List.fold_left((res, acc: ambient) => {
    /* print_string("[" ++ getName(ambient) ++ "] child: " ++ getName(acc) ++ "\n") */
    let child = createTransitionTreeRecursive(acc);
    let updatedChildren = _updatedWith(child, getChildren(res));
    let operation = createTranstion(acc, ambient);
    /* print_string("--- [" ++ getName(child) ++ "]\n") */
    let result = switch operation {
    | Some(a) => Ambient(getName(ambient), updatedChildren, getCapabilities(ambient), [a, ...getTransitions(ambient)])
    | None => Ambient(getName(ambient), updatedChildren, getCapabilities(ambient), getTransitions(ambient))
    };
    result;
  }, ambient, children);
};

let updateChild (child, parent) = {
  /* print_string(":: update child ::\nparent:\n" ++ toString(parent) ++ "child:\n" ++ toString(child)); */
  let children = _updatedWith(child, getChildren(parent));
  let updated = Ambient(getName(parent), children, getCapabilities(parent), getTransitions(parent));
  /* print_string(":: updated child ::\nparent\n" ++ toString(updated)); */
  updated;
};

let removeChild (child, parent) = {
  /* print_string(":: remove child ::\nparent:\n" ++ toString(parent) ++ "child:\n" ++ toString(child)); */
  let children = _updatedWithout(child, getChildren(parent));
  let updated = Ambient(getName(parent), children, getCapabilities(parent), getTransitions(parent));
  /* print_string(":: removed child:\nparent:\n" ++ toString(updated)); */
  updated;
};

let addChildren (children, parent) = {
  /* print_string(":: add children ::\nparent:\n" ++ toString(parent) ++ "child:\n"); */
  /* print_string(List.fold_left((s, a) => s ++ toString(a), "", children)); */
  let children = List.concat([getChildren(parent), children]);
  let updated = Ambient(getName(parent), children, getCapabilities(parent), getTransitions(parent));
  /* print_string(":: added children ::\nparent:\n" ++ toString(updated)); */
  updated
};

let addChild (child, parent) = {
  addChildren([child], parent);
};

let inheritChildren (b, a) = {
  /* print_string(":: inherit children ::\n"); */
  addChildren(getChildren(b), a);
};

let inheritCapabilities (b, a) = {
  /* print_string(":: inherit capabilities ::\nparent:\n" ++ toString(a) ++ "child:\n" ++ toString(b)); */
  let capabilities = List.concat([getCapabilities(a), getCapabilities(b)])
  let updated = Ambient(getName(a), getChildren(a), capabilities, getTransitions(a));
  /* print_string(":: inherited capabilities ::\nparent:\n" ++ toString(updated)); */
  updated;
};

let consumeCapabilities (a, b) = {
  /* print_string(":: consume capabilities ::\n"); */
  /* print_string("[" ++ getName(a) ++ "] " ++ Capability.toString(getNextAction(a)) ++ "\n"); */
  /* print_string("[" ++ getName(b) ++ "] " ++ Capability.toString(getNextAction(b)) ++ "\n"); */
  let source = Ambient(getName(a), getChildren(a), List.tl(getCapabilities(a)), getTransitions(a));
  let target = Ambient(getName(b), getChildren(b), List.tl(getCapabilities(b)), getTransitions(b));
  /* print_string(":: consumed capabilities ::\n"); */
  (source, target);
};

let enter (a, b, parent): ambient = {
  /* print_string(":: enter\n"); */
  let (source, target) = consumeCapabilities(a, b);
  let updated = addChild(source, target);
  parent |> removeChild(source) |> updateChild(updated);
}

let open_ (a, b, parent): ambient = {
  /* print_string(":: open\n") */
  let (source, target) = consumeCapabilities(a, b);
  let updated = source 
    |> removeChild(target)
    |> inheritChildren(target)
    |> inheritCapabilities(target);  
  parent |> updateChild(updated);
}

let processTransition (step: op(ambient), parent) = {
  let {source, target, op} = step;
  switch op {
  | In(_) => enter(source, target, parent)
  | Open(_) => open_(source, target, parent)
  | _ => parent
  };
}

let rec transitionRecursive (ambient): ambient = {
  /* print_string(">> processing children of [" ++ getName(ambient) ++ "]\n") */
  let children = getChildren(ambient);
    /* print_string("start!\n") */
    /* print_string(toString(ambient)) */
    /* print_int(List.length(children)) */
    /* print_newline(); */
  let updatedChildren = List.fold_left((_, acc: ambient) => {
    /* print_string("start2\n") */
    /* print_string("[" ++ getName(ambient) ++ "] child: " ++ getName(acc) ++ "\n") */
    let updated = transitionRecursive(acc);
    /* print_string("<<<--------->>>> " ++ getName(updated) ++ "\n") */
    updateChild(updated, ambient);
  }, ambient, children);
  let transitions = getTransitions(ambient);
    /* print_string("** transitions: " ++ string_of_int(List.length(transitions)) ++ "\n") */
  let u = List.fold_left((res, acc: op(ambient)) => {
    /* print_string("---------> " ++ getName(acc.parent) ++ "\n") */
    processTransition(acc, res);
  }, updatedChildren, transitions);
  let result = Ambient(getName(u), getChildren(u), getCapabilities(u), []);

  /* print_string("stop! [" ++ getName(result) ++ "]\n"); */
  result;
};

let rec reduceFully (ambient) = {
  let transitionTree = createTransitionTreeRecursive(ambient);
  switch (canReduce(transitionTree)) {
  | true => {
    let state = transitionRecursive(transitionTree);
    reduceFully(state);
  }
  | false => ambient
  };
};
