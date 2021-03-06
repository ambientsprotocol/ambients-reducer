open Ambient;

let inheritChildren (b, a) = {
  addChildren(getChildren(b), a);
};

let inheritCapabilities (b, a) = {
  List.append(getCapabilities(a), getCapabilities(b))
  |> updateCapabilities(a);
};

let inheritSpawns (b, a) = {
  List.append(getSpawns(a), getSpawns(b))
  |> updateSpawns(a);
};

let inheritAll (b, a) = {
  a |> inheritChildren(b) |> inheritSpawns(b) |> inheritCapabilities(b);
};

let consumeCapability (capability, ambient) = {
  let (matches, rest) = List.partition(Capability.isEqual(capability), getCapabilities(ambient));
  let firstRemoved = List.concat([List.tl(matches), rest]);
  switch (Capability.getNext(capability)) {
  | None => updateCapabilities(ambient, firstRemoved)
  | next => updateCapabilities(ambient, [next, ...firstRemoved])
  };
};

let consumeCapabilities (capability1, capability2, a, b) = {
  (consumeCapability(capability1, a), consumeCapability(capability2, b));
};

let consumeSpawn (a) = {
  Utils.mapWithDefault(List.tl, getSpawns(a), [])
  |> updateSpawns(a);
};

let create (a, parent, capability, callback): ambient = {
  let c = findChild(getId(a), parent)
  let source = consumeCapability(capability, c)
  let target = getNextSpawn(source)
  let createInAmbient (a, b) = consumeSpawn(a) |> inheritAll(b);
  let createInRoot (a, b) = consumeSpawn(a) |> addChild(b)
  let result = switch (getName(target)) {
  | "" => createInAmbient(source, target) -> updateChild(parent)
  | _ => createInRoot(source, target) -> updateChild(parent)
  };
  callback({Event.e: "create", s: getName(source), t: getName(target)});
  result;
};

let enter (a, b, parent, capability, cocapability, callback): ambient = {
  let c = findChild(getId(a), parent)
  let d = findChild(getId(b), parent)
  let (source, target) = consumeCapabilities(capability, cocapability, c, d);
  /* Add the entering ambient to the target ambient */
  let updated = addChild(source, target);
  /* Remove the entering ambient from its parent */
  let result = parent |> removeChild(source) |> updateChild(updated);
  callback({Event.e: "in", s: getName(source), t: getName(target)});
  callback({Event.e: "in_", s: getName(target), t: getName(source)});
  result;
};

let exit (a, b, parent, capability, cocapability, callback): ambient = {
  let c = findChild(getId(a), parent)
  let d = findChild(getId(b), c)
  let (target, source) = consumeCapabilities(capability, cocapability, c, d);
  /* Add the exiting ambient to its target ambient */
  let updated = target |> removeChild(source);
  /* Remove the entering ambient from its parent */
  let result = parent |> addChild(source) |> updateChild(updated);
  callback({Event.e: "exit", s: getName(source), t: getName(source)});
  result;
};

let open_ (a, b, parent, capability, cocapability, callback): ambient = {
  /* Find the child from the parent, 
  if we can't find it assume 'a' is the root ambient */
  let c = Utils.mapWithDefault(findChild(getId(a)), parent, parent);
  let d = findChild(getId(b), c);
  let (source, target) = consumeCapabilities(capability, cocapability, c, d);
  let updateInParent (parent, ambient) = switch (isEqual(ambient, parent)) {
  | true => ambient
  | false => parent |> updateChild(ambient)
  };
  /* Remove the opening ambient from its parent (source),
  inherit the children and capabilities of the opened ambient and
  update the parent of the ambient that opened the target ambient */
  let updated = source 
  |> removeChild(target)
  |> inheritAll(target)
  |> updateInParent(parent);
  /* "Create" is a special case in that it should be applied as soon 
  as the previous capability has been consumed. TODO: is this wanted? */
  let result = switch (Capability.getNext(capability)) {
  | Create => create(a, updated, Capability.Create, callback);
  | _ => updated
  };
  callback({Event.e: "open", s: getName(source), t: getName(target)});
  callback({Event.e: "open_", s: getName(target), t: ""});
  result;
};

let applyTransition (parent, transition: transition(ambient), callback) = {
  let {Transition.source, target, capability, cocapability} = transition;
  switch capability {
  | Create => create(source, parent, capability, callback)
  | In(_) => enter(source, target, parent, capability, cocapability, callback)
  | Out_(_) => exit(source, target, parent, capability, cocapability, callback)
  | Open(_) => open_(source, target, parent, capability, cocapability, callback)
  | _ => parent
  };
};

let rec applyTransitionsRecursive (ambient, callback): ambient = {
  let reducer (res, child: ambient) = updateChild(applyTransitionsRecursive(child, callback), res);
  let applyWithCb = (res, child) => applyTransition(res, child, callback);
  let updated1 = List.fold_left(reducer, ambient, getChildren(ambient));
  let updated2 = List.fold_left(applyWithCb, updated1, getTransitions(updated1))
  updateTransitions(updated2, []);
};

let rec canReduce (ambient) = {
  let hasTransitions (a) = List.length(getTransitions(a)) > 0;
  let recursiveReducer (res, acc) = res || (hasTransitions(acc) || canReduce(acc));
  hasTransitions(ambient) || List.fold_left(recursiveReducer, false, getChildren(ambient));
};

/* Summary of the reduction logic:
- Traverse the ambient structure (tree) and gather all possible reductions (transitions)
- Check if there are any reductions that can be made, if not, the ambient is fully reduced
- If there are reductions to make, recursively traverse the ambient structure (tree) and apply the reductions (transitions)
*/
let rec reduceFully (callback, ambient) = {
  let transitionTree = AmbientTransitionTree.createRecursive(ambient);
  switch (canReduce(transitionTree)) {
  | true => applyTransitionsRecursive(transitionTree, callback) |> reduceFully(callback)
  | false => ambient
  };
};

let rec reduceFullyDebug (index, callback, ambient) = {
  let transitionTree = AmbientTransitionTree.createRecursive(ambient);
  let canReduceFurther = canReduce(transitionTree);

  let prefix = switch (canReduceFurther && index > 0) {
    | true => "step " ++ string_of_int(index) ++ ":"
    | false => (index == 0 ? "initial state:" : "final state:");
  };
  print_string(prefix ++ "\n" ++ treeToString(transitionTree));

  switch (canReduceFurther) {
  | true => applyTransitionsRecursive(transitionTree, callback) |> reduceFullyDebug(index + 1, callback)
  | false => ambient
  };
};

let _toValue (ambient): Value.t = {
  switch (getName(ambient)) {
  | "string" => String.parse(firstChild(ambient))
  | "int" => Int.parse(firstChild(ambient))
  | _ => None
  };
};

let reduceToValue (callback, ambient) = {
  reduceFully(callback, ambient) |> Ambient.firstChild |> _toValue;
};

let reduceToValueDebug (callback, ambient) = {
  reduceFullyDebug(0, callback, ambient) |> Ambient.firstChild |> _toValue;
};
