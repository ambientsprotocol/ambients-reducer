open Ambient;

let inheritChildren (b, a) = {
  addChildren(getChildren(b), a);
};

let inheritCapabilities (b, a) = {
  List.concat([getCapabilities(a), getCapabilities(b)]) 
  |> updateCapabilities(a);
};

let consumeCapability (ambient) = {
  updateCapabilities(ambient, List.tl(getCapabilities(ambient)));
};

let consumeCapabilities (a, b) = {
  (consumeCapability(a), consumeCapability(b));
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
  let reducer (res, child: ambient) = updateChild(applyTransitionsRecursive(child), res);
  let updated1 = List.fold_left(reducer, ambient, getChildren(ambient));
  let updated2 = List.fold_left(applyTransition, updated1, getTransitions(ambient)) 
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
let rec reduceFully (ambient) = {
  let transitionTree = AmbientTransitionTree.createRecursive(ambient);
  switch (canReduce(transitionTree)) {
  | true => applyTransitionsRecursive(transitionTree) |> reduceFully
  | false => ambient
  };
};

let rec reduceFullyDebug (index, ambient) = {
  let transitionTree = AmbientTransitionTree.createRecursive(ambient);
  switch (canReduce(transitionTree)) {
  | true => {
      index == 0 
        ? print_string("initial state:\n") 
        : print_string("step " ++ string_of_int(index) ++ ":\n");
      print_string(treeToString(ambient));
      applyTransitionsRecursive(transitionTree) |> reduceFullyDebug(index + 1)
    }
  | false => {
      print_string("final state:\n") 
      print_string(treeToString(ambient));
      ambient
    }
  };
};
