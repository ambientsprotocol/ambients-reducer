type name = Name.t;
type capability = Capability.t;
type transition('a) = Transition.t('a);

module Deserializer {
  type node = {
    nType: string,
    id: option(string),
    children: option(list(node)),
  };

  let filterAmbients = (nodes: list(node)) => {
    List.filter((node) => {
      switch node.nType {
        | "Ambient" => true
        | "Noop" => true
        | "Parallel" => true
        | _ => false
        }
    }, nodes)
  }

  let filterCapabilities = (nodes: list(node)) => {
    List.filter((node) => {
      switch node.nType {
        | "In" => true
        | "In_" => true
        | _ => false
      }
    }, nodes)
  }

  let rec node = json => {
    Json.Decode.{
      id: json |> optional(field("id", string)),
      nType: json |> field("type", string),
      children: json |> optional(field("children", list(node)))
    }
  }
}


/* Ambient has:
   - a name
   - list of children (nested ambients)
   - list of (unused) capabilities
   - list of "transitions", ie. capabilities that can be reduced in the next reduction
*/
type ambient =
  | Ambient(name, list(ambient), list(capability), list(transition(ambient)))
  | Parallel(list(ambient));

let empty (name): ambient = {
  Ambient(name, [], [], []);
};

let create (name, children, capabilities, transitions): ambient = {
  Ambient(name, children, capabilities, transitions);
};

exception ID_Required(string);
exception Children_Required(string);
exception Unrecognized(string);

let fromJSON(json) = {
  let ast = json |> Json.parseOrRaise |> Deserializer.node;

  let parseCapability = (node: Deserializer.node) => {
    switch node.nType {
    | "In" => switch node.id {
      | Some(id) => Capability.In(id)
      | None => raise(ID_Required("ID is required"));
      }
    | "In_" => switch node.id {
      | Some(id) => Capability.In_(id)
      | None => raise(ID_Required("ID is required"));
      }
    | _ => raise(Unrecognized("Unrecognized Capability"))
    }
  }

  let rec parseAmbient = (node: Deserializer.node) => {
    switch node.nType {
    | "Ambient" => switch node.id {
      | Some(id) => {
        switch node.children {
        | Some(children) => {
          let childs = Deserializer.filterAmbients(children) |> List.map(parseAmbient);
          let capabilities = Deserializer.filterCapabilities(children) |> List.map(parseCapability);
          let transitions = [];
          Ambient(id, childs, capabilities, transitions)
        }
        | None => Ambient(id, [], [], [])
        }
      }
      | None => raise(ID_Required("ID is required"))
      }
    | "Parallel" => switch node.children {
      | Some(children) => {
        let childs = List.map(parseAmbient, children);
        Parallel(childs)
      }
      | None => raise(Children_Required("Children are required"))
      }
    | "Noop" => switch node.id {
      | Some(id) => Ambient(id, [], [], [])
      | None => raise(ID_Required("ID is required"));
      }
    | _ => raise(Unrecognized("Unrecognized node type"))
    }
  }

  parseAmbient(ast);
}

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

let isEqual (a, b) = getName(a) == getName(b);

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

let findChild (name: name, parent: ambient) = {
  List.find((a) => name == getName(a), getChildren(parent));
};

let toString (ambient): string = {
  let name = getName(ambient);
  let capabilities = getCapabilities(ambient);
  let seq = (i, e) => Capability.toString(e) ++ (Utils.isLast(i, capabilities) ? "" : ".");
  let caps = Utils.string_of_list(List.mapi(seq, capabilities));
  name ++ "[" ++ caps ++ "]\n";
}

let treeToString (ambient): string = {
  let rec format (ambient, prefix: string, first: bool, last: bool): string = {
    let prefixCurrent = first ? "" : last ? {js|└─ |js} : {js|├─ |js};
    let result = prefix ++ prefixCurrent ++ toString(ambient);
    let children = getChildren(ambient);
    switch (List.length(children)) {
    | 0 => result
    | _ => {
      let prefixChild = prefix ++ (first ? "" : last ? "   " : {js|"│  "|js});
      let f = (i, e) => format(e, prefixChild, false, Utils.isLast(i, children));
      let mapped = List.mapi(f, children);
      result ++ Utils.string_of_list(mapped);
    };
    };
  };
  format(ambient, "", true, true);
}
