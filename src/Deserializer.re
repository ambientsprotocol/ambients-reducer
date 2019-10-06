open Capability;
type ambient = Ambient.ambient;

type cap = {
  op: string, 
  target: string, 
  next: option(cap)
};

type node = {
  name: string,
  children: list(node),
  capabilities: list(cap),
  create: list(node)
};

let rec toCapability (capability: cap) = {
  let create (e, next) = switch (e.op) {
  | "in" => In(e.target, next)
  | "in_" => In_(e.target, next)
  | "out" => Out(e.target, next)
  | "out_" => Out_(e.target, next)
  | "open" => Open(e.target, next)
  | "open_" => Open_(next)
  | "create" => Create
  | _ => None
  };
  switch capability.next {
  | Some(x) => create(capability, toCapability(x))
  | None => create(capability, None)
  }
};

let rec capability = json => {
  Json.Decode.{
    op: json |> field("op", string),
    target: json |> field("target", string),
    next: json |> optional(field("next", capability))
  };
};

let rec node = json => {
  Json.Decode.{
    name: json |> field("name", string),
    children: json |> field("children", list(node)),
    capabilities: json |> field("capabilities", list(capability)),
    create: json |> field("create", list(node))
  };
};

let fromJSON(json): ambient = {
  let ast = json |> Json.parseOrRaise |> node;
  let rec parseAmbient = (node: node): ambient => {
    let children = List.map(parseAmbient, node.children);
    let capabilities = List.map(toCapability, node.capabilities);
    let create = List.map(parseAmbient, node.create);
    Ambient.create(Utils.generateId(), node.name, children, capabilities, [], create);
  };

  parseAmbient(ast);
};
