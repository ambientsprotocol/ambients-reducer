open Ambient;

type node = {
  name: string,
  children: list(node),
  capabilities: list(string)
};

/* exception ID_Required(string);
exception Children_Required(string);
exception Unrecognized(string); */

/* let filterAmbients = (nodes: list(node)) => {
  List.filter((node) => {
    switch node.nType {
      | "Ambient" => true
      | "Noop" => true
      | "Parallel" => true
      | _ => false
      }
  }, nodes);
};

let filterCapabilities = (nodes: list(node)) => {
  List.filter((node) => {
    switch node.nType {
      | "In" => true
      | "In_" => true
      | _ => false
    }
  }, nodes);
}; */

let rec node = json => {
  Json.Decode.{
    name: json |> field("name", string),
    children: json |> field("children", list(node)),
    capabilities: json |> field("capabilities", list(string))
  };
};

let fromJSON(json): ambient = {
  let ast = json |> Json.parseOrRaise |> node;
  /* let parseCapability = (node: node) => {
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
  };
 */
  let rec parseAmbient = (node: node): ambient => {
    let children = List.map(parseAmbient, node.children);
    let capabilities = List.map(Capability.fromString, node.capabilities);
    Ambient.create(Random.int(10000), node.name, children, capabilities, []);
    /*
    switch node.nType {
    | "Ambient" => switch node.id {
      | Some(id) => {
        switch node.children {
        | Some(children) => {
          let childs = filterAmbients(children) |> List.map(parseAmbient);
          let capabilities = filterCapabilities(children) |> List.map(parseCapability);
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
    };
    */
  };

  parseAmbient(ast);
};
