open Jest;
open Expect;

test("TODO", () => {
  expect(true) |> toEqual(true)
});
/* open Jest;
open Expect;
open Ambient;

type capability = Capability.t;

describe("From JSON", () => {
  test("001", () => {
    let json = {| { "id": "a", "type": "Noop" } |}
    let amb: ambient = Deserializer.fromJSON(json);
    let expected = Ambient(0, "a", [], [], [])
    expect(amb) |> toEqual(expected)
  });

  test("002", () => {
    let json = {| {
      "children": [
        {
          "children": [{
            "id": "b",
            "type": "Noop"
          }],
          "id": "a",
          "type": "Ambient"
        },
        {
          "id": "c",
          "type": "Noop"
        }
      ],
      "type": "Parallel"
    } |}
    let amb: ambient = Deserializer.fromJSON(json);
    let expected = Parallel([
      Ambient("a", [Ambient("b", [], [], [])], [], []),
      Ambient("c", [], [], [])
    ])
    expect(amb) |> toEqual(expected)
  });

  test("003", () => {
    let json = {|
    {
  "children": [
    {
      "children": [{
        "id": "b",
        "type": "In"
      }],
      "id": "a",
      "type": "Ambient"
    },
    {
      "children": [{
        "id": "a",
        "type": "In_"
      }],
      "id": "b",
      "type": "Ambient"
    }
  ],
  "type": "Parallel"
}

    |};
    let amb: ambient = Deserializer.fromJSON(json);
    let expected =
      Parallel([
        Ambient("a", [], [In("b")], []),
        Ambient("b", [], [In_("a")], [])
      ])
    expect(amb) |> toEqual(expected)
  });
});
      // (* read [package.json] file *) */
