open Jest;
open Expect;
open Belt;
open Xml;

let getElementNodeExn = (node: xml_node) =>
  switch (node) {
  | ElementNode(en) => en
  | _ => failwith("Not an element node")
  };

let isElementNode = (node: xml_node) =>
  switch (node) {
  | ElementNode(_) => true
  | _ => false
  };

let expectToEqual: ('a, 'a) => unit = [%raw
  {|
    function(left, right) {
        expect(left).toEqual(right);
    }
|}
];

let readFile: string => string = [%raw
  {|
    function(path) {
        return require("fs").readFileSync(path, { encoding: "utf8"});
    }
  |}
];

describe("Invalid sample", () =>
  test("Cannot be parsed", () => {
    let str = "<xml>";
    let xml = Xml.parse(str);
    expect(xml->Result.isError) |> toEqual(true);
  })
);

describe("Sample 1", () => {
  let str = {|
        <root attr1="value1">
            <item1 />
            <item2 attr2="value2" attr3="value3">
                Str 1
            </item2>
            <![CDATA[ Str 2 ]]>
        </root>
    |};
  let xml = Xml.parse(str);

  let expected =
    ElementNode({
      name: "root",
      attributes: [("attr1", "value1")],
      children: [
        ElementNode({name: "item1", attributes: [], children: []}),
        ElementNode({
          name: "item2",
          attributes: [("attr2", "value2"), ("attr3", "value3")],
          children: [TextNode("Str 1")],
        }),
        CDataNode(" Str 2 "),
      ],
    });

  test("Can be parsed", () =>
    expect(xml |> Result.getExn) |> toEqual(expected)
  );
});

describe("RSS sample", () => {
  let str = readFile("samples/ww.xml");
  let xml = Xml.parse(str);

  test("Can be parsed", () =>
    expect(xml |> Result.isOk) |> toBe(true)
  );

  test("Contains 10 items", () => {
    let rss = xml->Result.getExn;
    let rss = rss->getElementNodeExn;

    expectToEqual(rss.name, "rss");

    let channel = rss.children->List.getExn(0)->getElementNodeExn;
    expectToEqual(channel.name, "channel");

    let itemsLength =
      channel.children
      ->List.reduce(0, (c, x) =>
          switch (x) {
          | ElementNode({name: "item"}) => c + 1
          | _ => c
          }
        );

    expect(itemsLength) |> toEqual(10);
  });
});

describe("Readme example", () =>
  test("Can be parsed", () => {
    let result = Xml.parse("<root><item>Item 1</item></root>");
    let item =
      switch (result) {
      | Result.Ok(
          ElementNode({name: "root", children: [ElementNode({name: "item", children: [TextNode(item1)]})]}),
        ) =>
        Some(item1)
      | _ => None
      };
    expect(item) |> toEqual(Some("Item 1"));
  })
);
