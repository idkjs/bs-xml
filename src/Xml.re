type element_node = {
  name: string,
  attributes: list((string, string)),
  children: list(xml_node),
}
and xml_node =
  | TextNode(string)
  | CDataNode(string)
  | ElementNode(element_node);

module NodeType = {
  type t = int;

  let element_node = 1;
  let text_node = 3;
  let cdata_section_node = 4;
  /* let processin_instruction_node = 7;
     let comment_node = 8;
     let document_node = 9;
     let document_type_node = 10;
     let document_fragment_node = 11; */
};

type node_list;
type named_node_map('a);
type node;
type attribute;

[@bs.get] external node_list_length: node_list => int = "length";
[@bs.send] external node_list_item: (node_list, int) => node = "item"; /* it should be  => Js.Nullable.t(node) */

[@bs.get] external length: named_node_map('a) => int = "length";
[@bs.send] external item: (named_node_map('a), int) => 'a = "item"; /* it should be => Js.Nullable.t('a) */

[@bs.get] external name: attribute => string = "name";
[@bs.get] external value: attribute => string = "value";

[@bs.get] external nodeType: node => NodeType.t = "nodeType";
[@bs.get] external textContent: node => string = "textContent";
[@bs.get] external nodeName: node => string = "nodeName";
[@bs.send] external querySelector: (node, string) => Js.Nullable.t(node) = "querySelector";

[@bs.get] external attributes: node => named_node_map(attribute) = "attributes";

[@bs.get] external childNodes: node => node_list = "childNodes";

open Belt;

let collectAttrs = node => {
  let attrs: Js.Array.t((string, string)) = [||];
  for (i in 0 to node->attributes->length - 1) {
    attrs |> Js.Array.push((node->attributes->item(i)->name, node->attributes->item(i)->value)) |> ignore;
  };
  List.fromArray(attrs);
};

let rec makeXmlNode: node => option(Result.t(xml_node, string)) =
  node => {
    let collectChildren = node => {
      let oks: Js.Array.t(xml_node) = [||];
      let errs: Js.Array.t(string) = [||];

      for (i in 0 to node->childNodes->node_list_length - 1) {
        let item = node->childNodes->node_list_item(i);
        let maybeNode = makeXmlNode(item);
        switch (maybeNode) {
        | Some(result) =>
          switch (result) {
          | Ok(TextNode(text)) when text->String.trim == "" => () /* do not include empty text nodes, but do include empty CDATA */
          | Ok(xn) => oks |> Js.Array.push(xn) |> ignore
          | Error(err) => errs |> Js.Array.push(err) |> ignore
          };
          ();
        | None => ()
        };
      };

      (oks, errs);
    };

    if (node->nodeType == NodeType.text_node) {
      Some
        (Ok(TextNode(node->textContent->String.trim))); /* trim text nodes */
    } else if (node->nodeType == NodeType.cdata_section_node) {
      Some
        (Ok(CDataNode(node->textContent))); /* do NOT trim CDATA */
    } else if (node->nodeType == NodeType.element_node) {
      let attrs = collectAttrs(node);
      let (oks, errs) = collectChildren(node);

      switch (errs->Array.get(0)) {
      | Some(err) => Some(Error(err))
      | None =>
        let newElement = {name: node->nodeName, attributes: attrs, children: oks->List.fromArray};
        Some(Ok(ElementNode(newElement)));
      };
    } else {
      None;
    };
  };

type dom_parser;
[@bs.send] external parseFromString: (dom_parser, string, string) => node = "parseFromString";

let newDOMParser: unit => dom_parser = [%bs.raw {|
    function() {
        return new DOMParser();
    }
|}];

let parse: string => Result.t(xml_node, string) =
  text => {
    let parser = newDOMParser();
    let doc = parser->parseFromString(text, "text/xml");

    let parserError = doc->querySelector("parsererror");
    if (Js.Nullable.isNullable(parserError)) {
      let rootNodes: Js.Array.t(node) = [||];

      for (i in 0 to doc->childNodes->node_list_length - 1) {
        let item = doc->childNodes->node_list_item(i);
        if (item->nodeType == NodeType.element_node) {
          rootNodes |> Js.Array.push(item) |> ignore;
        };
      };

      if (rootNodes->Js.Array.length > 1) {
        Error(Printf.sprintf("Unexpected number of nodes: %d, 1 is required", rootNodes->Js.Array.length));
      } else {
        switch (rootNodes->Array.get(0)) {
        | Some(root) =>
          switch (makeXmlNode(root)) {
          | Some(result) => result
          | None => Error("No valid root node found")
          }
        | None => Error("No XML nodes found")
        };
      };
    } else {
      let errorMessage =
        switch (doc->querySelector("div")->Js.Nullable.toOption) {
        | Some(div) => div->textContent
        | None => "Failed to parse XML"
        };
      Error(errorMessage);
    };
  };
