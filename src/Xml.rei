type element_node = {
  name: string,
  attributes: list((string, string)),
  children: list(xml_node),
}
and xml_node =
  | TextNode(string)
  | CDataNode(string)
  | ElementNode(element_node);

let parse: string => Belt.Result.t(xml_node, string);