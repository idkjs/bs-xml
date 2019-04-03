open Belt;

type t;

[@bs.send]
external parseFromString: (t, string, string) => Dom.document =
  "parseFromString";

let parse = (self, text, type_): Result.t(Dom.element, string) => {
  let doc = self->parseFromString(text, type_);

  switch (doc->Xml_Document.querySelector("parsererror")) {
  | Some(errorElement) => Error(errorElement->Xml_Element.textContent)
  | None =>
    let nodes =
      doc->Xml_Document.childNodes->Xml_NodeList.asArrayLike->Js.Array.from;
    switch (nodes->Array.keepMap(Xml_Node.asElement)->Array.get(0)) {
    | Some(root) => Ok(root)
    | None => Error("root element missing")
    };
  };
};

let parseXml = (self, text) => parse(self, text, "text/xml");

let parseHtml = (self, text) => parse(self, text, "text/html");

[@bs.new] external make: unit => t = "DOMParser";