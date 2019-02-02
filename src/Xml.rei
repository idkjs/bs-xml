module DomParser: {
  type t;

  let parseFromString: (t, string, string) => Dom.document;

  let parse: (t, string, string) => Belt.Result.t(Dom.element, string);

  let parseXml: (t, string) => Belt.Result.t(Dom.element, string);

  let parseHtml: (t, string) => Belt.Result.t(Dom.element, string);

  let make: unit => t;
};

module Decode: {
  exception DecodeError(string);

  let withName: (Dom.element, string) => Dom.element;
  let withNamespace: (Dom.element, option(string)) => Dom.element;

  type decoder('a) = Dom.element => 'a;

  let attribute: (string, ~namespace: option(string)=?) => decoder(string);

  let name: decoder(string);
  let namespace: decoder(option(string));

  let text: decoder(string);

  let optional: decoder('a) => decoder(option('a));

  let child:
    (string, ~namespace: option(string)=?, decoder('a)) => decoder('a);

  let children:
    (string, ~namespace: option(string)=?, decoder('a)) =>
    decoder(array('a));

  let float: string => decoder(float);

  let int: string => decoder(int);

  let date: string => decoder(Js.Date.t);

  let bool: string => decoder(bool);

  let map: (decoder('a), 'a => 'b) => decoder('b);

  let andThen: (decoder('a), 'a => decoder('b)) => decoder('b);

  let oneOf: list(decoder('a)) => decoder('a);

  let either: (decoder('a), decoder('a)) => decoder('a);

  let withDefault: (decoder('a), 'a) => decoder('a);
};
