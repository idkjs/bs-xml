type decoder('a) = Dom.element => 'a;

exception DecodeError(string);

// text, name and namespace also fit decoder('a) signature


[@bs.get] external text: Dom.element => string = "textContent";

[@bs.get] [@bs.return nullable]
external namespace: Dom.element => option(string) = "namespaceURI";

[@bs.get] external name: Dom.element => string = "localName";

let ok: 'a => decoder('a);
let error: string => decoder('a);

// require* functions throws DecodeError, can be used in custom decoders

let requireSome: option('a) => 'a;
let requireName: (Dom.element, string) => Dom.element;
let requireNamespace: (Dom.element, option(string)) => Dom.element;

let attribute: (string, ~namespace: option(string)=?) => decoder(string);

let withName: (string, decoder('a)) => decoder('a);
let withNamespace: (option(string), decoder('a)) => decoder('a);
let optional: decoder('a) => decoder(option('a));

// select* functions are used in 'child' and 'children' decoders

let selectAny: Dom.element => bool;
let select: (string, ~namespace: option(string)=?, Dom.element) => bool;

let child: (Dom.element => bool, decoder('a)) => decoder('a);
let children: (Dom.element => bool, decoder('a)) => decoder(array('a));
let map: (decoder('a), 'a => 'b) => decoder('b);
let mapOptional: (decoder(option('a)), 'a => 'b) => decoder(option('b));
let andThen: (decoder('a), 'a => decoder('b)) => decoder('b);
let either: (decoder('a), decoder('a)) => decoder('a);
let withDefault: (Dom.element => 'a, 'a) => decoder('a);
let oneOf: list(decoder('a)) => decoder('a);

let float: string => float;
let int: string => int;
let date: string => Js.Date.t;
let bool: string => bool;

let childElements: decoder(array(Dom.element));