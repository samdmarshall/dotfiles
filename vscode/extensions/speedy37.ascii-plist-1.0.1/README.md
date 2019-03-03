# ASCII Property List

Add ASCII property list language support.

Language reference: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/PropertyLists/OldStylePlists/OldStylePLists.html.

Also add support for an extended version of ASCII property list that supports:

 - comments (`/* */` and `//` styles)
 - array of naturals (`[0, 2]`)
 - custom classes (`@myclassname{ myproperty = "value" ; }`)
 - constants (`YES`, `NO`)
