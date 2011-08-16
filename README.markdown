This library provides functions for parsing and pretty printing Roman
numerals. Because the notation of Roman numerals has varied through
the centuries this package allows for some customisation using a
configuration that is passed to the conversion functions.

Example:

    >>> toRoman 1729 ∷ String
    "MDCCXXIX"
    >>> fromRoman "MDCCXXIX" ∷ Maybe Integer
    Just 1729
    >>> convertTo simpleRoman 1729 ∷ String
    "MDCCXXVIIII"
    >>> fromRoman "Bla" ∷ Maybe Integer
    Nothing
