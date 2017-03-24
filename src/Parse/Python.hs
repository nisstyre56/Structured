{-# LANGUAGE OverloadedStrings #-}

module Parse.Python () where

-- Responsible for parsing python import statements

{-
 - import_stmt: import_name | import_from
 - import_name: 'import' dotted_as_names
 - note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
 - import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
 - 'import' ('*' | '(' import_as_names ')' | import_as_names))
 - import_as_name: NAME ['as' NAME]
 - dotted_as_name: dotted_name ['as' NAME]
 - import_as_names: import_as_name (',' import_as_name)* [',']
 - dotted_as_names: dotted_as_name (',' dotted_as_name)*
 - dotted_name: NAME ('.' NAME)*
 -}
