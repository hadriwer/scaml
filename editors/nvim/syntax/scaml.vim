" SCaml syntax, ported from editors/vscode/syntaxes/scaml.tmLanguage.json.

if exists("b:current_syntax")
  finish
endif

" `p'` is one identifier, as in OCaml.
syn iskeyword @,48-57,_,'

" Keywords
syn keyword scamlConditional if then else match
syn keyword scamlException   try with exception
syn keyword scamlStorage     fn op let type trait impl
syn keyword scamlOf          of
syn keyword scamlBoolean     true false
syn keyword scamlBuiltinType int float string char bool unit list array option range

" Identifiers (lowest priority: defined first)
syn match scamlConstructor "\<[A-Z][A-Za-z0-9_']*"
syn match scamlModule      "\<[A-Z][A-Za-z0-9_']*\ze\.[A-Za-z_]"
" `p.name`, but not `Array.make` (a module).
syn match scamlField       "\%(\<[A-Z][A-Za-z0-9_']*\.\)\@<!\%([A-Za-z0-9_')\]]\.\)\@2<=[a-z_][A-Za-z0-9_']*"

" Operators
syn match scamlOperator    "[-+*/<>:%.|=&!@]\+"
syn match scamlArrow       "->"
syn match scamlArrayDelim  "\[|\||\]"
syn match scamlLambda      "\^"
syn match scamlOpSection   "(\s*[-+*/<>:%.|=&!@]\+\s*)"

" Constants
syn match scamlUnit        "(\s*)"
syn match scamlEmpty       "\[|\s*|\]"
syn match scamlEmpty       "\[\s*\]"

" Numbers: `1.` and `1.5` are floats, but `0..2` is the int 0 then `..`.
syn match scamlInteger     "\%([A-Za-z0-9_']\)\@1<!\d\+"
syn match scamlFloat       "\%([A-Za-z0-9_']\)\@1<!\d\+\.\%(\.\)\@!\d*"

" Declarations
syn match scamlFnName      "\%(\<fn\s\+\)\@<=[a-z_][A-Za-z0-9_']*"
syn match scamlFnName      "\%(\<op\s\+\)\@<=[-+*/<>:%.|=&!@^]\+"
syn match scamlTypeName    "\%(\<\%(trait\|impl\)\s\+\)\@<=[a-z_][A-Za-z0-9_']*"
syn match scamlTypeName    "\%(\<type\s\+\%([a-z_][A-Za-z0-9_']*\s\+\)*\)\@<=[a-z_][A-Za-z0-9_']*\ze\s*{"
syn match scamlLetName     "\%(\<let\s\+\)\@<=[a-z_][A-Za-z0-9_']*"

" Literals
syn match  scamlChar       "\%([A-Za-z0-9_']\)\@1<!'\%([^\\'\n]\|\\[\\'"ntrb ]\)'"
syn match  scamlEscape     contained "\\[nt\"\\]"
syn match  scamlBadEscape  contained "\\[^nt\"\\]"
syn region scamlString     start=+"+ skip=+\\.+ end=+"+ contains=scamlEscape,scamlBadEscape

" Directive and comments (highest priority: defined last)
syn match scamlDirective   "#use\>"
syn keyword scamlTodo      contained TODO FIXME XXX NOTE
syn match scamlComment     "//.*$" contains=scamlTodo,@Spell

hi def link scamlConditional Conditional
hi def link scamlException   Exception
hi def link scamlStorage     Keyword
hi def link scamlOf          Keyword
hi def link scamlBoolean     Boolean
hi def link scamlBuiltinType Type
hi def link scamlConstructor Constant
hi def link scamlModule      Structure
hi def link scamlField       Identifier
hi def link scamlOperator    Operator
hi def link scamlArrow       Operator
hi def link scamlArrayDelim  Delimiter
hi def link scamlLambda      Keyword
hi def link scamlOpSection   Operator
hi def link scamlUnit        Constant
hi def link scamlEmpty       Constant
hi def link scamlInteger     Number
hi def link scamlFloat       Float
hi def link scamlFnName      Function
hi def link scamlTypeName    Type
hi def link scamlLetName     Identifier
hi def link scamlChar        Character
hi def link scamlEscape      SpecialChar
hi def link scamlBadEscape   Error
hi def link scamlString      String
hi def link scamlDirective   Include
hi def link scamlTodo        Todo
hi def link scamlComment     Comment

let b:current_syntax = "scaml"
