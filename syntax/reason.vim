" Vim syntax file
" Language:     Reason (Forked from Rust)
" Maintainer:   (Jordan - for Reason changes) Patrick Walton <pcwalton@mozilla.com>
" Maintainer:   Ben Blum <bblum@cs.cmu.edu>
" Maintainer:   Chris Morgan <me@chrismorgan.info>
" Last Change:  May 21st, 2019
" Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax sync fromstart

" Syntax definitions {{{1
" Basic keywords {{{2
syn keyword   reasonConditional switch if else for while
syn keyword   reasonKeyword    as to downto end of
syn match     reasonSemicolon   /;/ display

syn match     reasonAssert      "\<assert\%(\w\)*"
syn match     reasonFailwith    "\<failwith\%(\w\)*"

syn keyword   reasonTypeKeyword   type       skipwhite nextgroup=reasonRecurseType,reasonTypeDecl
syn keyword   reasonRecurseType   rec nonrec skipwhite nextgroup=reasonTypeDecl

syn keyword   reasonModuleKeyword module     skipwhite nextgroup=reasonRecurseModule,reasonModuleDeclName,reasonModuleTypeKeyword
syn keyword   reasonModuleTypeKeyword type   contained skipwhite nextgroup=reasonRecurseModule,reasonModuleTypeName
syn keyword   reasonRecurseModule rec nonrec skipwhite nextgroup=reasonModuleDeclName,reasonModuleTypeKeyword

syn match   reasonStorage            /\<let\%(\%(\$\|&\|\*\|+\|-\|\/\|<\|=\|>\|@\|^\||\|\.\|\!\)\+\w\+\)\?/        skipwhite nextgroup=reasonRecurseIdentifier,reasonIdentifier,reasonIdentifierTuple,reasonIdentifierDestructure,reasonStorageExtension
syn keyword   reasonRecurseIdentifier  rec nonrec skipwhite nextgroup=reasonIdentifier
syn match     reasonStorageExtension /%\u\%(\w\|'\)*/hs=s+1 skipwhite nextgroup=reasonRecurseIdentifier,reasonIdentifier,reasonIdentifierTuple
syn keyword   reasonTry                try        skipwhite nextgroup=reasonTryExtension
syn match     reasonTryExtension /%\u\%(\w\|'\)*/hs=s+1

syn keyword   reasonOpenKeyword   open skipwhite nextgroup=reasonModPath

syn keyword   reasonLazyKeyword   lazy

syn keyword   reasonStorage     with when where fun mutable class pub pri val inherit and exception include constraint
" FIXME: Scoped impl's name is also fallen in this category

syn match     reasonUnit /()/ display

syn match     reasonIdentifierTupleSeparator /,/             contained display skipwhite skipnl nextgroup=reasonIdentifier
syn region    reasonIdentifierTuple start="(" end=")"        contained display skipwhite skipnl contains=reasonIdentifier,reasonIdentifierSeparator
syn region    reasonIdentifierDestructure start="{" end="}"        contained display skipwhite skipnl contains=reasonModPath,reasonIdentifier,reasonIdentifierSeparator
syn match     reasonIdentifier  /\<\%(\l\|_\)\%(\k\|'\)*\>/  contained display skipwhite skipnl nextgroup=reasonIdentifierTypeSeparator,reasonIdentifierAssignmentSeparator
syn match     reasonIdentifierAssignmentSeparator /=/ contained display skipwhite skipnl nextgroup=reasonUnaryFunctionDef,reasonFunctionDef
syn match     reasonIdentifierTypeSeparator /:/ contained display skipwhite skipnl nextgroup=reasonTypeDefUnaryFunctionDef,reasonVariantDefRegion,reasonTypeDefRecord,reasonTypeDefVariantSeparator,reasonTypeDefUnaryFunctionDef,reasonTypeAliasDef,reasonTypeDefTuple,reasonTypeAliasDefModuleRef
syn match     reasonIdentifierType /\<\%(\l\|_\)\%(\k\|'\)*\%(\_s*=>\)\@!\>/ contained display skipwhite skipnl nextgroup=reasonIdentifierTypeArgList
syn match     reasonIdentifierTypeModuleRef "\<\u\%(\w\|'\)*\_s*\."he=e-1    contained display skipwhite skipnl nextgroup=reasonIdentifierType,reasonIdentifierTypeModuleRef
syn region    reasonIdentifierTypeArgList start="(" end=")"               contained display skipwhite skipnl contains=reasonIdentifierTypeArg,reasonIdentifierTypeArgModuleRef nextgroup=reasonIdentifierSeparator
syn match     reasonIdentifierTypeArg /\<\%(\l\|_\)\%(\k\|'\)*\>/         contained display skipwhite skipnl nextgroup=reasonIdentifierTypeListSeparator
syn match     reasonIdentifierTypeArgModuleRef "\<\u\%(\w\|'\)*\_s*\."he=e-1 contained display skipwhite skipnl nextgroup=reasonIdentifierTypeArg,reasonIdentifierTypeArgModuleRef

syn match     reasonTypeDecl     /\<\%(\l\|_\)\%(\k\|'\)*\>/ contained display skipwhite skipnl nextgroup=reasonTypeDefAssign,reasonTypeVariablesDecl
syn match     reasonTypeVariablesDecl /\%(([^)]\+)\)/             contained display skipwhite skipnl nextgroup=reasonTypeDefAssign
syn match     reasonTypeDefAssign /=/                        contained display skipwhite skipnl nextgroup=reasonTypeDefUnaryFunctionDef,reasonTypeDefRecord,reasonVariantDefRegion,reasonTypeDefVariantSeparator,reasonTypeDefUnaryFunctionDef,reasonTypeAliasDef,reasonTypeDefTuple,reasonTypeAliasDefModuleRef,reasonCommentLine,reasonCommentBlock
syn match     reasonTypeAliasDef /\<\%(\l\|_\)\%(\k\|'\)*\>/ contained display skipwhite skipnl nextgroup=reasonTypeAliasDefArgList
syn match     reasonTypeAliasDefModuleRef "\<\u\%(\w\|'\)*\_s*\."he=e-1    contained display skipwhite skipnl nextgroup=reasonTypeAliasDef,reasonTypeAliasDefModuleRef
syn region    reasonTypeAliasDefArgList start="(" end=")"               contained display skipwhite skipnl contains=reasonTypeAliasDefArgList,reasonTypeAliasDefArg,reasonTypeAliasDefArgModuleRef,reasonVariantDefRegion
syn match     reasonTypeAliasDefArg /'\@<!\<\%(\l\|_\)\%(\k\|'\)*\>/         contained display skipwhite skipnl nextgroup=reasonTypeAliasDefArgListSeparator
syn match     reasonTypeAliasDefArgListSeparator /,/                             contained display skipwhite skipnl nextgroup=reasonTypeAliasDefArg,reasonTypeAliasDefArgModuleRef,reasonVariantDefRegion
syn match     reasonTypeAliasDefArgModuleRef "\<\u\%(\w\|'\)*\_s*\."he=e-1 contained display skipwhite skipnl nextgroup=reasonTypeAliasDefArg,reasonTypeAliasDefArgModuleRef
syn region    reasonTypeDefTuple start="(" end=")"                      contained display skipwhite skipnl contains=reasonTypeDefRecord,reasonTypeDefVariantSeparator,reasonTypeDefUnaryFunctionDef,reasonTypeAliasDef,reasonTypeAliasDefModuleRef,reasonTypeDefTuple,bsDirective,reasonLabeledArgument nextgroup=reasonFunctionTypeArrowCharacter

syn match     reasonTypeDefVariantSeparator /|/     contained display skipwhite skipnl nextgroup=reasonVariantDef,bsDirective
syn region    reasonTypeDefRecord start="{" end="}" contained display skipwhite skipnl contains=reasonRecordFieldName,reasonRecordFieldTypeSeparator,reasonRecordFieldSeparator,reasonCommentLine,reasonCommentBlock,bsDirective

" record type definition
syn match     reasonRecordFieldName /\<\%(\l\|_\)\%(\k\|'\)*\>/            contained display skipwhite skipnl nextgroup=reasonRecordFieldTypeSeparator
syn match     reasonRecordFieldTypeSeparator /:/                           contained display skipwhite skipnl nextgroup=reasonVariantDefRegion,reasonTypeDefRecord,reasonTypeDefVariantSeparator,reasonTypeDefUnaryFunctionDef,reasonTypeAliasDef,reasonTypeDefTuple,reasonTypeAliasDefModuleRef

" variant type definition
syn region    reasonVariantDefRegion start="\[\_s*|" end="]"  contained display skipwhite skipnl contains=reasonPolyVariantDef,reasonTypeDefPolyVariantSeparator
syn match     reasonTypeDefPolyVariantSeparator /|/     contained display skipwhite skipnl nextgroup=reasonPolyVariantDef,bsDirective
syn match     reasonVariantDef  "\<\u\%(\w\|'\)*\>\.\@!"      contained display skipwhite skipnl nextgroup=reasonVariantArgsDef,reasonTypeDefVariantSeparator
syn match     reasonPolyVariantDef  "`\<\w\%(\w\|'\)*\>\.\@!" contained display skipwhite skipnl nextgroup=reasonPolyVariantArgsDef,reasonTypeDefPolyVariantSeparator
syn region    reasonVariantArgsDef start="(" end=")"       contained display skipwhite skipnl contains=reasonTypeAliasDef,reasonTypeAliasDefModuleRef nextgroup=reasonTypeDefVariantSeparator
syn region    reasonPolyVariantArgsDef start="(" end=")"       contained display skipwhite skipnl contains=reasonTypeAliasDef,reasonTypeAliasDefModuleRef nextgroup=reasonTypeDefPolyVariantSeparator,reasonTypeDefVariantTypeSeparator
syn match     reasonVariantArg /\<\%(\l\|_\)\%(\k\|'\)*\>/ contained display skipwhite skipnl nextgroup=reasonVariantArgListSeparator
syn match     reasonVariantArgListSeparator /,/            contained display skipwhite skipnl nextgroup=reasonVariantArg
syn match     reasonTypeDefVariantTypeSeparator /:/        contained display skipwhite skipnl nextgroup=reasonTypeAliasDef,reasonTypeAliasDefModuleRef

syn match     reasonFunctionTypeArrowCharacter display "=>" contained display skipwhite skipnl nextgroup=reasonTypeDefUnaryFunctionDef,reasonTypeDefRecord,reasonTypeDefVariantSeparator,reasonTypeDefUnaryFunctionDef,reasonTypeAliasDef,reasonTypeDefTuple,reasonTypeAliasDefModuleRef

" unary function type
syn match     reasonTypeDefUnaryFunctionDef /\%(\%(\w\)\%(\w\|'\|\.\)*\%((.*)\)\=\_s*=>\)\@=/ contained display skipwhite skipnl nextgroup=reasonTypeDefUnaryFunctionArgumentModuleRef,reasonTypeDefUnaryFunctionArgument
syn match     reasonTypeDefUnaryFunctionArgument "\%(\l\|_\)\%(\w\|'\)*"       contained display skipwhite skipnl nextgroup=reasonTypeDefUnaryFunctionArgumentTypeArgs,reasonFunctionTypeArrowCharacter
syn match     reasonTypeDefUnaryFunctionArgumentModuleRef "\<\u\%(\w\|'\)* *\."he=e-1    contained display skipwhite skipnl nextgroup=reasonTypeDefUnaryFunctionArgument,reasonTypeDefUnaryFunctionArgumentModuleRef,reasonFunctionTypeArrowCharacter
syn region    reasonTypeDefUnaryFunctionArgumentTypeArgs start="(" end=")"  contained display skipwhite skipnl contains=reasonTypeAliasDefModuleRef,reasonTypeAliasDef nextgroup=reasonFunctionTypeArrowCharacter

" function definition
syn match     reasonFunctionDef /\%((\%(\_s\|\w\|[~'`.,=?():>]\)*)\%(\_s*:\_s*.\+\)\=\_s*=>\)\@=/  contained display skipwhite skipnl nextgroup=reasonFunctionArguments
syn region    reasonFunctionArguments start="(" end=")"       contained display skipwhite skipnl contains=reasonArgument,reasonLabeledArgument,reasonFunctionDef nextgroup=reasonArrowCharacter,reasonFunctionDefReturnTypeSeparator
syn match     reasonArgument "\%(\l\|_\)\%(\w\|'\)*"            contained display skipwhite skipnl nextgroup=reasonArgumentSeparator,reasonArgumentTypeDecl
syn match     reasonLabeledArgument "\~\%(\l\|_\)\%(\w\|'\)*"   contained display skipwhite skipnl nextgroup=reasonArgumentPunning,reasonArgumentSeparator,reasonLabeledOptionalArgument,reasonArgumentTypeDecl
syn match     reasonLabeledOptionalArgument /=?/              contained display skipwhite skipnl nextgroup=reasonArgumentSeparator
syn keyword   reasonArgumentPunning as                        contained display skipwhite skipnl nextgroup=reasonArgumentAlias
syn match     reasonArgumentAlias /\<\%(\l\|_\)\%(\k\|'\)*\>/ contained display skipwhite skipnl nextgroup=reasonArgumentTypeDecl,reasonArgumentSeparator
syn match     reasonArgumentTypeDecl /:/                      contained display skipwhite skipnl nextgroup=reasonTypeAliasDef,reasonTypeAliasDefModuleRef
syn match     reasonFunctionDefReturnTypeSeparator /:/        contained display skipwhite skipnl nextgroup=reasonTypeAliasDef,reasonTypeAliasDefModuleRef
syn match     reasonArgumentSeparator /,/                     contained display skipwhite skipnl nextgroup=reasonArgument,reasonLabeledArgument
" unary function
syn match     reasonUnaryFunctionDef /\%(\%(\l\|_\)\%(\w\|'\)*\_s*=>\)\@=/           display skipwhite skipnl nextgroup=reasonUnaryFunctionArgument
syn match     reasonUnaryFunctionArgument "\%(\l\|_\)\%(\w\|'\)*"       contained display skipwhite skipnl nextgroup=reasonArrowCharacter

syn match     reasonConstructor  "\<\u\%(\w\|'\)*\>" display
" Polymorphic variants
syn match     reasonPolyVariant  "`\w\%(\w\|'\)*\>" display

syn match     reasonModuleTypeName "\<\u\%(\w\|'\)*\_s*" contained display skipwhite skipnl nextgroup=reasonModuleTypeAssign
syn match     reasonModuleTypeAssign /=/ contained display skipwhite skipnl nextgroup=reasonModuleTypeDecl,reasonModuleTypeAlias
syn match     reasonModuleTypeAlias "\<\u\%(\w\|'\)*\_s*" contained display skipwhite skipnl
syn region    reasonModuleTypeDecl start="{" end="}" contained display skipwhite skipnl contains=TOP
syn match     reasonModuleDeclName "\<\u\%(\w\|'\)*\_s*" contained display skipwhite skipnl nextgroup=reasonModuleDefAssign,reasonModuleTypeSeparator
syn match     reasonModuleDefAssign /=/ contained display skipwhite skipnl nextgroup=reasonModuleDecl,reasonFunctorDef,reasonModAlias
syn match     reasonModuleTypeSeparator /:/ contained display skipwhite skipnl nextgroup=reasonModuleAdHocTypeDecl,reasonModuleAdHocTypeAlias
syn match     reasonModuleAdHocTypeAlias "\<\u\%(\w\|'\)*\_s*" contained display skipwhite skipnl
syn region    reasonModuleAdHocTypeDecl start="{" end="}" contained display skipwhite skipnl contains=TOP nextgroup=reasonModuleDefAssign
syn region    reasonModuleDecl start="{" end="}" contained display skipwhite skipnl contains=TOP

" functor definition
syn match     reasonFunctorDef /\%((\%(\_s\|\w\|[~'`.,=?():>]\)*)\%(\_s*:\_s*.\+\)\=\_s*=>\)\@=/  contained display skipwhite skipnl nextgroup=reasonFunctorArguments
syn region    reasonFunctorArguments start="(" end=")"       contained display skipwhite skipnl contains=reasonFunctorArgument,reasonFunctorDef nextgroup=reasonArrowCharacter,reasonFunctorDefReturnTypeSeparator
syn match     reasonFunctorArgument "\%(\u\)\%(\w\|'\|\.\)*"            contained display skipwhite skipnl nextgroup=reasonFunctorArgumentSeparator,reasonFunctorArgumentTypeDecl
syn match     reasonFunctorArgumentTypeDecl /:/                      contained display skipwhite skipnl nextgroup=reasonFunctorArgumentType
syn match     reasonFunctorArgumentType "\<\u\%(\w\|'\|\.\)*\_s*" contained display skipwhite skipnl
syn match     reasonFunctorDefReturnTypeSeparator /:/        contained display skipwhite skipnl nextgroup=reasonModuleTypeAlias,reasonFunctorSharingConstraint
syn match     reasonFunctorArgumentSeparator /,/                     contained display skipwhite skipnl nextgroup=reasonFunctorArgument
syn region    reasonFunctorSharingConstraint start="(" end=")" contained display skipwhite skipnl contains=reasonSharingConstraintModuleType nextgroup=reasonArrowCharacter
syn match     reasonSharingConstraintModuleType "\<\u\%(\w\|'\|\.\)*" contained display skipwhite skipnl nextgroup=reasonSharingConstraintWith
syn match     reasonSharingConstraintWith /with/ contained display skipwhite skipnl nextgroup=reasonSharingConstraintList
syn region    reasonSharingConstraintList start="." end=")\@=" contained display skipwhite skipnl contains=reasonSharingConstraintType,reasonSharingConstraintSeparator
syn match     reasonSharingConstraintType /type/ contained display skipwhite skipnl nextgroup=reasonSharingConstraintInternalType
syn match     reasonSharingConstraintSeparator /and/ contained display skipwhite skipnl
syn match     reasonSharingConstraintInternalType /\<\%(\l\|_\)\%(\k\|'\)*\>/ contained display skipwhite skipnl nextgroup=reasonSharingConstraintAssign
syn match     reasonSharingConstraintAssign /=/ contained display skipwhite skipnl nextgroup=reasonTypeAliasDef,reasonTypeAliasDefModuleRef
syn match     reasonSharingConstraintExternalType /\<\%(\l\|_\)\%(\k\|'\)*\>/ contained display skipwhite skipnl nextgroup=reasonTypeAliasDef,reasonTypeAliasDefModuleRef

syn match     reasonModAlias  "\<\u\%(\w\|'\|\.\)*" display contained

"syn match     reasonModPath  /\<\u\%(\w\|\.\)*\>/ display
syn match     reasonModPath  "\<\u\%(\w\|'\)*\_s*\."he=e-1 display
syn match     reasonModPath  "\%(\<include\s\+\)\@<=\u\%(\w\|\.\)*" display
syn match     reasonModPath  "\%(\<open\s\+\)\@<=\u\%(\w\|\.\)*" display

syn keyword   reasonExternalKeyword external                                   skipwhite skipnl nextgroup=reasonExternalDecl
syn match     reasonExternalDecl /\<\%(\l\|_\)\%(\k\|'\)*\>/ contained display skipwhite skipnl nextgroup=reasonExternalSeparator
syn match     reasonExternalSeparator /:/                    contained display skipwhite skipnl nextgroup=reasonExternalTypeDef
syn region    reasonExternalTypeDef start="." end="\%(=>\@!\)\@="    contained display skipwhite skipnl contains=reasonTypeDefUnaryFunctionDef,reasonVariantDefRegion,reasonTypeDefRecord,reasonTypeDefVariantSeparator,reasonTypeDefUnaryFunctionDef,reasonTypeAliasDef,reasonTypeDefTuple,reasonTypeAliasDefModuleRef nextgroup=reasonOperator

" record field access
syn match     reasonRecordFieldAccess /\%(\<\%(\l\|_\)\%(\k\|'\)*\>\.\)\@<=\<\%(\l\|_\)\%(\k\|'\)*\>/ display

syn region    bsDirective start="\[\%(@\|%\)"rs=e-1 end="]"       display skipwhite skipnl contains=reasonMultilineString,reasonString,extensionPointName

syn match     extensionPointName /\%(@\|%\)[a-zA-Z_.]\+/    display contained

" {} are handled by reasonFoldBraces

syn region reasonMacroRepeat matchgroup=reasonMacroRepeatDelimiters start="$(" end=")" contains=TOP nextgroup=reasonMacroRepeatCount
syn match reasonMacroRepeatCount ".\?[*+]" contained
syn match reasonMacroVariable "$\w\+"

" Reexported core operators {{{3
syn keyword   reasonTrait       Copy Send Sized Sync
syn keyword   reasonTrait       Drop Fn FnMut FnOnce

" Reexported functions {{{3
" There’s no point in highlighting these; when one writes drop( or drop::< it
" gets the same highlighting anyway, and if someone writes `let drop = …;` we
" don’t really want *that* drop to be highlighted.
"syn keyword reasonFunction drop

" Reexported types and traits {{{3
syn keyword reasonTrait ToOwned
syn keyword reasonTrait Clone
syn keyword reasonTrait PartialEq PartialOrd Eq Ord
syn keyword reasonTrait AsRef AsMut Into From
syn keyword reasonTrait Default
syn keyword reasonTrait Iterator Extend IntoIterator
syn keyword reasonTrait DoubleEndedIterator ExactSizeIterator

" Other syntax {{{2
syn keyword   reasonSelf        self
syn keyword   reasonBoolean     true false

" This is merely a convention; note also the use of [A-Z], restricting it to
" latin identifiers rather than the full Unicode uppercase. I have not used
" [:upper:] as it depends upon 'noignorecase'
"syn match     reasonCapsIdent    display "[A-Z]\w\%(\w\)*"

syn match     reasonOperator     "\%(|\]\|\[|\|$\|:\|?\|\~\|\.\|#\|@\|+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)\+" display
" This one isn't *quite* right, as we could have binary-& with a reference

" This isn't actually correct; a closure with no arguments can be `|| { }`.
" Last, because the & in && isn't a sigil
"syn match     reasonOperator     display "&&\|||"
" This is reasonArrowCharacter rather than reasonArrow for the sake of matchparen,
" so it skips the ->; see http://stackoverflow.com/a/30309949 for details.
syn match     reasonArrowCharacter display "=>"

syn match     reasonEscapeError   display contained /\\./
syn match     reasonEscape        display contained /\\\%([nrt0\\'"]\|x\x\{2}\)/
syn match     reasonEscapeUnicode display contained /\\\%(u\x\{4}\|U\x\{8}\)/
syn match     reasonEscapeUnicode display contained /\\u{\x\{1,6}}/
syn match     reasonStringContinuation display contained /\\\n\s*/
syn region    reasonString      start=+b"+ skip=+\\\\\|\\"+ end=+"+ contains=reasonEscape,reasonEscapeError,reasonStringContinuation
syn region    reasonString      start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=reasonEscape,reasonEscapeUnicode,reasonEscapeError,reasonStringContinuation,@Spell
syn region    reasonString      start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

syn region    reasonMultilineString      start=+b{|+ skip=+\\\\\|\\"+ end=+|}+ contains=reasonEscape,reasonEscapeError,reasonStringContinuation
syn region    reasonMultilineString      start=+{|+ end=+|}+ contains=reasonEscape,reasonEscapeUnicode,reasonEscapeError,reasonStringContinuation,@Spell

" Number literals
syn match     reasonDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\)\)\="
syn match     reasonHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\)\)\="
syn match     reasonOctNumber   display "\<0o[0-7_]\+\%([iu]\%(size\|8\|16\|32\|64\)\)\="
syn match     reasonBinNumber   display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\)\)\="

" Special case for numbers of the form "1." which are float literals, unless followed by
" an identifier, which makes them integer literals with a method call or field access,
" or by another ".", which makes them integer literals followed by the ".." token.
" (This must go first so the others take precedence.)
syn match     reasonFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
syn match     reasonFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\%(f32\|f64\)\="
syn match     reasonFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\%(f32\|f64\)\="
syn match     reasonFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\%(f32\|f64\)"

" For the benefit of delimitMate

syn match   reasonCharacterInvalid   display contained /b\?'\zs[\n\r\t']\ze'/
" The groups negated here add up to 0-255 but nothing else (they do not seem to go beyond ASCII).
syn match   reasonCharacterInvalidUnicode   display contained /b'\zs[^[:cntrl:][:graph:][:alnum:][:space:]]\ze'/
syn match   reasonCharacter   /b'\%([^\\]\|\\\%(.\|x\x\{2}\)\)'/ contains=reasonEscape,reasonEscapeError,reasonCharacterInvalid,reasonCharacterInvalidUnicode
syn match   reasonCharacter   /'\%([^\\]\|\\\%(.\|x\x\{2}\|u\x\{4}\|U\x\{8}\|u{\x\{1,6}}\)\)'/ contains=reasonEscape,reasonEscapeUnicode,reasonEscapeError,reasonCharacterInvalid

syn match reasonShebang /\%^#![^[].*/
syn region reasonCommentLine                                        start="//"                      end="$"   contains=reasonTodo,@Spell
" syn region reasonCommentLineDoc                                     start="//\%(//\@!\|!\)"         end="$"   contains=reasonTodo,@Spell
syn region reasonCommentBlock    matchgroup=reasonCommentBlock        start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=reasonTodo,reasonCommentBlockNest,@Spell
syn region reasonCommentBlockDoc matchgroup=reasonCommentBlockDoc     start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=reasonTodo,reasonCommentBlockDocNest,@Spell
syn region reasonCommentBlockNest matchgroup=reasonCommentBlock       start="/\*"                     end="\*/" contains=reasonTodo,reasonCommentBlockNest,@Spell contained transparent
syn region reasonCommentBlockDocNest matchgroup=reasonCommentBlockDoc start="/\*"                     end="\*/" contains=reasonTodo,reasonCommentBlockDocNest,@Spell contained transparent
" FIXME: this is a really ugly and not fully correct implementation. Most
" importantly, a case like ``/* */*`` should have the final ``*`` not being in
" a comment, but in practice at present it leaves comments open two levels
" deep. But as long as you stay away from that particular case, I *believe*
" the highlighting is correct. Due to the way Vim's syntax engine works
" (greedy for start matches, unlike Zust's tokeniser which is searching for
" the earliest-starting match, start or end), I believe this cannot be solved.
" Oh you who would fix it, don't bother with things like duplicating the Block
" rules and putting ``\*\@<!`` at the start of them; it makes it worse, as
" then you must deal with cases like ``/*/**/*/``. And don't try making it
" worse with ``\%(/\@<!\*\)\@<!``, either...

syn keyword reasonTodo contained TODO FIXME XXX NB NOTE

" Folding rules {{{2
" Trivial folding rules to begin with.
" FIXME: use the AST to make really good folding
syn region reasonFoldBraces start="{" end="}" transparent fold

" Default highlighting {{{1
hi def link labelArgument       Special
hi def link labelArgumentPunned Special
hi def link reasonDecNumber       reasonNumber
hi def link reasonHexNumber       reasonNumber
hi def link reasonOctNumber       reasonNumber
hi def link reasonBinNumber       reasonNumber
hi def link reasonTrait           reasonType

hi def link reasonMacroRepeatCount   reasonMacroRepeatDelimiters
hi def link reasonMacroRepeatDelimiters   Macro
hi def link reasonMacroVariable Define
hi def link reasonEscape        Special
hi def link reasonEscapeUnicode reasonEscape
hi def link reasonEscapeError   Error
hi def link reasonStringContinuation Special
hi def link reasonString          String
hi def link reasonMultilineString String
hi def link reasonCharacterInvalid Error
hi def link reasonCharacterInvalidUnicode reasonCharacterInvalid
hi def link reasonCharacter     Character
hi def link reasonNumber        Number
hi def link reasonBoolean       Boolean
"hi def link reasonConstructor   Constant
hi def link reasonModPath       Include
hi def link reasonSelf          Constant
hi def link reasonFloat         Float
hi def link reasonKeyword       Keyword
hi def link reasonConditional   Conditional
hi def link reasonIdentifier    Identifier
hi def link reasonCapsIdent     reasonIdentifier
hi def link reasonFunctionDef      Function
hi def link reasonUnaryFunctionDef      Function
hi def link reasonShebang       Comment
hi def link reasonCommentLine   Comment
" hi def link reasonCommentLineDoc Comment
hi def link reasonCommentBlock  Comment
hi def link reasonCommentBlockDoc Comment
hi def link reasonAssert        Precondit
hi def link reasonFailwith      PreCondit
hi def link reasonType          Type
hi def link reasonTodo          Todo
hi def link reasonObsoleteStorage Error

" operators
hi def link reasonArgumentPunning reasonOperator

" keywords
hi def link reasonRecurseType reasonKeyword
hi def link reasonModuleKeyword reasonKeyword
hi def link reasonStorage reasonKeyword
hi def link reasonTypeKeyword reasonKeyword
hi def link reasonModuleTypeKeyword reasonKeyword
hi def link reasonExternalKeyword reasonKeyword
hi def link reasonRecurseIdentifier reasonKeyword
hi def link reasonRecurseModule reasonKeyword
hi def link reasonOpenKeyword reasonKeyword
hi def link reasonLazyKeyword reasonKeyword
hi def link reasonSharingConstraintWith reasonKeyword
hi def link reasonSharingConstraintType reasonKeyword
hi def link reasonSharingConstraintSeparator reasonKeyword
hi def link reasonTry reasonKeyword
hi def link extensionPointName reasonKeyword

" separators
hi def link reasonArrowCharacter reasonSeparator
hi def link reasonFunctionTypeArrowCharacter reasonSeparator
hi def link reasonLabeledOptionalArgument reasonSeparator
hi def link reasonExternalSeparator reasonSeparator
hi def link reasonRecordFieldTypeSeparator reasonSeparator
hi def link reasonIdentifierTypeSeparator reasonSeparator
hi def link reasonRecordFieldSeparator reasonSeparator
hi def link reasonArgumentSeparator reasonSeparator
hi def link reasonTypeDefVariantSeparator reasonSeparator
hi def link reasonVariantArgListSeparator reasonSeparator
hi def link reasonTypeDefAssign reasonSeparator
hi def link reasonSemicolon reasonSeparator
hi def link reasonIdentifierAssignmentSeparator reasonSeparator

" include path
hi def link reasonIdentifierTypeModuleRef reasonModPath
hi def link reasonIdentifierTypeArgModuleRef reasonModPath
hi def link reasonTypeAliasDefModuleRef reasonModPath
hi def link reasonTypeDefUnaryFunctionArgumentModuleRef reasonModPath
hi def link reasonTypeAliasDefArgModuleRef reasonModPath
hi def link reasonModuleDeclName reasonModPath
hi def link reasonModuleTypeName reasonModPath
hi def link reasonSharingConstraintModuleType reasonModPath
hi def link reasonFunctorArgumentType reasonModPath
hi def link reasonStorageExtension reasonModPath
hi def link reasonTryExtension reasonModPath
hi def link reasonModAlias reasonModPath

" types
hi def link reasonIdentifierType reasonType
hi def link reasonVariantArg reasonType
hi def link reasonIdentifierTypeArg reasonType
hi def link reasonTypeDefUnaryFunctionArgument reasonType
hi def link reasonTypeAliasDef reasonType
hi def link reasonTypeAliasDefArg reasonType
hi def link reasonTypeDecl reasonType
hi def link reasonSharingConstraintInternalType reasonType

let b:current_syntax = "reason"
