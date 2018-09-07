/?    310
!:
:-  %say
|=  {^ {{demo=hoon ~} ~}}
:-  %txt
^-  wain
=<  =/  plum=plum  (hoon-to-plum demo)
    ~(tall plume plum)
:: |=  {^ {{demo=type ~} ~}}
:: :-  %txt
:: ^-  wain
:: =<  =/  plum=plum  (hoon-to-plum (type-to-hoon demo))
::     ~(tall plume plum)
|%
::
::  A `plum` is the intermediate representation for the pretty-printer. It
::  encodes hoon-like data with the least amount of structured needed
::  for formating.
::
::  A `plum` is either a
::
::  - `cord`: A simple cord
::  - `[%para *]`: A wrappable paragraph.
::  - `[%tree *]`: A formated plum tree
::  - `[%sbrk *]`: An indication of a nested subexpression.
::
::  The formatter will use the tall mode unless:
::
::    - A plum has only a `wide` style.
::    - The plum is in `%sbrk` form and it's subplum (`kid`), when
::      formatted in wide mode, can fit on a single line.
::
+$  plum
  $@  cord
  $%  [%para prefix=tile lines=(list @t)]
      [%tree fmt=plumfmt kids=(list plum)]
      [%sbrk kid=plum]
  ==
::
::  A `plumfmt` is a description of how to render a `plum`. A `plumfmt`
::  must include a `wide`, a `tall`, or both.
::
::  A `wide` is a description of how to render a plum in a single
::  line. The nested (`kids`) sub-plums will be interleaved with `delimit`
::  strings, and, if `enclose` is set, then the output will be enclosed
::  with `p.u.enclose` abnd `q.u.enclose`.
::
::  For examle, to build a plumfmt for string literals, we could write:
::
::      [wide=[~ '' [~ '"' '"']] tall=~]
::
::  A `tall` is a description of how to render a plum accross multiple
::  lines. The output will be prefixed by `intro`, suffixed by
::  `final.u.indef`, and each subplum prefixed by `sigil.u.indef`.
::
::  For example, to build a plumfmt for cores, we could write:
::
::      [wide=~ tall=`['' `['++' '--']]]
::
+$  plumfmt
  $:  wide=(unit [delimit=tile enclose=(unit (pair tile tile))])
      tall=(unit [intro=tile indef=(unit [sigil=tile final=tile])])
  ==
::
::  Map a gate over a list and then concatenate the results.
::
++  flat-map
  |*  {a/(list) b/gate}
  (zing (turn a b))
::
::  Append an element to the end of a list.
::
++  snoc
  |*  {a/(list) b/*}
  (weld a ^+(a [b]~))
::
::  Prepend an element to the front of a list.
::
++  cons
  |*  {a/* b/(list *)}
  [a b]
::
::  Render an `axis`.
::
++  axis-to-cord
  |=  p=@
  ^-  cord
  ?:  =(p 1)  '.'
  ?:  =(p 2)  '-'
  ?:  =(p 3)  '+'
  (cat 3 '+' (scot %ud p))
::
::  Render a limb.  A limb is either an empty atom (which is rendered as
::  '$'), an axis, or a
::
++  limb-to-plum
  |=  =limb
  ^-  plum
  ?@  limb
    ?:  .=('' limb)  '$'
      limb
  ?-  -.limb
    %&  (axis-to-cord p.limb)
    ::  {%| p/@ud q/(unit term) ]
    %|  (crip (runt [0 p.limb] ?~(q.limb "," (trip u.q.limb))))
                                                        ::  ^ TODO This `runt`
                                                        ::  does nothing.  What
                                                        ::  was the idea here?
                                                        ::  TODO What is this
                                                        ::  comma supposed
                                                        ::  to be?
  ==
::
::  This is a helper function for testing out this code. It just digs
::  through a type and finds hoon values reference within that type.
::
++  type-to-hoon
  |=  =hoon=type
  ^-  hoon
  ?+  hoon-type  [%zpzp ~]
  [%core *]
    =/  tomes=(list tome)  ~(val by q.r.q.hoon-type)
    =/  hoons=(list hoon)  (turn tomes |=(t=tome [%cltr ~(val by q.t)]))
    [%cltr hoons]
  ==
::
++  wing-to-plum
  |=  =wing
  ^-  plum
  :+  %tree
    [wide=`['.' ~] tall=~]
  (turn `^wing`wing limb-to-plum)
::
++  battery-spec-to-plum-list
  |=  =(map term spec)
  %+  turn  ~(tap by map)
  |=  [=term =spec]
  :-  %sbrk
  :+  %tree
    [wide=~ tall=`['' ~]]
  [term (spec-to-plum spec) ~]
::
++  core-spec-to-plum
  |=  [=knot =spec =(map term spec)]
  ^-  plum
  :-  %sbrk
  :+  %tree
    [~ `[knot ~]]
  :~  (spec-to-plum spec)
      :+  %tree
        [~ tall=`['' `['++' '--']]]
      (battery-spec-to-plum-list map)
  ==
::
++  varying
  |=  [intro=knot final=knot]
  [`[' ' `[(cat 3 intro '(') ')']] `[intro `['' final]]]
::
::  TODO What is this and why?
::
++  fixed
  |=  @ta
  [`[' ' `[(cat 3 +< '(') ')']] `[+< ~]]
::
::  TODO What is this and why?
::
++  standard
  |=  =stud
  ^-  plum
  ?@  stud  stud
  :+  %tree
    [wide=`['/' ~] tall=~]
  `(list plum)`[auth.stud type.stud]
::
::  TODO What is this and why?
::
++  limb-plum
  |=  =limb
  ^-  plum
  ?^  limb  %todo-wtf-is-this  limb
::
::  TODO What is this and why?
::
++  tall-fixed
  |=  rune=cord
  ^-  (unit [cord (unit [cord cord])])
  [~ rune ~]
::
::  TODO What is this and why?
::
++  tall-running
  |=  [rune=cord sigil=cord term=cord]
  ^-  (unit [cord (unit [cord cord])])
  [~ rune [~ sigil term]]
::
::  TODO What is this and why?
::
++  woof-plum
  |=  =woof
  ^-  plum
  ?@  woof
    woof
  :+  %tree
    :-  wide=`[' ' `['{' '}']]  tall=~
  (turn (unwrap-woof-tuple +.woof) hoon-to-plum)
::
::  TODO What is this and why?
::
++  unwrap-woof-tuple
  |=  =hoon
  ^-  (list ^hoon)
  ?:  ?=([%cltr *] hoon)
    p.hoon
  ~[hoon]
::
::  TODO What is this and why?
::
++  hoons-to-plum-list
  |=  =hoon=(list hoon)
  ^.  (list plum)
  (turn hoon-list hoon-to-plum)
::
::  TODO What is this and why?
::
++  raw-tall-plum
  |=  kids=(list plum)
  ^.  plum
  tree/[[wide=~ tall=[~ '' ~]] kids]
::
::  TODO What is this and why?
::
++  rune-short-form
  |=  [rune=cord short=(unit [fst=cord mid=cord lst=cord])]
  ^-  (unit (pair cord (unit [cord cord])))
  :+  ~  ?~(short ' ' mid.u.short)
  :-  ~
  ?^  short   [fst.u.short lst.u.short]
  [(cat 3 rune '(') ')']
::
::  TODO What is this and why?
::
++  rune-to-plum
  |=  $:  rune=cord
          term=(unit cord)
          short=(unit [cord cord cord])
          kids=(list plum)
      ==
  ^.  plum
  :-  %sbrk
  :+  %tree
    :-  (rune-short-form rune short)
    ?~  term  (tall-fixed rune)
    (tall-running rune '' u.term)
  kids
::
::  TODO What is this and why?
::
++  chum-to-plum
  |=  =chum
  ^-  plum
  %todo-chum
::
::  TODO What is this and why?
::
++  tyre-to-plum
  |=  =tyre
  ^-  plum
  %todo-tyre
::
::  TODO What is this and why?
::
++  matches-to-plum-list
  |=  =update=(list (pair spec hoon))
  ^-  (list plum)
  %-  add-trailing-commas-to-wide-form
  %+  turn  update-list
  |=  [=spec =hoon]
  ^-  (pair plum plum)
  [(spec-to-plum spec) (hoon-to-plum hoon)]
::
::  TODO What is this and why?
::
++  updates-to-plum-list
  |=  =update=(list (pair wing hoon))
  ^-  (list plum)
  %-  add-trailing-commas-to-wide-form
  %+  turn  update-list
  |=  [=wing =hoon]
  ^-  (pair plum plum)
  [(wing-to-plum wing) (hoon-to-plum hoon)]
::
::  This adds commas to a list of pair of hoons, but only in wide form.
::
::  For example, in wide form with commas:
::
::    %=($ a 1, b 2)
::
::  In tall form without commas:
::
::    %=  $  a  1  b  2  ==
::
::  It's important that this not be wrapped in an %sbrk, since we need
::  to be sure that this is rendered in wide mode if-and-only-if our
::  parent is rendered in wide mode.
::
++  add-trailing-commas-to-wide-form
  |=  plums=(list (pair plum plum))
  =|  acc=(list (list plum))
  |^  ^-  (list plum)
    ?~  plums  (zing (flop acc))
    =/  x=plum  p.i.plums
    =/  y=plum  q.i.plums
    ?~  t.plums
      $(plums t.plums, acc [~[x y] acc])
    $(plums t.plums, acc [~[x (comma y)] acc])
  ++  comma
    |=  =sub=plum
    ^-  plum
    :+  %tree
      :-  [~ '' [~ '' ',']]  [~ '' ~]
    ~[sub-plum]
  --
::
::  Just a helper function for constructing a wide-form %tree plum.
::
++  simple-wide-plum
  |=  [init=cord sep=cord end=cord babes=(list plum)]
  ^-  plum
  :+  %tree  [wide=[~ sep [~ init end]] tall=~]  babes
::
::  XX Add special wide forms for:
::
::    - `$(a b)`
::    - `!x`
::    - `:x`
::
++  hoon-to-plum
  |=  x=hoon
  |^  ^-  plum
    ?+    x
        %autocons
      [%$ @]     (axis-to-cord p.x)
      [%base *]  (spec [%base p.x])
      [%bust *]  (simple-wide-plum '*' '' '' (spec [%base p.x]) ~)
      [%dbug *]  (hn q.x)                               ::  p.x is irrelevant
      [%eror *]  %assembly-error
      [%hand *]  %ast-node-hand
      [%note *]  (hn q.x)                               ::  p.x is irrelevant
      [%fits *]  %ast-node-fits
      [%knit *]  (simple-wide-plum '"' '' '"' (turn p.x woof-plum))
      [%leaf *]  (spec x)
      [%limb *]  p.x
      [%lost *]  (hn p.x)                               ::  for internal use
      [%rock *]  ?^  q.x  !!  (cat 3 '%' (crip (scow p.x `@`q.x)))
      [%sand *]  ?^  q.x  !!  (crip (scow p.x `@`q.x))
      [%tell *]  (simple-wide-plum '<' ' ' '>' (hoons p.x))
      [%tune *]  ?@(p.x p.x %todo-tune)
      [%wing *]  (simple-wide-plum '' '.' '' (turn p.x limb-to-plum))
      [%yell *]  (simple-wide-plum '>' ' ' '<' (hoons p.x))
      [%xray *]  (xray-to-plum p.x)
      [%brcb *]  (chapter '|_' `(spec p.x) r.x)         ::  skip aliases
      [%brcl *]  (rune '|:' ~ ~ (hoons ~[p q]:x))
      [%brcn *]  (chapter '|%' ~ q.x)                   ::  Ignoring p.x
      [%brdt *]  (rune '|.' ~ ~ (hoons ~[p]:x))
      [%brkt *]  (chapter '|^' `(hn p.x) q.x)
      [%brhp *]  (rune '|-' ~ ~ (hn p.x) ~)
      [%brsg *]  (rune '|~' ~ ~ (spec p.x) (hn q.x) ~)
      [%brtr *]  (rune '|*' ~ ~ (spec p.x) (hn q.x) ~)
      [%brts *]  (rune '|=' ~ ~ (spec p.x) (hn q.x) ~)
      [%brvt *]  (chapter '|@' ~ q.x)                   ::  Ignoring p.x
      [%brwt *]  (rune '|?' ~ ~ (hn p.x) ~)
      [%clcb *]  (rune ':_' ~ ~ (hoons ~[p q]:x))
      [%clkt *]  (rune ':^' ~ ~ (hoons ~[p q r s]:x))
      [%clhp *]  (rune ':-' ~ `['[' spc ']'] (hoons ~[p q]:x))
      [%clls *]  (rune ':+' ~ `['[' spc ']'] (hoons ~[p q r]:x))
      [%clsg *]  (rune ':~' `'==' `['~[' spc ']'] (hoons p.x))
      [%cltr *]  ?~  p.x    '~'
                 ?~  +.p.x  (hn -.p.x)
                 (rune ':*' `'==' `['[' spc ']'] (hoons p.x))
      [%cncb *]  (rune '%_' `'==' ~ (wing p.x) (updates q.x))
      [%cndt *]  (rune '%.' ~ ~ (hoons ~[p q]:x))
      [%cnhp *]  (rune '%-' ~ `['(' spc ')'] (hoons ~[p q]:x))
      [%cncl *]  (rune '%:' `'==' `['(' spc ')'] (hoons [p q]:x))
      [%cntr *]  (rune '%*' `'==' ~ (wing p.x) (hn q.x) (updates r.x))
      [%cnkt *]  (rune '%^' ~ ~ (hoons ~[p q r s]:x))
      [%cnls *]  (rune '%+' ~ ~ (hoons ~[p q r]:x))
      [%cnsg *]  (rune '%~' `'==' `['~(' spc ')'] (wing p.x) (hoons [q r]:x))
      [%cnts *]  ?~  q.x  (wing p.x)
                 (rune '%=' `'==' ~ (wing p.x) (updates q.x))
      [%dtkt *]  (rune '.^' ~ ~ (spec p.x) (hn q.x) ~)
      [%dtls *]  (rune '.+' ~ `['+(' spc ')'] (hoons ~[p]:x))
      [%dttr *]  (rune '.*' ~ ~ (hoons ~[p q]:x))
      [%dtts *]  (rune '.=' ~ `['=(' spc ')'] (hoons ~[p q]:x))
      [%dtwt *]  (rune '.?' ~ ~ (hoons ~[p.x]))
      [%ktbr *]  (rune '^|' ~ ~ (hoons ~[p.x]))
      [%ktcn *]  (rune '^%' ~ ~ (hoons ~[p]:x))
      [%ktdt *]  (rune '^.' ~ ~ (hoons ~[p q]:x))
      [%ktls *]  (rune '^+' ~ ~ (hoons ~[p q]:x))
      [%kthp *]  (rune '^-' ~ ~ ~[(spec p.x) (hn q.x)])
      [%ktpd *]  (rune '^&' ~ ~ (hoons ~[p]:x))
      [%ktsg *]  (rune '^~' ~ ~ (hoons ~[p]:x))
      [%ktts *]  (rune '^=' ~ `['' '=' ''] ~[(skin p.x) (hn q.x)])
      [%ktwt *]  (rune '^?' ~ ~ (hoons ~[p]:x))
      [%kttr *]  (rune '^*' ~ ~ ~[(spec p.x)])
      [%ktcl *]  (rune '^:' ~ ~ ~[(spec p.x)])
      [%sgbr *]  (rune '~|' ~ ~ (hoons ~[p q]:x))
      [%sgcb *]  (rune '~_' ~ ~ (hoons ~[p q]:x))
      [%sgcn *]  (rune '~%' ~ ~ (chum p.x) (hn q.x) (tyre r.x) (hn s.x) ~)
      [%sgnt *]  (rune '~/' ~ ~ (chum p.x) (hn q.x) ~)
      [%sgld *]  (rune '~<' ~ ~ (hint p.x) (hn q.x) ~)
      [%sgbn *]  (rune '~>' ~ ~ (hint p.x) (hn q.x) ~)
      [%sgbs *]  (rune '~$' ~ ~ p.x (hn q.x) ~)
      [%sgls *]  (rune '~+' ~ ~ (hn q.x) ~)             ::  Ignoring p.x
      [%sgpd *]  (rune '~&' ~ ~ (hoons ~[q r]:x))       ::  Ignoring p.x
      [%sgts *]  (rune '~=' ~ ~ (hoons ~[p q]:x))
      [%sgwt *]  (rune '~?' ~ ~ (hoons ~[q r s]:x))     ::  Ignoring p.x
      [%sgzp *]  (rune '~!' ~ ~ (hoons ~[p q]:x))
      [%mcts *]  %ast-node-mcts
      [%mccl *]  (rune ';:' `'==' `[':(' spc ')'] (hoons [p q]:x))
      [%mcnt *]  (rune ';/' ~ ~ (hoons ~[p]:x))
      [%mcsg *]  (rune ';~' `'==' ~ (hoons [p q]:x))
      [%mcmc *]  (rune ';;' ~ ~ (hoons ~[p q]:x))
      [%tsbr *]  (rune ';;' ~ ~ ~[(spec p.x) (hn q.x)])
      [%tscl *]  (tiscol-to-plum p.x q.x)
      [%tsnt *]  (rune '=/' ~ ~ (skin p.x) (hn q.x) (hn r.x) ~)
      [%tsmc *]  (rune '=;' ~ ~ [(skin p.x) (hoons ~[q r]:x)])
      [%tsdt *]  (rune '=.' ~ ~ [(wing p.x) (hoons ~[q r]:x)])
      [%tswt *]  (rune '=?' ~ ~ [(wing p.x) (hoons ~[q r s]:x)])
      [%tsld *]  (rune '=>' ~ `['' ':' ''] (hoons ~[p q]:x))
      [%tshp *]  (rune '=-' ~ ~ (hoons ~[p q]:x))
      [%tsbn *]  (rune '=<' ~ ~ (hoons ~[p q]:x))
      [%tskt *]  (rune '=^' ~ ~ [(skin p.x) (wing q.x) (hoons ~[r s]:x)])
      [%tsls *]  (rune '=+' ~ ~ (hoons ~[p q]:x))
      [%tssg *]  (rune '=~' `'==' ~ (hoons p:x))
      [%tstr *]  ?~  q.p.x
                   (rune '=*' ~ ~ p.p.x (hoons ~[q r]:x))
                 (rune '=*' ~ ~ (spec [%bsts p.p.x u.q.p.x]) (hoons ~[q r]:x))
      [%tscm *]  (rune '=,' ~ ~ (hoons ~[p q]:x))
      [%wtbr *]  (rune '?|' `'--' `['|(' ' ' ')'] (hoons p:x))
      [%wthp *]  (rune '?-' `'==' ~ (wing p.x) (matches q.x))
      [%wtcl *]  (rune '?:' ~ ~ (hoons ~[p q r]:x))
      [%wtdt *]  (rune '?.' ~ ~ (hoons ~[p q r]:x))
      [%wtkt *]  (rune '?^' ~ ~ [(wing p.x) (hoons ~[q r]:x)])
      [%wtld *]  (rune '?<' ~ ~ (hoons ~[p q]:x))
      [%wtbn *]  (rune '?>' ~ ~ (hoons ~[p q]:x))
      [%wtls *]  (rune '?+' `'==' ~ (wing p.x) (hn q.x) (matches r.x))
      [%wtpd *]  (rune '?&' `'==' `['&(' ' ' ')'] (hoons p:x))
      [%wtvt *]  (rune '?@' ~ ~ (wing p.x) (hoons ~[q r]:x))
      [%wtsg *]  (rune '?~' ~ ~ (wing p.x) (hoons ~[q r]:x))
      [%wthx *]  (rune '?#' ~ ~ (skin p.x) (wing q.x) ~)
      [%wtts *]  (rune '?=' ~ ~ (spec p.x) (wing q.x) ~)
      [%wtzp *]  (rune '?!' ~ `['!' '' ''] (hoons ~[p]:x))
      [%zpcm *]  (rune '!,' ~ ~ (hoons ~[p q]:x))
      [%zpbn *]  (rune '!>' ~ ~ (hoons ~[p]:x))
      [%zpmc *]  (rune '!;' ~ ~ (hoons ~[p q]:x))
      [%zpts *]  (rune '!=' ~ ~ (hoons ~[p]:x))
      [%zpvt *]  (rune '!@' ~ ~ (wingseq p.x) (hoons ~[q r]:x))
      [%zpwt *]  (hn q.x)                               ::  Ignore p.x
      [%zpzp ~]  '!!'
    ==
    ++  battery    battery-to-plum-list
    ++  chapter    chapters-to-plum
    ++  chum       chum-to-plum
    ++  hint       hint-to-plum
    ++  hn         hoon-to-plum
    ++  hoons      hoons-to-plum-list
    ++  matches    matches-to-plum-list
    ++  rune       rune-to-plum
    ++  skin       skin-to-plum
    ++  spc        ' '
    ++  spec       spec-to-plum
    ++  tyre       tyre-to-plum
    ++  updates    updates-to-plum-list
    ++  wing       wing-to-plum
    ++  wingseq    wingseq-to-plum
    ::
    ::  Note [TisCol Order]
    ::  ~~~~~~~~~~~~~~~~~~~
    ::  By accumulating over the updates list from the front, we are
    ::  effectivly reversing the assignment order of the forms in `.=`.
    ::  This is semantically correct:
    ::
    ::      > =a 3
    ::      > =b 4
    ::      > =:  a  4  b  a  ==  b
    ::      3
    ::      > +hoon-printer !,  *hoon  =:  a  4  b  a  ==  b
    ::      <|=.(b a =.(a 4 b))|>
    ::      > =.(a 4 =.(b a b))
    ::      4
    ::      > =.(b a =.(a 4 b))
    ::      3
    ::
    ::  Here's an example of what a hint looks like.
    ::
    ::      ~>(%mean.[%leaf "need"] !!)
    ::
    ::  The actual form that we're printing here looks something like this:
    ::
    ::      %mean.[%leaf "need"]
    ::
    ::  XX I'm not sure if the `[%leaf "need"]` bit represents a literal
    ::  AST fragment or an expression that evaluates to `[%leaf "need"]. I'm
    ::  going to assume the latter for now.
    ::
    ++  tiscol-to-plum
      |=  [updates=(list [^wing hoon]) body=hoon]
      ^-  plum
      =/  rem=(list (pair ^wing hoon))  updates         ::  Note [TisCol Order]
      =/  acc=hoon  body
      %-  hoon-to-plum
      |-  ^-  hoon
      ?~  rem  acc
      $(rem t.rem, acc `hoon`[%tsdt `^wing`p.i.rem `hoon`q.i.rem `hoon`acc])
  --
::
::  TODO What is this and why?
::
++  hint-to-plum
  |=  hint=$@(term (pair term hoon))
  ^-  plum
  ?@  hint  (cat 3 '%' hint)
  :+  %tree
    [wide=`['.' ~] tall=~]
  :~  (cat 3 '%' p.hint)
      (hoon-to-plum q.hint)
  ==
::
::  TODO What is this and why?
::
++  battery-to-plum-list
  |=  =(map term hoon)
  ^-  (list plum)
  %+  turn  ~(tap by map)
  |=  [=term =hoon]
  :+  %tree
    [wide=~ tall=`['' ~]]
  [term (hoon-to-plum hoon) ~]
::
::  TODO What is this and why?
::
++  core-to-plum
  |=  [=knot head=(unit plum) =(map term hoon)]
  ^-  plum
  :-  %sbrk
  :-  %tree
    ?~  head
      :-
        [~ `[knot `['++' '--']]]
        (battery-to-plum-list map)
    :-
      [~ `[knot ~]]
      :~  u.head
          :+  %tree
            [~ `['' `['++' '--']]]
          (battery-to-plum-list map)
      ==
::
::  TODO What is this and why?
::
++  chapters-to-plum
  |=  [=knot head=(unit plum) =(map term tome)]
  ^-  plum
  =/  chapters=(list (pair term tome))  ~(tap by map)
  ?~  chapters  (chapters-to-plum-verbose knot head map)
  ?~  t.chapters
    ?:  .=('' p.i.chapters)
      (core-to-plum knot head q.q.i.chapters)
    (chapters-to-plum-verbose knot head map)
  (chapters-to-plum-verbose knot head map)
::
::  TODO What is this and why?
::
++  chapters-to-plum-verbose
  |=  [=knot head=(unit plum) =(map term tome)]
  ^-  plum
  =/  chaps=(list (pair term tome))
    ~(tap by map)
  :+  %tree
    [~ `[knot `['' '--']]]
  =/  kids=(list plum)
    %+  turn  chaps
    chapter-to-plum
  ?~  head  kids
  [u.head kids]
::
::  TODO What is this and why?
::
++  chapter-to-plum
  |=  [nm=knot [* bat=(map term hoon)]]
  ^-  plum
  :+  %tree
    [~ `['+|' ~]]
  :~  (cat 3 '%' nm)
      :+  %tree
        [~ `['' `['++' '']]]
      (battery-to-plum-list bat)
  ==
::
::  TODO What is this and why?
::
++  chapters-to-plum-list
  |=  =(map term tome)
  ^-  (list plum)
  %+  turn  ~(tap by map)
  |=  [=term [* hoons=(^map term hoon)]]
  ^-  plum
  ?:  =(term '')
    (raw-tall-plum (battery-to-plum-list hoons))
  (rune-to-plum '+|' ~ ~ [(cat 3 '%' term) (battery-to-plum-list hoons)])
::
::  TODO What is this and why?
::
++  xray-to-plum
  |=  =manx:hoot
  ^-  plum
  %ast-node-xray                                        ::  XX Punt
::
::  TODO What is this and why?
::
++  skin-to-plum
  |=  =skin
  ^-  plum
  ?@  skin  skin
  %todo-complex-skin                                    ::  XX Punt
::
::  TODO What is this and why?
::
++  wingseq-to-plum
  |=  =(list wing)
  ^-  plum
  :+  %tree
    [wide=`[':' ~] tall=~]
  (turn list wing-to-plum)
::
::  TODO What is this and why?
::
++  subtree
  |=  [p=plumfmt q=(list plum)]
  ^-  plum
  [%sbrk [%tree p q]]
::
::  TODO What is this and why?
::
++  spec-to-plum
  |=  =spec
  ^-  plum
  ?-  -.spec
    %base  ?-  p.spec
             %noun  '*'
             %cell  '^'
             %flag  '?'
             %null  '~'
             %void  '!!'
             [%atom *]  (cat 3 '@' p.p.spec)
           ==
    %dbug  $(spec q.spec)
    %leaf  =+((scot p.spec q.spec) ?:(=('~' -) - (cat 3 '%' -)))
    %like  tree/[[`[':' ~] ~] (turn `(list wing)`+.spec wing-to-plum)]
    %loop  (cat 3 '$' p.spec)
    %name  $(spec q.spec)
    %made  $(spec q.spec)
    %over  $(spec q.spec)
    %make  =+  (lent q.spec)
           :-  %sbrk
           :+  %tree
             :-  wide=`[' ' `['(' ')']]
             :-  ~
             ?:  |((gth - 3) =(- 0))
               ['%:' `['' '==']]
             :_  ~
             ?:  =(- 3)  '%^'
             ?:  =(- 2)  '%+'  '%-'
           [(hoon-to-plum p.spec) (turn q.spec ..$)]
    %bsbs  (core-spec-to-plum '$$' p.spec q.spec)
    %bsbr  (subtree (fixed '$|') $(spec p.spec) (hoon-to-plum q.spec) ~)
    %bscb  (hoon-to-plum p.spec)
    %bscl  :-  %sbrk
           :+  %tree
             [`[' ' `['[' ']']] `['$:' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bscn  (subtree (varying '$%' '==') (turn `(list ^spec)`+.spec ..$))
    %bsdt  (core-spec-to-plum '$.' p.spec q.spec)
    %bsld  (subtree (fixed '$<') $(spec p.spec) $(spec q.spec) ~)
    %bsbn  (subtree (fixed '$>') $(spec p.spec) $(spec q.spec) ~)
    %bshp  (subtree (fixed '$-') $(spec p.spec) $(spec q.spec) ~)
    %bskt  (subtree (fixed '$-') $(spec p.spec) $(spec q.spec) ~)
    %bsls  (subtree (fixed '$+') (standard p.spec) $(spec q.spec) ~)
    %bsnt  (core-spec-to-plum '$/' p.spec q.spec)
    %bsmc  (subtree (fixed '$;') (hoon-to-plum p.spec) ~)
    %bspd  (subtree (fixed '$&') $(spec p.spec) (hoon-to-plum q.spec) ~)
    %bssg  (subtree (fixed '$~') (hoon-to-plum p.spec) $(spec q.spec) ~)
    %bstc  (core-spec-to-plum '$`' p.spec q.spec)
    %bsts  :-  %sbrk
           :+  %tree
             [`['=' ~] `['$=' ~]]
           :~  (skin-to-plum p.spec)
               $(spec q.spec)
           ==
    %bsvt  (subtree (fixed '$@') $(spec p.spec) $(spec q.spec) ~)
    %bswt  :-  %sbrk
           :+  %tree
              [`[' ' `['?(' ')']] `['$?' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bszp  (core-spec-to-plum '$.' p.spec q.spec)
  ==
::
::  TODO What is this and why?
::
++  plume
  |_  =plum
  ::
  ::  An indented line.
  ::
  +$  line  [indent=@ud text=tape]
  ::
  ::  An sequence of indented lines.
  ::
  +$  block  (list line)
  ::
  ::  +flat: print as a single line
  ::
  ++  flat
    text:linear
  ::
  ::  +tall: print as multiple lines
  ::
  ++  tall
    ^-  wain
    %+  turn  window
    |=  line
    (crip (runt [indent ' '] text))
  ::
  ::  +adjust: adjust lines to right
  ::
  ++  adjust
    |=  [tab=@ud =block]
    ^-  ^block
    (turn block |=([@ud tape] [(add tab +<-) +<+]))
  ::
  ::  +window: print as list of tabbed lines
  ::
  ++  prepend-spaces
    |=  [n=@ =tape]
    (runt [n ' '] tape)
  ++  window
    ^-  block
    ~+                                                  ::  for random access
    ?@  plum  [0 (trip plum)]~                          ::  trivial text
    ?-  -.plum
      ::
      ::  %para: Line-wrappable paragraph. This is a stub; it should
      ::  wrap text to 40 characters.
      ::
      %para
        [0 +:linear]~
      ::
      ::  %sbrk: nested subexpression
      ::
      ::  This is an opportunity to switch to wide mode. First, try
      ::  rendered in wide mode. If that's possible and the result
      ::  isn't too big, use that. Otherwise recurse into the subplum
      ::  without switching to wide mode.
      ::
      %sbrk
        =/  sub  kid.plum
        ?+    sub
            window(plum sub)
          [%tree *]
            =/  wideresult
              ?~(wide.fmt.sub ~ [~ u=linear])
            ?:  ?&(?=(^ wideresult) (lte length.u.wideresult 40))
              [0 text.u.wideresult]~
            window(plum sub)
        ==
      ::
      ::  %tree: Try to render a text tree in tall mode.
      ::
      ::  We want to render this in tall mode. First, verify that there
      ::  the plum has a tall render (if not, fall back to `linear`
      ::  formatting), then render all the subplums, and then render
      ::  them in one of three ways:
      ::
      ::  - If the `plumfmt` contains an `indef` and that indef has
      ::    no prefix, then this is variable-arity rune with a terminator:
      ::    Use vertical formatting.
      ::
      ::  - If the `plumfmt` contains an `indef` and that indef DOES have
      ::    a prefix, then this is something that looks like a core: Use
      ::    `core-like` formatting.
      ::
      ::  - Otherwise, this is a rune with a fixed number of arguments
      ::    Render the subplums using backstop indentation.
      ::
      ::  There's also a special case where something has exactly one sub-plum.
      ::  where something has exactly one sub-block. For example, we
      ::  want this output:
      ::
      ::      |-
      ::      foo
      ::
      %tree
        ?~  tall.fmt.plum  [0 text:linear]~
        =/  prelude  (trip intro.u.tall.fmt.plum)
        |^  =/  blocks   (turn kids.plum |=(=^plum window(plum plum)))
            =/  prelude  (trip intro.u.tall.fmt.plum)
            ?~  indef.u.tall.fmt.plum
              ?:  =(1 (lent blocks))
                [[0 prelude] (zing blocks)]
              (backstep prelude blocks)
            =/  prefix  (trip sigil.u.indef.u.tall.fmt.plum)
            =/  finale  (trip final.u.indef.u.tall.fmt.plum)
            ?~  blocks  %+  weld
                          ?~(prelude ~ [0 prelude]~)
                        ?~(finale ~ [0 finale]~)
            ?~  prefix  (running prelude blocks finale)
            (core-like prelude prefix blocks finale)
        --
    ==
    ::
    ::  Render a plum in tall-mode using backstop indentation. Here,
    ::  we are rendering things that look something like this:
    ::
    ::      :+  foo
    ::        bar
    ::      baz
    ::
    ++  backstep
      |=  [prelude=tape blocks=(list block)]
      ^-  block
      %-  zing
      =/  nkids  (lent blocks)
      =/  idx  1
      |-  ^-  (list block)
      ?~  blocks  ~
      :_  $(blocks t.blocks, idx +(idx))
      ^-  block
      =/  indent  (mul 2 (sub nkids idx))
      ?.  =(1 idx)  (adjust indent i.blocks)
      (rune-inline-with-block prelude indent i.blocks)
    ::
    ::  To make things look a bit nicer, we want to put the first
    ::  sub-block on the same line as the rune. We want this:
    ::
    ::      :-  foo
    ::      baz
    ::
    ::  Instead of this:
    ::
    ::      :-
    ::          foo
    ::      baz
    ::
    ::  This handles the "foo" case.
    ::
    ++  rune-inline-with-block
      |=  [rune=tape indent=@ blk=block]
      ^-  block
      =.  indent  (max indent (add 2 (lent rune)))
      =.  blk     (adjust indent blk)
      ?~  rune  blk
      ?~  blk   [0 rune]~
      :_  t.blk
      :-  0
      %+  weld  rune
      =/  spaces-btwn  (sub indent.i.blk (lent rune))
      (prepend-spaces spaces-btwn text.i.blk)
    ::
    ::  Render a tall hoon with running indentation. Here, we are
    ::  rendering things that look sopmething like:
    ::
    ::      :~  foo
    ::          bar
    ::          baz
    ::      ==
    ::
    ::  So, there's basically three cases here: Either the prelude
    ::  is a rune, the prelude is empty, or prelude is some other
    ::  random-ass thing.
    ::
    ::  - If there is no prelude, then just combine all of the
    ::    sub-blocks together unaltered.
    ::  - If it's a rune (two-chatacters wide), then combine the
    ::    rune and the first line into one line (separated by two
    ::    spaces) and indent the rest of the lines by four spaces.
    ::  - If the rune is some other random-ass thing (has a length
    ::    that isn't 0 or 2), then render the prelude alone on the
    ::    first line and then combine the sub-blocks together,
    ::    all indented by another two spaces.
    ::
    ::  Regardless, if there's a finale, stick it on the end without
    ::  any indentation.
    ::
    ++  running
      |=  [prelude=tape blocks=(list block) finale=tape]
      ^-  block
      =/  result=block  (zing blocks)
      =.  result
        ?+    (lent prelude)
            [[0 prelude] (adjust 2 result)]         ::  unusual prelude
          %0                                        ::  empty prelude
            result
          %2                                        ::  rune prelude
            (rune-inline-with-block prelude 4 result)
        ==
      ?~  finale  result
      (snoc result [0 finale])
    ::
    ::  This renders sub-blocks where each sub-block needs to be
    ::  prefixed by some tape. For example:
    ::
    ::      |%
    ::      ++  foo
    ::        bar
    ::      ++  baz
    ::        qux
    ::      --
    ::
    ++  core-like
      |=  [prelude=tape prefix=tape blocks=(list block) finale=tape]
      ^-  block
      =/  clear  (add 2 (lent prefix))
      =/  result
        ^-  block
        %-  zing
        ^-  (list block)
        %+  turn  blocks
        |=  blk=block
        ^-  block
        ^+  +<
        =*  tab  ?~(blk 0 (sub clear (min clear indent.i.blk)))
        =.  blk  (adjust tab blk)
        ?~  blk  ~
        :_  t.blk
        :-  0
        %+  weld  prefix
        (runt [(sub indent.i.blk (lent prefix)) ' '] text.i.blk)
      =.  result
        ?~  finale  result
        (snoc result [0 finale])
      ?~  prelude  result
      [[0 prelude] result]
  ::
  ::  +linear: Render a plum onto a single line, even if it only has a
  ::  wide form.
  ::
  ++  linear
    ^-  [length=@ud text=tape]
    ~+                                                  ::  ~+ for random access
    ?@  plum  [(met 3 plum) (trip plum)]                ::  Just a cord.
    ?-  -.plum
      ::
      ::  This is already in wide mode, so %sbrk nodes don't matter here.
      ::
      %sbrk
        linear(plum kid.plum)
      ::
      ::  %para: To write a wrappable text paragraph to a single line,
      ::  we just combine all the lines into one, interspersing single
      ::  spaces chars.
      ::
      %para
        |-  ^-  [length=@ud text=tape]
        ?~  lines.plum  [0 ~]
        =/  next  $(lines.plum t.lines.plum)
        =/  this  [length=(met 3 i.lines.plum) text=(trip i.lines.plum)]
        :-  (add +(length.this) length.next)
        (weld text.this `tape`['╳' text.next])

      ::
      ::  Render a text tree to a single line.
      ::
      %tree
        |^  ^-  [length=@ud text=tape]
            ?~  wide.fmt.plum  (force-wide window)
            =/  body  (render-body delimit.u.wide.fmt.plum kids.plum)
            ?~  enclose.u.wide.fmt.plum  body
            (wrap-with-enclose u.enclose.u.wide.fmt.plum body)
        ::
        ::  Given a list of subplums and a delimiter, render all the
        ::  subplums onto a single line, and combine them into a single
        ::  string by interspersing the delimiter.
        ::
        ++  render-body
           |=  [delimit=cord kids=(list ^plum)]
           =/  stop  (trip delimit)
           |-  ^-  [length=@ud text=tape]
           ?~  kids  [0 ~]
           =/  next  $(kids t.kids)
           =/  this  linear(plum i.kids)
           ?~  text.next  this
           :-  :(add length.this (lent stop) length.next)
           :(weld text.this stop text.next)
        ::
        ::  Wrap a wide-form-rendered result with the `enclose`  cords
        ::  from it's `plumefmt`.
        ::
        ++  wrap-with-enclose
          |=  [clamps=(pair cord cord) body=[length=@ text=tape]]
          ^-  [length=@ud text=tape]
          ::
          =/  close  [(trip -.clamps) (trip +.clamps)]
          :-  :(add length.body (lent -.close) (lent +.close))
          :(weld -.close text.body +.close)
        ::
        ::  Given the result of rendering a plum in tall form, combine
        ::  all the lines into one by separating each by two spaces.
        ::
        ++  force-wide
          |=  render=(list [@ud text=tape])
          ^-  [length=@ud text=tape]
          ::
          ?~  render  [0 ~]
          =/  next  (force-wide t.render)
          :-  :(add (lent text.i.render) 2 length.next)
          ?~(text.next text.i.render :(weld text.i.render "╠╣" text.next))
        --
    ==
  --
--
