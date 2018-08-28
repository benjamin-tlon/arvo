::  Hoon printer
::
::::  /hoon/hoon-printer/gen
  ::
/?    310
!:
::
::::
  ::
:-  %say
|=  {^ {{demo=hoon ~} ~}}
:-  %txt
^-  wain
=<  =/  ploom=ploom  (hoon-to-ploom demo)
    ~(tall ploomer ploom)
:: |=  {^ {{demo=type ~} ~}}
:: :-  %txt
:: ^-  wain
:: =<  =/  ploom=ploom  (hoon-to-ploom (type-to-hoon demo))
::     ~(tall ploome ploom)
|%
::
::  A `ploom` is the intermediate representation of the pretty-printer. It
::  encodes hoon-like data with the least amount of structured needed
::  for formating.
::
::  A `ploom` is either a
::
::  - `cord`: A simple cord
::  - `[%para *]`: A wrappable paragraph.
::  - `[%tree *]`: A formated ploom tree
::  - `[%sbrk *]`: An indication of a nested subexpression.
::
::  The formatter will use the tall mode unless:
::
::    - A ploom has only a `wide` style.
::    - The ploom is in `%sbrk` form and it's subploom, when formatted in
::      wide mode, can fit on a single line.
::
+$  ploom
  $@  cord
  $%  [%para prefix=tile lines=(list @t)]
      [%tree fmt=ploomfmt nested=(list ploom)]
      [%sbrk subploom=ploom]
  ==
::
::  A `ploomfmt` is a description of how to format a `ploom`. A `ploomfmt`
::  must include a `wide`, a `tall`, or both.
::
::  A `wide` is a description of how to format a ploom in a single
::  line. The `nested` sub-plooms will be interleaved with `delimit`
::  strings, and, if `enclose` is set, then the output will be enclosed
::  with `p.u.enclose` abnd `q.u.enclose`.
::
::  For examle, to build a ploomfmt for string literals, we could write:
::
::      [wide=[~ '' [~ '"' '"']] tall=~]
::
::  A `tall` is a description of how to format a ploom accross multiple
::  lines. The output will be prefixed by `intro`, suffixed by
::  `final.u.indef`, and each subploom prefixed by `sigil.u.indef`.
::
::  For example, to build a ploomfmt for cores, we could write:
::
::      [wide=~ tall=`['' `['++' '--']]]
::
+$  ploomfmt
  $:  wide=(unit [delimit=tile enclose=(unit (pair tile tile))])
      tall=(unit [intro=tile indef=(unit [sigil=tile final=tile])])
  ==
::
++  axis-to-cord
  |=  p=@
  ^-  cord
  ?:  =(p 1)  '.'
  ?:  =(p 2)  '-'
  ?:  =(p 3)  '+'
  (cat 3 '+' (scot %ud p))
::
++  limb-to-ploom
  |=  =limb
  ^-  ploom
  ?@  limb
    ?:  .=('' limb)  '$'
      limb
  ?-  -.limb
    %&  (axis-to-cord p.limb)
    %|  (crip (runt [0 p.limb] ?~(q.limb "," (trip u.q.limb))))
  ==
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
++  wing-to-ploom
  |=  =wing
  ^-  ploom
  :+  %tree
    [wide=`['.' ~] tall=~]
  (turn `^wing`wing limb-to-ploom)
::
++  battery-spec-to-ploom-list
  |=  =(map term spec)
  %+  turn  ~(tap by map)
  |=  [=term =spec]
  :-  %sbrk
  :+  %tree
    [wide=~ tall=`['' ~]]
  [term (spec-to-ploom spec) ~]
::
++  core-spec-to-ploom
  |=  [=knot =spec =(map term spec)]
  ^-  ploom
  :-  %sbrk
  :+  %tree
    [~ `[knot ~]]
  :~  (spec-to-ploom spec)
      :+  %tree
        [~ tall=`['' `['++' '--']]]
      (battery-spec-to-ploom-list map)
  ==
::
++  varying
  |=  [intro=knot final=knot]
  [`[' ' `[(cat 3 intro '(') ')']] `[intro `['' final]]]
::
++  fixed
  |=  @ta
  [`[' ' `[(cat 3 +< '(') ')']] `[+< ~]]
::
++  standard
  |=  =stud
  ^-  ploom
  ?@  stud  stud
  :+  %tree
    [wide=`['/' ~] tall=~]
  `(list ploom)`[auth.stud type.stud]
::
++  limb-ploom
  |=  =limb
  ^-  ploom
  ?^  limb  %todo-wtf-is-this  limb
::
++  tall-fixed
  |=  rune=cord
  ^-  (unit [cord (unit [cord cord])])
  [~ rune [~ '' '']]
::
++  tall-running
  |=  [rune=cord sigil=cord term=cord]
  ^-  (unit [cord (unit [cord cord])])
  [~ rune [~ sigil (cat 3 ' ' term)]]
::
++  woof-ploom
  |=  =woof
  ^-  ploom
  ?@  woof
    woof
  :+  %tree
    :-  wide=`[' ' `['{' '}']]  tall=~
  (turn (unwrap-woof-tuple +.woof) hoon-to-ploom)
::
++  unwrap-woof-tuple
  |=  =hoon
  ^-  (list ^hoon)
  ?:  ?=([%cltr *] hoon)
    p.hoon
  ~[hoon]
::
++  hoons-to-ploom-list
  |=  =hoon=(list hoon)
  ^.  (list ploom)
  (turn hoon-list hoon-to-ploom)
::
++  raw-tall-ploom
  |=  nested=(list ploom)
  ^.  ploom
  tree/[[wide=~ tall=[~ '' ~]] nested]
::
++  rune-short-form
  |=  [rune=cord short=(unit [cord cord])]
  ^-  (unit (pair cord (unit [cord cord])))
  :+  ~  ' '
  :-  ~
  ?^  short   u.short
  [(cat 3 rune '(') ')']
::
++  rune-to-ploom
  |=  [rune=cord term=(unit cord) short=(unit [cord cord]) nested=(list ploom)]
  ^.  ploom
  :-  %sbrk
  :+  %tree
    :-  (rune-short-form rune short)
    ?~  term  (tall-fixed rune)
    (tall-running rune '' u.term)
  nested
::
++  chum-to-ploom
  |=  =chum
  ^-  ploom
  %todo-chum
::
++  tyre-to-ploom
  |=  =tyre
  ^-  ploom
  %todo-tyre
::
++  matches-to-ploom-list
  |=  =update=(list (pair spec hoon))
  ^-  (list ploom)
  %-  add-trailing-commas-to-wide-form
  %+  turn  update-list
  |=  [=spec =hoon]
  ^-  (pair ploom ploom)
  [(spec-to-ploom spec) (hoon-to-ploom hoon)]
::
++  updates-to-ploom-list
  |=  =update=(list (pair wing hoon))
  ^-  (list ploom)
  %-  add-trailing-commas-to-wide-form
  %+  turn  update-list
  |=  [=wing =hoon]
  ^-  (pair ploom ploom)
  [(wing-to-ploom wing) (hoon-to-ploom hoon)]
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
::  to be sure that this is formatted in wide mode if-and-only-if our
::  parent is formatted in wide mode.
::
++  add-trailing-commas-to-wide-form
  |=  plooms=(list (pair ploom ploom))
  =|  acc=(list (list ploom))
  |^  ^-  (list ploom)
    ?~  plooms  (zing (flop acc))
    =/  x=ploom  p.i.plooms
    =/  y=ploom  q.i.plooms
    ?~  t.plooms
      $(plooms t.plooms, acc [~[x y] acc])
    $(plooms t.plooms, acc [~[x (comma y)] acc])
  ++  comma
    |=  =sub=ploom
    ^-  ploom
    :+  %tree
      :-  [~ '' [~ '' ',']]  [~ '' ~]
    ~[sub-ploom]
  --
::
::  Just a helper function for constructing a wide-form %tree plum.
::
++  simple-wide-ploom
  |=  [init=cord sep=cord end=cord babes=(list ploom)]
  ^-  ploom
  :+  %tree  [wide=[~ sep [~ init end]] tall=~]  babes
::
::  XX Add special wide forms for:
::
::    - `$(a b)`
::    - `!x`
::    - `:x`
::
++  hoon-to-ploom
  |=  x=hoon
  |^  ^-  ploom
    ?+    x
        %autocons
      [%$ @]     (axis-to-cord p.x)
      [%base *]  (spec [%base p.x])
      [%bust *]  (simple-wide-ploom '*' '' '' (spec [%base p.x]) ~)
      [%dbug *]  (hn q.x)                               ::  p.x is irrelevant
      [%eror *]  %assembly-error
      [%hand *]  %ast-node-hand
      [%note *]  (hn q.x)                               ::  p.x is irrelevant
      [%fits *]  %ast-node-fits
      [%knit *]  (simple-wide-ploom '"' '' '"' (turn p.x woof-ploom))
      [%leaf *]  (spec x)
      [%limb *]  p.x
      [%lost *]  (hn p.x)                               ::  for internal use
      [%rock *]  ?^  q.x  !!  (cat 3 '%' (crip (scow p.x `@`q.x)))
      [%sand *]  ?^  q.x  !!  (crip (scow p.x `@`q.x))
      [%tell *]  (simple-wide-ploom '<' ' ' '>' (hoons p.x))
      [%tune *]  ?@(p.x p.x %todo-tune)
      [%wing *]  (simple-wide-ploom '' '.' '' (turn p.x limb-to-ploom))
      [%yell *]  (simple-wide-ploom '>' ' ' '<' (hoons p.x))
      [%xray *]  (xray-to-ploom p.x)
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
      [%clhp *]  (rune ':-' ~ `['[' ']'] (hoons ~[p q]:x))
      [%clls *]  (rune ':+' ~ `['[' ']'] (hoons ~[p q r]:x))
      [%clsg *]  (rune ':~' ~ `['~[' ']'] (hoons p.x))
      [%cltr *]  ?~  p.x    '~'
                 ?~  +.p.x  (hn -.p.x)
                 (rune ':*' `['=='] `['[' ']'] (hoons p.x))
      [%cncb *]  (rune '%_' `['=='] ~ (wing p.x) (updates q.x))
      [%cndt *]  (rune '%.' ~ ~ (hoons ~[p q]:x))
      [%cnhp *]  (rune '%-' ~ `['(' ')'] (hoons ~[p q]:x))
      [%cncl *]  (rune '%:' `['=='] `['(' ')'] (hoons [p q]:x))
      [%cntr *]  (rune '%*' `['=='] ~ (wing p.x) (hn q.x) (updates r.x))
      [%cnkt *]  (rune '%^' ~ ~ (hoons ~[p q r s]:x))
      [%cnls *]  (rune '%+' ~ ~ (hoons ~[p q r]:x))
      [%cnsg *]  (rune '%~' `['=='] `['~(' ')'] (wing p.x) (hoons [q r]:x))
      [%cnts *]  ?~  q.x  (wing p.x)
                 (rune '%=' `['=='] ~ (wing p.x) (updates q.x))
      [%dtkt *]  (rune '.^' ~ ~ (spec p.x) (hn q.x) ~)
      [%dtls *]  (rune '.+' ~ `['+(' ')'] (hoons ~[p]:x))
      [%dttr *]  (rune '.*' ~ ~ (hoons ~[p q]:x))
      [%dtts *]  (rune '.=' ~ `['=(' ')'] (hoons ~[p q]:x))
      [%dtwt *]  (rune '.?' ~ ~ (hoons ~[p.x]))
      [%ktbr *]  (rune '^|' ~ ~ (hoons ~[p.x]))
      [%ktcn *]  (rune '^%' ~ ~ (hoons ~[p]:x))
      [%ktdt *]  (rune '^.' ~ ~ (hoons ~[p q]:x))
      [%ktls *]  (rune '^+' ~ ~ (hoons ~[p q]:x))
      [%kthp *]  (rune '^-' ~ ~ ~[(spec p.x) (hn q.x)])
      [%ktpd *]  (rune '^&' ~ ~ (hoons ~[p]:x))
      [%ktsg *]  (rune '^~' ~ ~ (hoons ~[p]:x))
      [%ktts *]  (rune '^=' ~ ~ ~[(skin p.x) (hn q.x)])
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
      [%mccl *]  (rune ';:' `['=='] ~ (hoons [p q]:x))
      [%mcnt *]  (rune ';/' ~ ~ (hoons ~[p]:x))
      [%mcsg *]  (rune ';~' `['=='] ~ (hoons [p q]:x))
      [%mcmc *]  (rune ';;' ~ ~ (hoons ~[p q]:x))
      [%tsbr *]  (rune ';;' ~ ~ ~[(spec p.x) (hn q.x)])
      [%tscl *]  (tiscol-to-ploom p.x q.x)
      [%tsnt *]  (rune '=/' ~ ~ (skin p.x) (hn q.x) (hn r.x) ~)
      [%tsmc *]  (rune '=;' ~ ~ [(skin p.x) (hoons ~[q r]:x)])
      [%tsdt *]  (rune '=.' ~ ~ [(wing p.x) (hoons ~[q r]:x)])
      [%tswt *]  (rune '=?' ~ ~ [(wing p.x) (hoons ~[q r s]:x)])
      [%tsld *]  (rune '=>' ~ ~ (hoons ~[p q]:x))
      [%tshp *]  (rune '=-' ~ ~ (hoons ~[p q]:x))
      [%tsbn *]  (rune '=<' ~ ~ (hoons ~[p q]:x))
      [%tskt *]  (rune '=^' ~ ~ [(skin p.x) (wing q.x) (hoons ~[r s]:x)])
      [%tsls *]  (rune '=+' ~ ~ (hoons ~[p q]:x))
      [%tssg *]  (rune '=~' `['=='] ~ (hoons p:x))
      [%tstr *]  ?~  q.p.x
                   (rune '=*' ~ ~ p.p.x (hoons ~[q r]:x))
                 (rune '=*' ~ ~ (spec [%bsts p.p.x u.q.p.x]) (hoons ~[q r]:x))
      [%tscm *]  (rune '=,' ~ ~ (hoons ~[p q]:x))
      [%wtbr *]  (rune '?|' `['--'] ~ (hoons p:x))
      [%wthp *]  (rune '?-' `['=='] ~ (wing p.x) (matches q.x))
      [%wtcl *]  (rune '?:' ~ ~ (hoons ~[p q r]:x))
      [%wtdt *]  (rune '?.' ~ ~ (hoons ~[p q r]:x))
      [%wtkt *]  (rune '?^' ~ ~ [(wing p.x) (hoons ~[q r]:x)])
      [%wtld *]  (rune '?<' ~ ~ (hoons ~[p q]:x))
      [%wtbn *]  (rune '?>' ~ ~ (hoons ~[p q]:x))
      [%wtls *]  (rune '?+' `['=='] ~ (wing p.x) (hn q.x) (matches r.x))
      [%wtpd *]  (rune '?&' `['=='] ~ (hoons p:x))
      [%wtvt *]  (rune '?@' ~ ~ (wing p.x) (hoons ~[q r]:x))
      [%wtsg *]  (rune '?~' ~ ~ (wing p.x) (hoons ~[q r]:x))
      [%wthx *]  (rune '?#' ~ ~ (skin p.x) (wing q.x) ~)
      [%wtts *]  (rune '?=' ~ ~ (spec p.x) (wing q.x) ~)
      [%wtzp *]  (rune '?!' ~ ~ (hoons ~[p]:x))
      [%zpcm *]  (rune '!,' ~ ~ (hoons ~[p q]:x))
      [%zpbn *]  (rune '!>' ~ ~ (hoons ~[p]:x))
      [%zpmc *]  (rune '!;' ~ ~ (hoons ~[p q]:x))
      [%zpts *]  (rune '!=' ~ ~ (hoons ~[p]:x))
      [%zpvt *]  (rune '!@' ~ ~ (wingseq p.x) (hoons ~[q r]:x))
      [%zpwt *]  (hn q.x)                               ::  Ignore p.x
      [%zpzp ~]  '!!'
    ==
    ++  hn         hoon-to-ploom
    ++  battery    battery-to-ploom-list
    ++  chapter    chapters-to-ploom
    ++  chum       chum-to-ploom
    ++  hint       hint-to-ploom
    ++  hoons      hoons-to-ploom-list
    ++  matches    matches-to-ploom-list
    ++  rune       rune-to-ploom
    ++  skin       skin-to-ploom
    ++  spec       spec-to-ploom
    ++  tyre       tyre-to-ploom
    ++  updates    updates-to-ploom-list
    ++  wing       wing-to-ploom
    ++  wingseq    wingseq-to-ploom
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
    ++  tiscol-to-ploom
      |=  [updates=(list [^wing hoon]) body=hoon]
      ^-  ploom
      =/  rem=(list (pair ^wing hoon))  updates         ::  Note [TisCol Order]
      =/  acc=hoon  body
      %-  hoon-to-ploom
      |-  ^-  hoon
      ?~  rem  acc
      $(rem t.rem, acc `hoon`[%tsdt `^wing`p.i.rem `hoon`q.i.rem `hoon`acc])
  --
::
++  hint-to-ploom
  |=  hint=$@(term (pair term hoon))
  ^-  ploom
  ?@  hint  (cat 3 '%' hint)
  :+  %tree
    [wide=`['.' ~] tall=~]
  :~  (cat 3 '%' p.hint)
      (hoon-to-ploom q.hint)
  ==
::
++  battery-to-ploom-list
  |=  =(map term hoon)
  ^-  (list ploom)
  %+  turn  ~(tap by map)
  |=  [=term =hoon]
  :+  %tree
    [wide=~ tall=`['' ~]]
  [term (hoon-to-ploom hoon) ~]
::
++  core-to-ploom
  |=  [=knot head=(unit ploom) =(map term hoon)]
  ^-  ploom
  :-  %sbrk
  :+  %tree
    [~ `[knot ~]]
  =/  nested=ploom
    :+  %tree
      [~ `['' `[' ++' ' --']]]                        ::  Note [Plum Spaces]
    (battery-to-ploom-list map)
  ?~  head  ~[nested]
  ~[u.head nested]
::
::  Note [Plum Spaces]
::  ~~~~~~~~~~~~~~~~~~
::
::  XX I don't understand why the space prefixes on ++ and -- are
::  necessary to get valid output. The ploom printer manages to output
::  double spaces in other situations, but in this situation it only
::  outputs one space.
::
::  With the space prefixes (' ++' and ' --'):
::
::      > +hoon-printer !,  *hoon  |_  x=*  ++  y  3  ++  z  5  ++  g  4  --
::      <||_  x=*  ++  z    5  ++  y    3  ++  g    4  --|>
::
::  With ('++' and '--') instead:
::
::      > +hoon-printer !,  *hoon  |_  x=*  ++  y  3  ++  z  5  ++  g  4  --
::      <||_  x=* ++  z    5  ++  y    3  ++  g    4 --|>
::
++  chapters-to-ploom
  |=  [=knot head=(unit ploom) =(map term tome)]
  ^-  ploom
  =/  chapters=(list (pair term tome))  ~(tap by map)
  ?~  chapters  (chapters-to-ploom-verbose knot head map)
  ?~  t.chapters
    ?:  .=('' p.i.chapters)
      (core-to-ploom knot head q.q.i.chapters)
    (chapters-to-ploom-verbose knot head map)
  (chapters-to-ploom-verbose knot head map)
::
++  chapters-to-ploom-verbose
  |=  [=knot head=(unit ploom) =(map term tome)]
  ^-  ploom
  =/  chaps=(list (pair term tome))  ~(tap by map)
  :+  %tree
    [~ `[knot `['' ' --']]]                             ::  Note [Plum Spaces]
  =/  nested=(list ploom)
    %+  turn  chaps
    chapter-to-ploom
  ?~  head  nested
  [u.head nested]
::
++  chapter-to-ploom
  |=  [nm=knot [* bat=(map term hoon)]]
  ^-  ploom
  :+  %tree
    [~ `['+|' ~]]
  :~  (cat 3 '%' nm)
      :+  %tree
        [~ `['' `['++' '']]]
      (battery-to-ploom-list bat)
  ==
::
++  chapters-to-ploom-list
  |=  =(map term tome)
  ^-  (list ploom)
  %+  turn  ~(tap by map)
  |=  [=term [* hoons=(^map term hoon)]]
  ^-  ploom
  ?:  =(term '')
    (raw-tall-ploom (battery-to-ploom-list hoons))
  (rune-to-ploom '+|' ~ ~ [(cat 3 '%' term) (battery-to-ploom-list hoons)])
::
++  xray-to-ploom
  |=  =manx:hoot
  ^-  ploom
  %ast-node-xray                                        ::  XX Punt
::
++  skin-to-ploom
  |=  =skin
  ^-  ploom
  ?@  skin  skin
  %todo-complex-skin                                    ::  XX Punt
::
++  wingseq-to-ploom
  |=  =(list wing)
  ^-  ploom
  :+  %tree
    [wide=`[':' ~] tall=~]
  (turn list wing-to-ploom)
::
++  subtree
  |=  [p=ploomfmt q=(list ploom)]
  ^-  ploom
  [%sbrk [%tree p q]]
::
++  spec-to-ploom
  |=  =spec
  ^-  ploom
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
    %like  tree/[[`[':' ~] ~] (turn `(list wing)`+.spec wing-to-ploom)]
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
           [(hoon-to-ploom p.spec) (turn q.spec ..$)]
    %bsbs  (core-spec-to-ploom '$$' p.spec q.spec)
    %bsbr  (subtree (fixed '$|') $(spec p.spec) (hoon-to-ploom q.spec) ~)
    %bscb  (hoon-to-ploom p.spec)
    %bscl  :-  %sbrk
           :+  %tree
             [`[' ' `['[' ']']] `['$:' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bscn  (subtree (varying '$%' '==') (turn `(list ^spec)`+.spec ..$))
    %bsdt  (core-spec-to-ploom '$.' p.spec q.spec)
    %bsld  (subtree (fixed '$<') $(spec p.spec) $(spec q.spec) ~)
    %bsbn  (subtree (fixed '$>') $(spec p.spec) $(spec q.spec) ~)
    %bshp  (subtree (fixed '$-') $(spec p.spec) $(spec q.spec) ~)
    %bskt  (subtree (fixed '$-') $(spec p.spec) $(spec q.spec) ~)
    %bsls  (subtree (fixed '$+') (standard p.spec) $(spec q.spec) ~)
    %bsnt  (core-spec-to-ploom '$/' p.spec q.spec)
    %bsmc  (subtree (fixed '$;') (hoon-to-ploom p.spec) ~)
    %bspd  (subtree (fixed '$&') $(spec p.spec) (hoon-to-ploom q.spec) ~)
    %bssg  (subtree (fixed '$~') (hoon-to-ploom p.spec) $(spec q.spec) ~)
    %bstc  (core-spec-to-ploom '$`' p.spec q.spec)
    %bsts  :-  %sbrk
           :+  %tree
             [`['=' ~] `['$=' ~]]
           :~  (skin-to-ploom p.spec)
               $(spec q.spec)
           ==
    %bsvt  (subtree (fixed '$@') $(spec p.spec) $(spec q.spec) ~)
    %bswt  :-  %sbrk
           :+  %tree
              [`[' ' `['?(' ')']] `['$?' `['' '==']]]
           (turn `(list ^spec)`+.spec ..$)
    %bszp  (core-spec-to-ploom '$.' p.spec q.spec)
  ==
::
++  ploomer
  |_  =ploom
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
    |=  [indent=@ud text=tape]
    (crip (runt [indent ' '] text))
  ::
  ::  +adjust: adjust lines to right
  ::
  ++  adjust
    |=  [tab=@ud =(list [length=@ud text=tape])]
    (turn list |=([@ud tape] [(add tab +<-) +<+]))
  ::
  ::  +window: print as list of tabbed lines
  ::
  ++  window
    ^-  (list [indent=@ud text=tape])
    ~+                                                  ::  for random access
    ?@  ploom  [0 (trip ploom)]~                        ::  trivial text
    ?-  -.ploom
      ::
      ::  %para: Line-wrappable paragraph. This is a stub; it should
      ::  wrap text to 40 characters.
      ::
      %para  [0 +:linear]~
      ::
      ::  %sbrk: nested subexpression
      ::
      ::  This is an opportunity to switch to wide mode. First, try
      ::  formatting in wide mode. If that's possible and the result
      ::  isn't too big, use that. Otherwise recurse into the subploom
      ::  without switching to wide mode.
      ::
      %sbrk
          =/  sub  subploom.ploom
          ?+    sub
              window(ploom sub)
            [%tree *]
              =/  wideresult
                ?~(wide.fmt.sub ~ [~ u=linear])
              ?:  ?&(?=(^ wideresult) (lte length.u.wideresult 40))
                [0 text.u.wideresult]~
                :: [0 (zing ~["█" text.u.wideresult "█"])]~
              window(ploom sub)
          ==
      ::
      ::  %tree: text tree
      ::
      ::  We are formatting in tall-mode here, but if `fmt.ploom` only
      ::  contains a wide form, use that. Otherwise, do the tall-mode
      ::  formatting.
      ::
      %tree
          ?:  ?&(?=(~ tall.fmt.ploom) ?=(^ wide.fmt.ploom))
            [0 text:linear]~
          ::
          ::  If there isn't a wide form, then there *must* be a tall
          ::  form. Assert that.
          ::
          ?>  ?=(^ tall.fmt.ploom)
          ::
          ::  First, render all of our subplooms recursivly.
          ::
          =/  blocks
            (turn nested.ploom |=(=^ploom window(ploom ploom)))
          ::
          =/  prelude  (trip intro.u.tall.fmt.ploom)
          ::
          ::  If `indef` isn't set in the plumfmt, then we will print
          ::  in sloping mode.
          ::
          ?~  indef.u.tall.fmt.ploom
            ?:  =(~ blocks)                             ::  no children
              ?~(prelude ~ [0 prelude]~)
            ^-  (list [indent=@ud text=tape])           ::  some children
            %-  zing
            =/  count  (lent blocks)                    ::  num children
            =/  index  1                                ::  child index
            |-  ^+  blocks
            ?~  blocks  ~
            :_  $(blocks t.blocks, index +(index))
            ^-  (list [indent=@ud text=tape])
            ::  indent: backstep indentation level
            ::
            =/  indent  (mul 2 (sub count index))
            ::  unless, we're on the first block
            ::
            ?.  =(1 index)
              ::  else, apply normal backstep indentation
              ::
              (adjust indent i.blocks)
            ::  then, apply and/or inject prelude
            ::
            ::    this fixes the naive representations
            ::
            ::      :+
            ::          foo
            ::        bar
            ::      baz
            ::
            ::    and
            ::
            ::      :-
            ::        foo
            ::      bar
            ::
            =.  indent  (max indent (add 2 (lent prelude)))
            =.  i.blocks  (adjust indent i.blocks)
            ?~  i.blocks  ?~(prelude ~ [0 prelude]~)
            ?~  prelude   i.blocks
            :_  t.i.blocks
            :-  0
            %+  weld  prelude
            (runt [(sub indent.i.i.blocks (lent prelude)) ' '] text.i.i.blocks)
          ::
          ::  else, print in vertical mode
          ::
          ::  prefix: before each entry
          ::  finale: after all entries
          ::
          =/  prefix  (trip sigil.u.indef.u.tall.fmt.ploom)
          =/  finale  (trip final.u.indef.u.tall.fmt.ploom)
          ::  if, no children, then, just prelude and finale
          ::
          ?:  =(~ blocks)
            %+  weld
              ?~(prelude ~ [0 prelude]~)
            ?~(finale ~ [0 finale]~)
          ::  if, no :prefix
          ::
          ?:  =(~ prefix)
            ::  kids: flat list of child lines
            ::  tab:  amount to indent kids
            ::
            =/  kids  `(list [indent=@ud text=tape])`(zing blocks)
            =*  tab   =+((lent prelude) ?+(- 2 %0 0, %1 2, %2 4))
            ::  indent kids by tab
            ::
            =.  kids  (turn kids |=([@ud tape] [(add tab +<-) +<+]))
            ::  prepend or inject prelude
            ::
            =.  kids
              ?:  =(~ prelude)  kids
              ::  if, no kids, or prelude doesn't fit
              ::
              ?:  |(?=(~ kids) (gte +((lent prelude)) indent.i.kids))
                ::  don't inject, just add to head if needed
                ::
                [[0 prelude] kids]
              ::  inject: prelude
              ::
              =*  inject  %+  weld  prelude
                          %+  runt  [(sub indent.i.kids (lent prelude)) ' ']
                          text.i.kids
              [[0 inject] t.kids]
            ::  append finale
            ::
            ?~  finale  kids
            (weld kids ^+(kids [0 finale]~))
          ::  else, with :prefix
          ::
          ::  append :finale
          ::
          =-  ?~  finale  -
              (weld - ^+(- [0 finale]~))
          ^-  (list [indent=@ud text=tape])
          ::  clear: clearance needed to miss prefix
          ::
          =/  clear  (add 2 (lent prefix))
          %-  zing
          ::  combine each subtree with the prefix
          ::
          %+  turn  blocks
          |=  =(list [indent=@ud text=tape])
          ^+  +<
          ::  tab: depth to indent
          ::
          =*  tab  ?~(list 0 (sub clear (min clear indent.i.list)))
          =.  list  (turn list |=([@ud tape] [(add tab +<-) +<+]))
          ?~  list  ~
          :_  t.list
          :-  0
          %+  weld
            prefix
          (runt [(sub indent.i.list (lent prefix)) ' '] text.i.list)
    ==
  ::
  ::  +linear: Format a plum onto a single line, even if it only has a
  ::  wide form.
  ::
  ++  linear
    ^-  [length=@ud text=tape]
    ~+                                                  ::  ~+ for random access
    ?@  ploom  [(met 3 ploom) (trip ploom)]             ::  Just a cord.
    ?-  -.ploom
      ::
      ::  This is already in wide mode, so %sbrk nodes don't matter here.
      ::
      %sbrk
        linear(ploom subploom.ploom)
      ::
      ::  %para: To write a wrappable text paragraph to a single line,
      ::  we just combine all the lines into one, interspersing single
      ::  spaces chars.
      ::
      %para
          |-  ^-  [length=@ud text=tape]
          ?~  lines.ploom  [0 ~]
          =/  next  $(lines.ploom t.lines.ploom)
          =/  this  [length=(met 3 i.lines.ploom) text=(trip i.lines.ploom)]
          :-  (add +(length.this) length.next)
          (weld text.this `tape`[' ' text.next])
      ::
      ::  %tree: text tree
      ::
      %tree
          ::
          ::  If there is no wide plumfmt, then just combine all
          ::  of the windows, separated by double-spaces.
          ::
          ?~  wide.fmt.ploom
            =/  window  window
            |-  ^-  [length=@ud text=tape]
            ?~  window  [0 ~]
            =/  next  $(window t.window)
            :-  :(add (lent text.i.window) 2 length.next)
            ?~(text.next text.i.window :(weld text.i.window "  " text.next))
          ::
          ::  If there *is* a wide plumfmt, then render the plum using that
          ::
          ::  Wrap the rendered body with `enclose.wide.fmt`.
          ::
          =-  ?~  enclose.u.wide.fmt.ploom  body
              =*  clamps  u.enclose.u.wide.fmt.ploom
              =/  close  [(trip -.clamps) (trip +.clamps)]
              :-  :(add length.body (lent -.close) (lent +.close))
              :(weld -.close text.body +.close)
          ::
          ::  Render all the subplums, and interleave `delimit.wide.fmt`
          ::  between each.
          ::
          ^=  body
          =/  stop  (trip delimit.u.wide.fmt.ploom)
          |-  ^-  [length=@ud text=tape]
          ?~  nested.ploom  [0 ~]
          =/  next  $(nested.ploom t.nested.ploom)
          =/  this  linear(ploom i.nested.ploom)
          ?~  text.next  this
          :-  :(add length.this (lent stop) length.next)
          :(weld text.this stop text.next)
    ==
  --
--
