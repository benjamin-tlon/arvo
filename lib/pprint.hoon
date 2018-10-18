/?  310
::
/-  *xray
/+  *xray, *plum
::
|^  ^-  $:  render-noun=$-([=ximage =noun] =plum)
            render-vase=$-(vase wain)
        ==
    [render-noun render-vase]
::
+|  %utils
::
+$  battery  (map term (pair what (map term hoon)))
::
::  Given a battery expression (from a hoon expression), produce a list
::  of arm names.
::
++  arm-names
  |=  =battery
  ^-  (list term)
  %-  zing
  %+  turn  ~(val by battery)
  |=  [=what arms=(map term hoon)]
  ^-  (list term)
  ~(tap in ~(key by arms))
::
::  This is basically a `mapM` over a list using the State monad.
::
::  Another way to think about this is that it is the same as `turn`,
::  except that a state variable `st` is threaded through the
::  execution. The list is processed from left to right.
::
++  traverse
   |*  [a=mold b=mold s=mold]
   |=  [[xs=(list a) st=s] f=$-([a s] [b s])]
   ^-  [(list b) s]
   ?~  xs  [~ st]
   =^  r   st  (f i.xs st)
   =^  rs  st  $(xs t.xs, st st)
   [[r rs] st]
::
::  Same as `traverse` but executes state updates in reverse order.
::
++  traverse-right
   |*  [a=mold b=mold s=mold]
   |=  [[xs=(list a) st=s] f=$-([a s] [b s])]
   ^-  [(list b) s]
   ?~  xs  [~ st]
   =^  rs  st  $(xs t.xs, st st)
   =^  r   st  (f i.xs st)
   [[r rs] st]
::
+|  %entry-points-for-testing
::
++  render-vase
  |=  =vase
  ^-  wain
  ~(tall plume (vase-to-plum vase))
::
::  Pretty-print a vase.
::
++  vase-to-plum
  |=  v=vase  ^-  plum
  (render-noun (xray-type 1 p.v) q.v)
::
++  render-type
  |=  =type  ^-  wain
  ~(tall plume (type-to-plum type))
::
::  Pretty-print a type given as a string.
::
++  render-type-from-cord
  |=  =cord  ^-  wain
  =/  t=type  -:(ride -:!>(..zuse) cord)
  ~(tall plume (type-to-plum t))
::
::  Pretty-print a type.
::
++  type-to-plum
  |=  t=type  ^-  plum
  (spec-to-plum (ximage-to-spec (xray-type 1 t)))
::
::  Pretty-print a hoon.
::
++  render-hoon
  |=  =hoon  ^-  wain
  ~(tall plume (hoon-to-plum 999 hoon))
::
::  This is just a helper function for testing out this code.  It just digs
::  through a type and finds hoon values referenced within that type,
::  and then renders the result.
::
++  render-all-hoons-inside-of-type
  |=  =type  ^-  wain
  ?.  ?=([%core *] type)  [%zpzp ~]
  =*  tomes=(list tome)  ~(val by q.r.q.type)
  =*  hoons=(list hoon)  (turn tomes |=(t=tome [%cltr ~(val by q.t)]))
  ~(tall plume (hoon-to-plum 999 [%cltr hoons]))
::
+|  %pretty-printer
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
::  '$') or an axis.
::
::  XX The code for handling the `%|` ("by name") case is obviously
::  wrong (the `runt` call does nothing, for example), but I'm not sure
::  what it was trying to do in the first place.
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
  ==
::
::  Render a wing
::
++  wing-to-plum
  |=  =wing
  ^-  plum
  :+  %tree
    [wide=`['.' ~] tall=~]
  (turn `^wing`wing limb-to-plum)
::
::  In the spec for a battery, there's a `(map term spec)`. This transforms one
::  of those into a list of plums, one per `term/spec` pair.
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
::  Given a rune and a spec for a core, transform that into a plum.
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
::  Convert a "standard name" into a plum.
::
++  stud-to-plum
  |=  =stud
  ^-  plum
  ?@  stud  stud
  :+  %tree
    [wide=`['/' ~] tall=~]
  `(list plum)`[auth.stud type.stud]
::
::  Convert a woof (an interpolated expression inside of a string literal)
::  to a plum.
::
++  woof-to-plum
  |=  =woof
  ^-  plum
  |^  ?@  woof  woof
      =*  fmt  [wide=`[' ' `['{' '}']] tall=~]
      :+  %tree  fmt
      (turn (unwrap-woof-tuple +.woof) |=(h=hoon (hoon-to-plum 999 h)))
  ::
  ::  Woofs contain one or more hoons, and if there are more than one,
  ::  it's encoded with a %cltr ast node. This just simplifies both
  ::  cases down into a list of subhoons.
  ::
  ++  unwrap-woof-tuple
    |=  =hoon
    ^-  (list ^hoon)
    ?:  ?=([%cltr *] hoon)
      p.hoon
    ~[hoon]
  --
::
::  This is just a trivial helper function. It's only here because this
::  pattern is used repeatedly in `hoon-to-plum`.
::
++  hoons-to-plum-list
  |=  =hoon=(list hoon)
  ^.  (list plum)
  (turn hoon-list |=(h=hoon (hoon-to-plum 999 h)))
::
::  XX Placeholder for rendering a chum to a plum.
::
++  chum-to-plum
  |=  =chum
  ^-  plum
  %todo-chum
::
::  XX Placeholder for rendering a tyre to a plum
::
++  tyre-to-plum
  |=  =tyre
  ^-  plum
  %todo-tyre
::
::  Generate a list of plums from a list of matches. This would be
::  trivial, but we also need to append commas on each match (besides
::  the last) when `matches` is rendered in wide mode.
::
++  matches-to-plum-list
  |=  matches=(list (pair spec hoon))
  ^-  (list plum)
  %-  add-trailing-commas-to-wide-form
  %+  turn  matches
  |=  [=spec =hoon]
  ^-  (pair plum plum)
  [(spec-to-plum spec) (hoon-to-plum 999 hoon)]
::
::  Generate a list of plums from a list of updates. This would be
::  trivial, but we also need to append commas on each update (besides
::  the last) when the update-list is rendered in wide mode.
::
++  updates-to-plum-list
  |=  =update=(list (pair wing hoon))
  ^-  (list plum)
  %-  add-trailing-commas-to-wide-form
  %+  turn  update-list
  |=  [=wing =hoon]
  ^-  (pair plum plum)
  [(wing-to-plum wing) (hoon-to-plum 999 hoon)]
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
::  Render a hoon as a plum.  Given the helper functions above, this is
::  fairly straightforward.  It is a big-ass switch, though.
::
++  hoon-to-plum
  |=  [maxdepth=@ x=hoon]
  |^  ^-  plum
    ?+    x
        %autocons
      [%$ @]     (axis-to-cord p.x)
      [%base *]  (spec [%base p.x])
      [%bust *]  (simple-wide '*' '' '' (spec [%base p.x]) ~)
      [%dbug *]  (hn q.x)                               ::  p.x is irrelevant
      [%eror *]  %assembly-error
      [%hand *]  %ast-node-hand
      [%note *]  (hn q.x)                               ::  p.x is irrelevant
      [%fits *]  %ast-node-fits
      [%knit *]  (simple-wide '"' '' '"' (turn p.x woof-to-plum))
      [%leaf *]  (spec x)
      [%limb *]  p.x
      [%lost *]  (hn p.x)                               ::  for internal use
      [%rock *]  ?^  q.x  !!  (cat 3 '%' (crip (scow p.x `@`q.x)))
      [%sand *]  ?^  q.x  !!  (crip (scow p.x `@`q.x))
      [%tell *]  (simple-wide '<' ' ' '>' (hoons p.x))
      [%tune *]  ?@(p.x p.x %todo-tune)
      [%wing *]  (simple-wide '' '.' '' (turn p.x limb))
      [%yell *]  (simple-wide '>' ' ' '<' (hoons p.x))
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
    ++  hoons      hoons-to-plum-list
    ++  battery    battery-to-plum-list
    ++  chapter    chapters-to-plum
    ++  chum       chum-to-plum
    ++  hint       hint-to-plum
    ++  hn         |=  h=hoon  (hoon-to-plum (dec maxdepth) h)
    ++  limb       limb-to-plum
    ++  matches    matches-to-plum-list
    ++  skin       skin-to-plum
    ++  spc        ' '
    ++  spec       spec-to-plum
    ++  tyre       tyre-to-plum
    ++  updates    updates-to-plum-list
    ++  wing       wing-to-plum
    ++  wingseq    wingseq-to-plum
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
      %+  hoon-to-plum  (dec maxdepth)
      |-  ^-  hoon
      ?~  rem  acc
      $(rem t.rem, acc `hoon`[%tsdt `^wing`p.i.rem `hoon`q.i.rem `hoon`acc])
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
  --
::
::  Pretty-print a hint.
::
++  hint-to-plum
  |=  hint=$@(term (pair term hoon))
  ^-  plum
  ?@  hint  (cat 3 '%' hint)
  :+  %tree
    [wide=`['.' ~] tall=~]
  :~  (cat 3 '%' p.hint)
      (hoon-to-plum 999 q.hint)
  ==
::
::  Pretty-print a hoon battery.
::
++  battery-to-plum-list
  |=  =(map term hoon)
  ^-  (list plum)
  %+  turn  ~(tap by map)
  |=  [=term =hoon]
  =/  fmt  [wide=`['  ' ~] tall=`['' ~]]
  :-  %sbrk
  :+  %tree  fmt
  [term (hoon-to-plum 999 hoon) ~]
::
::  Pretty-print a core.
::
++  core-to-plum
  |=  [=knot head=(unit plum) =(map term hoon)]
  ^-  plum
  =*  kids  (battery-to-plum-list map)
  :-  %sbrk
  :-  %tree
    ?~  head
      :-  [~ `[knot `['++' '--']]]
      kids
    :-  [~ `[knot ~]]
    :~  u.head
        =*  battery-fmt  [~ `['' `['++' '--']]]
        [%tree battery-fmt kids]
    ==
::
::  XX Document this
::
::  XX What's a cleaner way to implement this?
::
++  chapters-to-plum
  |=  [=knot head=(unit plum) =(map term tome)]
  ^-  plum
  =/  chapters=(list (pair term tome))  ~(tap by map)
  =*  with-chapters  (chapters-to-plum-verbose knot head map)
  =*  without-chaps  (core-to-plum knot head q.q.i.chapters)
  ?~  chapters  with-chapters
  ?~  t.chapters
    ?:  .=('' p.i.chapters)  without-chaps
    with-chapters
  with-chapters
::
::  XX Document this.
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
::  XX Document this.
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
::  XX Document this.
::
++  chapters-to-plum-list
  |=  =(map term tome)
  ^-  (list plum)
  %+  turn  ~(tap by map)
  |=  [=term [* hoons=(^map term hoon)]]
  ^-  plum
  ?:  =(term '')
    :+  %tree  [wide=~ tall=[~ '' ~]]  (battery-to-plum-list hoons)
  (rune '+|' ~ ~ [(cat 3 '%' term) (battery-to-plum-list hoons)])
::
::  XX Document this.
::
++  xray-to-plum
  |=  =manx:hoot
  ^-  plum
  %ast-node-xray                                        ::  XX Punt
::
::  Render a plum to a skin.
::
++  skin-to-plum
  |=  =skin
  ^-  plum
  ?@  skin  skin
  %todo-complex-skin                                    ::  XX Punt
::
::  Render a list of wings a plum that looks something like "a:b:c"
::
++  wingseq-to-plum
  |=  =(list wing)
  ^-  plum
  =*  fmt  [wide=`[':' ~] tall=~]
  [%tree fmt (turn list wing-to-plum)]
::
::  Renders a spec to a plum. Similarly to `hoon-to-plum`, given all of
::  the helper functions this becomse quite simple. It does have a lot of
::  cases, though.
::
++  spec-to-plum
  |^  |=  =spec
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
               [(hoon-to-plum 999 p.spec) (turn q.spec ..$)]
        %bsbs  (core-spec-to-plum '$$' p.spec q.spec)
        %bsbr  (subtree (fixed '$|') $(spec p.spec) (hoon-to-plum 999 q.spec) ~)
        %bscb  (hoon-to-plum 999 p.spec)
        %bscl  :-  %sbrk
               :+  %tree
                 [`[' ' `['[' ']']] `['$:' `['' '--']]]
               (turn `(list ^spec)`+.spec ..$)
        %bscn  (subtree (varying '$%' '==') (turn `(list ^spec)`+.spec ..$))
        %bsdt  (core-spec-to-plum '$.' p.spec q.spec)
        %bsld  (subtree (fixed '$<') $(spec p.spec) $(spec q.spec) ~)
        %bsbn  (subtree (fixed '$>') $(spec p.spec) $(spec q.spec) ~)
        %bshp  (subtree (fixed '$-') $(spec p.spec) $(spec q.spec) ~)
        %bskt  (subtree (fixed '$^') $(spec p.spec) $(spec q.spec) ~)
        %bsls  (subtree (fixed '$+') (stud-to-plum p.spec) $(spec q.spec) ~)
        %bsnt  (core-spec-to-plum '$/' p.spec q.spec)
        %bsmc  (subtree (fixed '$;') (hoon-to-plum 999 p.spec) ~)
        %bspd  (subtree (fixed '$&') $(spec p.spec) (hoon-to-plum 999 q.spec) ~)
        %bssg  (subtree (fixed '$~') (hoon-to-plum 999 p.spec) $(spec q.spec) ~)
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
  ++  varying
    |=  [intro=knot final=knot]
    [`[' ' `[(cat 3 intro '(') ')']] `[intro `['' final]]]
  --
::
++  focus-on
  |=  [img=xtable i=key]
  ^-  xray
  =/  res=(unit xray)  (~(get by xrays.img) i)
  ?~  res  ~&  ['internal error: invalid xray reference' i]  !!
  u.res
::
++  tape-to-plum
  |=  =tape
  ^-  plum
  (simple-wide '"' '' '"' `(list plum)`tape)
::
++  battery-refs
  |=  b=xbattery
  ^-  (list key)
  %-  zing
  %+  turn  ~(val by b)
  |=  [=what =(map term key)]
  ^-  (list key)
  ~(val by map)
::
++  render-noun
  |=  [xt=ximage =top=noun]
  ^-  plum
  ::
  =/  img  xtable.xt
  ::
  |^  (main root.xt top-noun)
  ::
  ++  main
    |=  [i=key n=*]
    ^-  plum
    =/  x=xray  (focus-on img i)
    ?~  pats.x  (render-with-data i (need data.x) n)
    (render-with-pattern u.pats.x n)
  ::
  ++  tree-noun-to-list
    |=  n=*
    ^-  (list *)
    ?@  n  ~
    :-  -.n
    %-  zing
    :~  (tree-noun-to-list +.+.n)
        (tree-noun-to-list -.+.n)
    ==
  ::
  ++  noun-to-list
    |=  n=*
    ^-  (list *)
    ?@  n  ~
    [-.n $(n +.n)]
  ::
  ++  render-tree
    |=  [elt=key noun=*]
    ^-  plum
    ?~  noun  '~'
    =/  ns=(list *)     (tree-noun-to-list noun)
    =/  ps=(list plum)  (turn ns |=(n=* (main elt n)))
    =/  elems=plum      (rune ':~' `'==' `['~[' ' ' ']'] ps)
    (rune '%-' ~ `['(' ' ' ')'] ~['tree' elems])
  ::
  ++  render-list
    |=  [elt=key noun=*]
    ^-  plum
    ?~  noun  '~'
    =/  ns=(list *)     (noun-to-list noun)
    =/  ps=(list plum)  (turn ns |=(n=* (main elt n)))
    (rune ':~' `'==' `['~[' ' ' ']'] ps)
  ::
  ++  render-unit
    |=  [i=key n=*]
    ^-  plum
    ?~  n  '~'
    (tuple-plum ~['~' (main i +:n)])
  ::
  ++  tuple-plum
    |=  kids=(list plum)
    ^-  plum
    =/  n  (lent kids)
    (rune ':*' `['=='] `['[' ' ' ']'] kids)
  ::
  ++  render-atom
    |=  [=aura atom=@]
    ^-  plum
    ::  ~&  ['render-atom' aura atom]
    (scot aura atom)
  ::
  ++  render-const
    |=  [=aura const=@ =atom]
    ^-  plum
    ?:  =(~.n aura)  '~'
    (cat 3 '%' (scot aura atom))
  ::
  ++  untyped-noun  ::  XX Where is the existing code for doing this?
    |=  [n=*]      ::  Can I just use it?
    ^-  plum
    ?@  n  (render-atom 'ud' n)
    (tuple-plum ~[(untyped-noun -:n) (untyped-noun +:n)])
  ::
  ++  render-tuple
    |=  [i=key n=*]
    ^-  plum
    =/  acc=(list plum)  ~
    %-  tuple-plum
    %-  flop
    |-
    ^-  (list plum)
    ::
    =/  x=xray             (focus-on img i)
    =/  d=data             (need data.x)
    ::
    ?^  pats.x           [(main i n) acc]
    ?.  ?=([%cell *] d)  [(main i n) acc]
    %=  $
      acc  [(main head.d -:n) acc]
      n    +:n
      i    tail.d
    ==
  ::
  ++  render-with-data
    |=  [i=key d=data n=*]
    ^-  plum
    ::  ~&  ['render-with-data' d n]
    ?-  d
      %void      '!!'
      %noun      (untyped-noun n)
      [%cell *]  (render-tuple i n)
      [%atom *]  ?^  n  ~&  [%not-an-atom i d n]  !!
                 ?~  constant.d  (render-atom aura.d n)
                 (render-const aura.d u.constant.d n)
      [%face *]  (main xray.d n)
      [%pntr *]  !!
      [%core *]  (render-core garb.d xray.d batt.d)
      [%fork *]  (render-fork i n)
    ==
  ::
  ++  render-fork
    |=  [i=key n=*]
    ^-  plum
    ::
    =/  x=xray  (focus-on img i)
    ?~  role.x  ~&  x  '%evil-fork'
    =/  r=role  u.role.x
    ::
    ?+  r  '%bad-fork'
      [%union *]        '%union'                        ::  XX TODO
      [%option *]
        =/  pairs=(list (pair atom key))  ~(tap by map.r)
        |-
        ?~  pairs  '%invalid-option'
        ?.  =(p.i.pairs n)  $(pairs t.pairs)
        (main q.i.pairs n)
      [%junction *]     '%junction'                     ::  XX TODO
      [%conjunction *]  '%conjunction'                  ::  XX TODO
      [%misjunction *]  '%misjunction'                  ::  XX TODO
    ==
  ::
  ++  render-gate
    |=  [=sample=key =product=key]
    ^-  plum
    %-  spec-to-plum  :*
      %bshp
      (ximage-to-spec sample-key img)
      (ximage-to-spec product-key img)
    ==
  ::
  ++  render-core
    |=  [=garb xray=key =xbattery]
    ^-  plum
    ::
    =/  cvt-arms
      |=  m=(map term key)
      ^-  (map term hoon)
      %-  ~(gas by *(map term hoon))
      %+  turn  ~(tap by m)
      |=  [t=term i=key]
      =.  t  ?:(=('' t) '$' t)
      ^-  [term hoon]
      :-  t
      [%zpzp ~]
    ::
    =/  batt=(map term tome)
      %-  ~(gas by *(map term tome))
      %+  turn  ~(tap by xbattery)
      |=  [nm=term w=what arms=(map term key)]
      [nm w (cvt-arms arms)]
    ::
    (hoon-to-plum 999 [%brcn p.garb batt])
  ::
  ++  path-to-plum
    |=  =path
    ^-  plum
    =/  fmt=plumfmt  [[~ '/' [~ '/' '']] ~]

    [%tree fmt path]
  ::
  ++  nock-to-plum
    |=  n=nock
    ^-  plum
    (untyped-noun n)
  ::
  ++  tour-to-plum
    |=  t=tour
    ^-  plum
    '%tour'                                             ::  XX TODO
  ::
  ++  json-to-plum
    ::
    ::  Note that `arrayfmt` and `objfmt` use core-like formatting in
    ::  the tall case. This is kind-of a hack but works well!
    ::
    =/  arrfmt=plumfmt  :-  wide=`[' ' `['[' ']']]
                            tall=`['[ ' `['' ']']]
    ::
    =/  objfmt=plumfmt  :-  wide=`[' ' `['{' '}']]
                            tall=`['{ ' `['' '}']]
    ::
    ::  Note that `kidfmt` uses the magical "ace-ace" rune to get
    ::  4-space indentation.
    =/  kidfmt=plumfmt  [wide=`['' ~] tall=`['  ' `['' '']]]
    ::
    =/  colfmt=plumfmt  [wide=`[' ' ~] tall=`['' `['' '']]]
    ::
    |^  jsn
    ::
    ++  str  |=  t=@t
             ^-  cord
             (cat 3 '"' (cat 3 t '"'))                  ::  XX Escaping
    ::
    ++  key  |=  t=@t
             ^-  cord
             (cat 3 (str t) ':')
    ::
    ++  kid  |=  kids=(list plum)
             ^-  plum
             [%tree kidfmt kids]
    ::
    ++  jsn  |=  j=json
             ^-  plum
             ?-  j
               ~       'null'
               [%a *]  (arr p.j)
               [%b *]  ?:(p.j 'true' 'false')
               [%o *]  (obj p.j)
               [%n *]  p.j
               [%s *]  (str p.j)
             ==
    ::
    ++  arr  |=  l=(list json)
             ^-  plum
             [%sbrk [%tree arrfmt (seq (turn l jsn))]]
    ::
    ++  obj  |=  m=(map @t json)
             ^-  plum
             [%sbrk [%tree objfmt (seq (turn ~(tap by m) col))]]
    ::
    ++  col  |=  [k=@t v=json]
             ^-  plum
             [%sbrk [%tree colfmt ~[(key k) (kid (jsn v) ~)]]]
    ::
    ::
    ::  Adds a comma to the end of every plum but the last.
    ::
    ++  seq  |=  ps=(list plum)
             ^-  (list plum)
             =/  acc=(list plum)  ~
             |-
             ?~  ps    (flop acc)
             ?~  t.ps  (flop [i.ps acc])
             %=  $
               acc  [(com i.ps) acc]
               ps   `(list plum)`t.ps
             ==
    ::
    ++  lst  |=  ps=(list plum)
             ^-  (list plum)
             =/  acc=(list plum)  ~
             |-
             ?~  ps    (flop acc)
             ?~  t.ps  (flop [(com i.ps) acc])
             %=  $
               acc  [i.ps acc]
               ps   `(list plum)`t.ps
             ==
    ::
    ::  Adds a comma at the end of a plum in both wide and tall modes.
    ::
    ++  com  |=  p=plum
             ^-  plum
             ?-  p
               @          (cat 3 p ',')
               [%sbrk *]  [%sbrk (com kid.p)]
               [%para *]  p
               [%tree *]
                 ?.  ?&(?=(^ tall.fmt.p) ?|(=('  ' intro.u.tall.fmt.p) =('' intro.u.tall.fmt.p)))
                   p(fmt (hak fmt.p))
                 p(kids (lst kids.p))
             ==
    ::
    ::  Nasty hack to add a trailing comma to an element in a sequence.
    ::
    ::  Everything that can appear in a sequence has a plum that is
    ::  either a cord or has a `plumfmt` that contains a terminator
    ::  character (possibly empty) in both wide and tall formats.
    ::
    ::  This routine fudges a `plumfmt` value so that a trailing comma
    ::  will be inserted at the end
    ::
    ++  hak  |=  fmt=plumfmt
             ^-  plumfmt
             ::
             %=  fmt
               wide  ?~  wide.fmt            wide.fmt
                     ?~  enclose.u.wide.fmt  wide.fmt
                     =.  q.u.enclose.u.wide.fmt
                       (cat 3 q.u.enclose.u.wide.fmt ',')
                     wide.fmt
               tall  ?~  tall.fmt          tall.fmt
                     ?~  indef.u.tall.fmt  tall.fmt
                     =.  final.u.indef.u.tall.fmt
                       (cat 3 final.u.indef.u.tall.fmt ',')
                     tall.fmt
             ==
    ::
    --
  ::
  ++  mane-to-cord
    |=  m=mane
    ^-  cord
    ?@  m  m
    (cat 3 -:m (cat 3 ':' +:m))
  ::
  ++  manx-text
    |=  [[=mane =mart] =marl]  ^-  (unit tape)
    ?~  mart  ~
    ?:  =('' n.i.mart)  `v.i.mart
    $(mart t.mart)
  ::
  ++  manx-to-plum
    |=  [[=mane attrs=mart] =marl]
    ^-  plum
    |^  result
    ::
    ++  result  `plum`[%sbrk [%tree outfmt toptag childs ~]]
    ++  outfmt  ^-  plumfmt  :-  `['' `['' endtag]]  `['' [~ '' endtag]]
    ::
    ++  tagstr  (mane-to-cord mane)
    ::
    ++  toptag  =/  a  atribs
                ?~  a  (cat 3 topstr '>')
                [%sbrk [%tree topfmt a]]
    ::
    ++  txtstr  ^-  (unit plum)
                =/  res  (manx-text [[mane attrs] marl])
                ?~  res  res
                `(crip u.res)
                ::  `[%para '' ~[(crip u.res)]]
    ::
    ::  Note that `kidfmt` uses "the ace-ace rune" (scare quotes) to
    ::  get indentation.
    ::
    ++  childs  ^-  plum
                =/  body  txtstr
                ?~  body  [%tree kidfmt (turn marl manx-to-plum)]
                    [%tree kidfmt [u.body (turn marl manx-to-plum)]]
    ++  kidfmt  ^-  plumfmt  :-  `['' `['' '']]  `['  ' `['' '']]
    ::
    ++  topfmt  =/  widetopstr  (cat 3 topstr ' ')
                :-  wide=[~ ' ' [~ widetopstr '>']]
                    tall=[~ topstr [~ '' '>']]
    ++  topstr  (cat 3 '<' tagstr)
    ++  atribs  (turn (drop-body attrs) attr-to-plum)
    ::
    ++  endtag  (cat 3 '</' (cat 3 tagstr '>'))
    ++  endfmt  [[~ '' [~ '</' '>']] ~]
    ::
    ++  atrfmt  [[~ '="' [~ '' '"']] ~]                 ::  XX Escaping
    ::
    ::  All attributes except the bullshit '' attribute. (It indicates
    ::  the tag body).
    ::
    ++  drop-body
      |=  l=mart
      ^-  mart
      =/  acc=mart  ~
      |-  ^-  mart
      ?~  l  (flop acc)
      ?:  =('' n.i.l)  $(l t.l)
      $(l t.l, acc [i.l acc])
    ::
    ++  attr-to-plum
      |=  [m=^mane t=tape]
      ^-  plum
      [%tree atrfmt (mane-to-cord m) (crip t) ~]
    ::
    --
  ::
  ++  render-with-pattern
    |=  [p=pattern n=*]
    ^-  plum
    ::  ~&  ['render-with-pattern' p n]
    ?-  p
      %hoon      (hoon-to-plum 999 ((hard hoon) n))
      %json      (json-to-plum ((hard json) n))
      %manx      (manx-to-plum ((hard manx) n))
      %nock      (nock-to-plum ((hard nock) n))
      %path      (path-to-plum ((hard path) n))
      %plum      ((hard plum) n)
      %skin      (skin-to-plum ((hard skin) n))
      %spec      (spec-to-plum ((hard spec) n))
      %tape      (tape-to-plum ((hard tape) n))
      %tour      (tour-to-plum ((hard tour) n))
      %type      =/  ttp  type-to-plum
                 ((hard plum) .*(ttp(+< n) [9 2 0 1]))
      %vase      =/  vtp  vase-to-plum
                 =/  =plum  ((hard plum) .*(vtp(+< n) [9 2 0 1]))
                 (rune '!>' ~ ~ ~[plum])
      [%gate *]  (render-gate sample.p product.p)
      [%gear *]  '%gear'                                ::  XX TODO
      [%list *]  (render-list item.p n)
      [%tree *]  (render-tree item.p n)
      [%unit *]  (render-unit item.p n)
    ==
  --
::
++  simple-type-to-plum
  =/  armsfmt=plumfmt  [[~ ' ' [~ '(' ')']] ~]
  ::
  |^  main
  ::
  ++  arms
    |=  =coil
    ^-  plum
    =/  arms  (arm-names q.r.coil)
    =.  arms  (turn arms |=(c=cord ?:(=('' c) '$' c)))
    ?:  (gte (lent arms) 50)  'KERNEL'
    (sexp 'arms' (chapters-to-plum-list q.r.coil))

  ++  main
    |=  [ty=type maxdepth=@ud]
    ^-  plum
    ?:  =(0 maxdepth)  'DEEP'
    =/  d  (dec maxdepth)
    ?-  ty
      %void      '!!'
      %noun      '*'
      [%atom *]  (sexp 'atom' p.ty ?~(q.ty '~' (scot %ud u.q.ty)) ~)
      [%cell *]  (sexp 'cons' (main p.ty d) (main q.ty d) ~)
      [%core *]  =/  payload  (sexp 'payload' (main p.ty d) ~)
                 (sexp 'core' (arms q.ty) payload ~)
      [%face *]  (sexp 'face' (type-face-to-plum p.ty) (main q.ty d) ~)
      [%fork *]  =/  forks  %+  turn  ~(tap in p.ty)  |=(t=type (main t d))
                 (sexp 'fork' forks)
      [%hint *]  (sexp 'hint' 'hint' (main q.ty d) ~)
      [%hold *]  'HOLD'
    ==
  --
::
++  type-face-to-plum
  |=  f=$@(term tune)
  ^-  plum
  ?@  f  f
  (tune-to-plum f)
::
++  tune-to-plum
  |=  =tune
  ^-  plum
  =/  aliases  p.tune
  =/  bridges  q.tune
  =/  fmt  [[~ ' ' [~ '[' ']']] ~]
  =/  aliases  [%sbrk [%tree fmt 'aliases' (turn ~(tap by p.tune) alias-to-plum)]]
  =/  bridges  [%sbrk [%tree fmt 'bridges' (turn q.tune |=(h=hoon (hoon-to-plum 999 h)))]]
  [%sbrk [%tree fmt 'tune' bridges aliases ~]]
::
++  alias-to-plum
  |=  [=term =(unit hoon)]
  ^-  plum
  =/  fmt  [[~ ' ' [~ '(' ')']] ~]
  [%sbrk [%tree fmt 'alias' term ?~(unit '~' (hoon-to-plum 999 u.unit)) ~]]
::
--
