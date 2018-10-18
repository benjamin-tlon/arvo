::  # Xray Types
::
::  - XX Create patterns and printers for maps and sets.
::
::  - XX The pattern matching code is basically brute-force.
::
::    If it turns out to be a performance bottleneck, there's lots of
::    low-hanging fruit there. For example:
::
::    - Faces repeat the work done for the type they reference.
::    - When detecting whether a cell is part of an "informal" list,
::      we recurse into the tail repeatedly. For example, the following
::      example will do the "formal-list" test 3 times:
::
::      - `[1 2 `(list @)`~]`
::
::  - XX Try to find a way to drop the `%pntr` constructor from
::    `%data`. The consumer of an `xray` does not care.
::
::  - XX Simply lying about the type of deep arms is not robust. I am just
::    claiming that they are nouns, but if another thing in the xray
::    actually needs it, it will think it's a noun too.
::
::  - XX We shouldn't need `traverse-right`.
::
::  - XX It should be possible to wrote a `traverse-battery` routine.
::
::  - XX Finish the logic for printing a noun given a fork.
::
::  - XX Find some way to test the shit out of the fork logic.
::
::  - XX Printing !>(**) blows up. Why?
::
/?  310
::
/-  xray
/+  plum
::
=,  plum
=,  xray
::
=<  |%
    ::
    ++  ximage-to-spec
      ^-  $-(=ximage =spec)
      ximage-to-spec
    ::
    ++  render-noun
      ^-  $-([=ximage =noun] =plum)
      render-noun-using-xray
    ::
    ++  xray
      ^-  $-([@ type] ximage)
      analyze-type-and-decorate
    ::
    ++  render-vase
      |=  =vase
      ^-  wain
      ~(tall plume (vase-to-plum vase))
    ::
    --
|%
::
+|  %utils
::
::  Left-fold over a list.
::
++  fold
   |*  [state=mold elem=mold]
   |=  [[st=state xs=(list elem)] f=$-([state elem] state)]
   ^-  state
   |-
   ?~  xs  st
   =.  st  (f st i.xs)
   $(xs t.xs, st st)

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
::  Pretty-print a vase.
::
++  vase-to-plum
  |=  v=vase  ^-  plum
  (render-noun-using-xray (analyze-type-and-decorate 1 p.v) q.v)
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
  (spec-to-plum (ximage-to-spec (analyze-type-and-decorate 1 t)))
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
::  XX I'm not sure what this is. It's only used in the %bscn case.
::
++  varying
  |=  [intro=knot final=knot]
  [`[' ' `[(cat 3 intro '(') ')']] `[intro `['' final]]]
::
::  Convenience function to build a `plumfmt` for a rune with a fixed
::  number of parameters.
::
++  fixed
  |=  rune=@ta
  ^-  plumfmt
  [wide=`[' ' `[(cat 3 +< '(') ')']] tall=`[+< ~]]
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
::  Same as `fixed` but only constructs the `tall` part of a `plumfmt`.
::
++  tall-fixed
  |=  rune=cord
  ^-  (unit [cord (unit [cord cord])])
  `[rune ~]
::
::  Convenience function to build a the `tall` part of a `plumfmt` for
::  a running-style rune (one that takes a variable number of parameters
::  and has a terminator).
::
++  tall-running
  |=  [rune=cord sigil=cord term=cord]
  ^-  (unit [cord (unit [cord cord])])
  [~ rune [~ sigil term]]
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
::  Convenience function for rendering a rune into a plum. This takes
::  a rune, an optional tall-form terminator, optionally a short-form (if
::  you don't supply a short-form, we'll just construct the standard
::  wide-form (e.g. "?~(x x ~)") for you, and a list of sub-plums.
::
++  rune-to-plum
  |=  $:  rune=cord
          term=(unit cord)
          short=(unit [cord cord cord])
          kids=(list plum)
      ==
  ^.  plum
  |^  :-  %sbrk
      :+  %tree
        :-  (rune-wide-form rune short)
        ?~  term  (tall-fixed rune)
        (tall-running rune '' u.term)
      kids
  ::
  ::  If you just give this a rune, it'll build the standard wide-form.
  ::  Otherwise, it'll just ose the one that you gave it.
  ::
  ++  rune-wide-form
    |=  [rune=cord short=(unit [fst=cord mid=cord lst=cord])]
    ^-  (unit (pair cord (unit [cord cord])))
    =*  fst  (cat 3 rune '(')
    =*  std  `[' ' `[fst ')']]
    ?~  short  std
    `[mid.u.short `[fst.u.short lst.u.short]]
  --
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
::  Just a helper function for constructing a wide-form %tree plum.
::
++  simple-wide-plum
  |=  [init=cord sep=cord end=cord babes=(list plum)]
  ^-  plum
  :+  %tree  [wide=[~ sep [~ init end]] tall=~]  babes
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
      [%bust *]  (simple-wide-plum '*' '' '' (spec [%base p.x]) ~)
      [%dbug *]  (hn q.x)                               ::  p.x is irrelevant
      [%eror *]  %assembly-error
      [%hand *]  %ast-node-hand
      [%note *]  (hn q.x)                               ::  p.x is irrelevant
      [%fits *]  %ast-node-fits
      [%knit *]  (simple-wide-plum '"' '' '"' (turn p.x woof-to-plum))
      [%leaf *]  (spec x)
      [%limb *]  p.x
      [%lost *]  (hn p.x)                               ::  for internal use
      [%rock *]  ?^  q.x  !!  (cat 3 '%' (crip (scow p.x `@`q.x)))
      [%sand *]  ?^  q.x  !!  (crip (scow p.x `@`q.x))
      [%tell *]  (simple-wide-plum '<' ' ' '>' (hoons p.x))
      [%tune *]  ?@(p.x p.x %todo-tune)
      [%wing *]  (simple-wide-plum '' '.' '' (turn p.x limb))
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
    ++  hoons      hoons-to-plum-list
    ++  battery    battery-to-plum-list
    ++  chapter    chapters-to-plum
    ++  chum       chum-to-plum
    ++  hint       hint-to-plum
    ++  hn         |=  h=hoon  (hoon-to-plum (dec maxdepth) h)
    ++  limb       limb-to-plum
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
  (rune-to-plum '+|' ~ ~ [(cat 3 '%' term) (battery-to-plum-list hoons)])
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
::  Convenience function that builds a plum for a subexpression. The
::  `%sbrk` tells the pretty-printer that this is a valid place to
::  switch from tall mode to wide mode.
::
++  subtree
  |=  [p=plumfmt q=(list plum)]
  ^-  plum
  [%sbrk [%tree p q]]
::
::  Renders a spec to a plum. Similarly to `hoon-to-plum`, given all of
::  the helper functions this becomse quite simple. It does have a lot of
::  cases, though.
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
::  Create an new xray and put it in the xray table. If there's already
::  a stub xray under this type, replace it.  Otherwise, allocate a
::  new index and put it there.
::
++  post-xray
  |=  [img=xtable ty=type d=(unit data)]
  ^-  [key xtable]
  ::
  =/  old  (~(get by type-map.img) ty)
  ::
  =^  i=key  img  ?^  old  [u.old img]
                    =/  newkey  next.img
                    =.  next.img  +(next.img)
                    [newkey img]
  =/  x=xray  [i ty d ~ ~ ~ ~ ~ ~ ~]
  =.  xrays.img     (~(put by xrays.img) i x)
  =.  type-map.img  (~(put by type-map.img) ty i)
  [i img]
::
::  Create an new xray and put it in the xray table. If there's already
::  a stub xray under this type, replace it.  Otherwise, allocate a
::  new index and put it there.
::
++  replace-xray
  |=  [img=xtable x=xray]
  ^-  xtable
  img(xrays (~(put by xrays.img) key.x x))
::
++  set-xray-data
  |=  [img=xtable i=key d=data]
  ^-  xtable
  =/  x=xray  (focus-on img i)
  (replace-xray img x(data `d))
::
::  Return an xtable modified to be focus on the %void type. If no
::  void type exists, one will be created.
::
++  void-xray
  |=  img=xtable
  ^-  [key xtable]
  (post-xray img %void `%void)
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
  (simple-wide-plum '"' '' '"' `(list plum)`tape)
::
++  render-noun-using-xray
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
    =/  elems=plum      (rune-to-plum ':~' `'==' `['~[' ' ' ']'] ps)
    (rune-to-plum '%-' ~ `['(' ' ' ')'] ~['tree' elems])
  ::
  ++  render-list
    |=  [elt=key noun=*]
    ^-  plum
    ?~  noun  '~'
    =/  ns=(list *)     (noun-to-list noun)
    =/  ps=(list plum)  (turn ns |=(n=* (main elt n)))
    (rune-to-plum ':~' `'==' `['~[' ' ' ']'] ps)
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
    (rune-to-plum ':*' `['=='] `['[' ' ' ']'] kids)
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
  ++  render-noun  ::  XX Where is the existing code for doing this?
    |=  [n=*]      ::  Can I just use it?
    ^-  plum
    ?@  n  (render-atom 'ud' n)
    (tuple-plum ~[(render-noun -:n) (render-noun +:n)])
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
      %noun      (render-noun n)
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
    (render-noun n)
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
                 (rune-to-plum '!>' ~ ~ ~[plum])
      [%gate *]  (render-gate sample.p product.p)
      [%gear *]  '%gear'                                ::  XX TODO
      [%list *]  (render-list item.p n)
      [%tree *]  (render-tree item.p n)
      [%unit *]  (render-unit item.p n)
    ==
  --
::
++  analyze-type-and-decorate
  |=  [core-depth=@ =type]
  ^-  ximage
  ~&  %analyze-type
  =/  =ximage  (analyze-type core-depth type)
  ~&  %gc-ximage
  =.  ximage  (gc-ximage ximage)
  ~&  %decorate-ximage-with-loops
  =.  ximage  (decorate-ximage-with-loops ximage)
  ~&  %decorate-ximage-with-patterns
  =.  ximage  (decorate-ximage-with-patterns ximage)
  ~&  %decorate-ximage-with-shapes
  =.  ximage  (decorate-ximage-with-shapes ximage)
  ~&  %decorate-ximage-with-roles
  (decorate-ximage-with-roles ximage)
::
++  analyze-type
  |=  [core-depth=@ud =top=type]
  ^-  ximage
  ::
  |^  =/  st=state  [[0 ~ ~] 0]
      =^  res  st  (main top-type st)
      [res img.st]
  ::
  +$  state  [img=xtable depth=@]
  ::
  ::  Create an new xray and put it in the xray table. If there's already
  ::  a stub xray under this type, replace it.  Otherwise, allocate a
  ::  new index and put it there.
  ::
  ++  with-new-xray
    |=  [st=state ty=type]  ^-  [key state]
    =^  res  img.st  (post-xray img.st ty ~)
    [res st]
  ::
  ::  The main analysis code. This basically just calls out to other
  ::  helper functions based on which type of type this is.
  ::
  ::  For %hint, we get analyse the type that this is a hint about, add
  ::  information to it's xray, and then ourselves to the type-map
  ::  as a reference to that type (the one that this hint is
  ::  about). We can delete it from the type-map latter. It just
  ::  needs to be there for now in order to avoid duplicating work. We
  ::  do this for %hold types as well. We really don't want to hold onto
  ::  them, but we don't want to evaluate them repeatedly either.
  ::
  ++  main
    |=  [ty=type st=state]
    ^-  [key state]
    ::
    =/  old  (~(get by type-map.img.st) ty)             ::  don't loop
    ?^  old  [u.old st]
    ::
    =^  res=key  st  (with-new-xray st ty)
    ::
    ^-  [key state]
    ::
    :-  res
    ?-  ty
      %void      st(img (set-xray-data img.st res %void))
      %noun      st(img (set-xray-data img.st res %noun))
      [%atom *]  st(img (set-xray-data img.st res `data`ty))
      ::
      [%cell *]  =^  hed=key  st  (main p.ty st)
                 =^  tyl=key  st  (main q.ty st)
                 =.  img.st  (set-xray-data img.st res [%cell hed tyl])
                 st
      ::
      [%core *]  =^  d=data   st       (xray-core [p.ty q.ty] st)
                 =.  img.st            (set-xray-data img.st res d)
                 st
      ::
      [%face *]  =^  i=key  st   (main q.ty st)
                 =.  img.st  (set-xray-data img.st res [%face p.ty i])
                 st
      ::
      [%fork *]  =^  d=data   st  (fork p.ty st)
                 =.  img.st  (set-xray-data img.st res d)
                 st
      ::
      [%hint *]  =^  ref      st  (main q.ty st)
                 =^  updated  st  (hint p.ty (focus-on img.st res) st)
                 =.  img.st       (replace-xray img.st updated)
                 =.  img.st       (set-xray-data img.st res [%pntr ref])
                 st
      ::
      [%hold *]  ::  ~&  'eval %hold type'
                 ::  ~&  'what is the type of this hoon:'
                 ::  ~&  `wain`~(tall plume (hoon-to-plum q.ty))
                 ::  ~&  q.ty
                 ::  ~&  'with a context of type?'
                 ::  ~&  `wain`~(tall plume (simple-type-to-plum p.ty 10))
                 =^  ref  st  (main ~(repo ut ty) st)
                 =.  img.st   (set-xray-data img.st res [%pntr ref])
                 st
    ==
  ::
  ::  Analyze a %hint type.
  ::
  ::    subject-type: subject of note
  ::    note: hint information
  ::    content-type: type of hinted content
  ::
  ++  hint
    |=  [[=subject=type =note] x=xray st=state]
    ^-  [xray state]
    ?-  -.note
      %help
        =.  helps.x  (~(put in helps.x) p.note)
        [x st]
      %know
        =.  studs.x  (~(put in studs.x) p.note)
        [x st]
      %made
        =^  recipe  st
          ?~  q.note  [[%direct p.note] st]
          =^  params=(list key)  st
            |-
            ^-  [(list key) state]
            ?~  u.q.note  [~ st]
            =/  tsld  [%tsld [%limb %$] [%wing i.u.q.note]]
            =/  part  (~(play ut subject-type) tsld)
            =^  this  st  (main part st)
            =^  more  st  $(u.q.note t.u.q.note)
            [[this more] st]
          [[%synthetic p.note params] st]
        =.  recipes.x  (~(put in recipes.x) recipe)
        [x st]
    ==
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
  +*  batt  [item]  (map term (pair what (map term item)))
  +*  chap  [item]  (pair term (pair what (map term item)))
  +*  arm   [item]  (pair term item)
  ::
  ++  noun-xray
    |=  st=state
    ^-  [key state]
    =^  i  st   (with-new-xray st %noun)
    =.  img.st  (set-xray-data img.st i %noun)
    [i st]
  ::
  ::  In general, there's no way to determine the type of an arm of a
  ::  wet  core, so we just assign all wet arms the type `%noun`.
  ::
  ++  xray-arm
    |=  [st=state =payload=type =coil x=(arm hoon)]
    ^-  [(arm key) state]
    ::  ~&  [depth.st (cat 3 'arm=' p.x)]
    =/  arm-name  ?:(=(p.x '') '$' p.x)
    =.  r.p.coil  %gold
    =^  i=key  st
      ?:  =(0 core-depth)  (noun-xray st)
      =.  core-depth       (dec core-depth)
      ?:  =(%wet q.p.coil)  (noun-xray st)
      (main [%hold [%core payload-type coil] q.x] st)
    [x(q i) st]
  ::
  ::  Analyze a core.
  ::
  ++  xray-core
    |=  [[=payload=type =coil] st=state]
    ^-  [data state]
    =^  payload-key  st  (main payload-type st)
    =^  chapters=(list (chap key))  st
      %+  (traverse-right (chap hoon) (chap key) state)
        [~(tap by q.r.coil) st]
      |=  [c=(chap hoon) st=state]
      =^  l=(list (arm key))  st
        ^-  [(list (arm key)) state]
        %+  (traverse-right (arm hoon) (arm key) state)
          [~(tap by q.q.c) st]
        |=  [=(arm hoon) st=state]
        (xray-arm st payload-type coil arm)
      =/  r  `(chap key)`[p.c `what`p.q.c (~(gas by *(map term key)) l)]
      [r st]
    =/  chaps  (~(gas by *(batt key)) chapters)
    =/  d=data  [%core p.coil payload-key chaps]
    [d st]
  ::
  ::  +fork: convert a %fork $type to an $xray
  ::
  ::  set: set of union types
  ::
  ++  fork
    |=  [types=(set type) st=state]
    ^-  [data state]
    =^  xrays  st
      %+  (traverse type key state)
        [~(tap in types) st]
      |=  [ty=type st=state]
      (main ty st)
    =/  d=data  [%fork (~(gas in *(set key)) xrays)]
    [d st]
  --
::
++  decorate-ximage-with-loops
  |=  xt=ximage  ^-  ximage
  |^  xt(xtable decorated)
  ::
  ++  decorated  ^-  xtable
    =/  root=key      root.xt
    =/  all-indicies  ~(tap in ~(key by xrays.xtable.xt))
    ((fold xtable key) [xtable.xt all-indicies] decorate)
  ::
  ++  decorate
    |=  [img=xtable i=key]  ^-  xtable
    ::
    =/  trace=(set key)  ~
    |-  ^-  xtable
    ::
    =/  x    (focus-on img i)
    =/  dat  (need data.x)
    ::
    ?.  =(~ loop.x)          img                        ::  don't repeat work
    ?:  (~(has in trace) i)  (replace-xray img x(loop `%.y))
    ::
    =.  trace  (~(put in trace) i)
    ::
    =.  img  ?-  dat
               %noun      img
               %void      img
               [%atom *]  img
               [%cell *]  =.  img  $(i head.dat)
                          $(i tail.dat)
               [%core *]  =.  img  $(i xray.dat)
                          %+  (fold xtable key)
                            [img (battery-refs batt.dat)]
                          |=  [img=xtable i=key]
                          ^$(img img, i i)
               [%face *]  $(i xray.dat)
               [%pntr *]  !!                            ::  gc before this
               [%fork *]  %+  (fold xtable key)
                            [img ~(tap in set.dat)]
                          |=  [img=xtable i=key]
                          ^$(img img, i i)
             ==
    ::
    =.  x  (focus-on img i)                             ::  get updated xray
    ?^  loop.x  img                                     ::  loop found
    (replace-xray img x(loop `%.n))                     ::  no loop found
  --
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
++  trace-xray-xtable
  |=  [img=xtable i=key]
  ^-  xtable
  ~&  ['focus=' i]
  ~&  %+  sort  ~(tap by xrays.img)
      |=  [[xi=key x=xray] [yi=key y=xray]]
      (lth xi yi)
  img
::
++  decorate-ximage-with-patterns
  |=  xt=ximage  ^-  ximage
  ::
  =/  img=xtable  xtable.xt
  ::
  |^  =/  pairs  %+  turn  ~(tap by xrays.xtable.xt)
                 |=  [i=key x=xray]
                 ^-  [key xray]
                 [i x(pats (xray-pats x))]
      xt(xrays.xtable (~(gas by *(map key xray)) pairs))
  ::
  ++  focus
    |=  i=key  ^-  xray
    (focus-on img i)
  ::
  ++  is-nil
    |=  i=key  ^-  ?
    =/  d=data  (need data:(focus i))
    ?+  d  %.n
      [%atom *]  =(d [%atom ~.n `0])
      [%face *]  $(i xray.d)
    ==
  ::
  ::  Is `ref`, after dereferencing faces, a loop-reference to `target`?
  ::
  ++  is-ref-to
    |=  [target=key ref=key]  ^-  ?
    ?:  =(target ref)  %.y
    =/  =data  (need data:(focus ref))
    ?:  ?=([%face *] data)  $(ref xray.data)
    %.n
  ::
  ++  is-pair-of-refs-to
    |=  [target=key cell=key]  ^-  ?
    =/  =data  (need data:(focus cell))
    ?:  ?=([%face *] data)  $(cell xray.data)
    ?.  ?=([%cell *] data)  %.n
    ?.  (is-ref-to target head.data)  %.n
    ?.  (is-ref-to target tail.data)  %.n
    %.y
  ::
  ++  is-atom-with-aura
    |=  [c=cord i=key]  ^-  ?
    =/  =data  (need data:(focus i))
    ?+  data  %.n
      [%atom *]  =(data [%atom aura=c constant-unit=~])
      [%face *]  $(i xray.data)
    ==
  ::
  ::  If the xray is a exactly two things, nil and a cell type, then
  ::  return the xray for the cell type.
  ::
  ++  fork-of-nil-and-cell
    |=  x=xray  ^-  (unit key)
    ::
    =/  d=data  (need data.x)
    ::
    ?.  ?=([%fork *] d)  ~
    ::
    =/  branches  ~(tap in set.d)
    ?.  ?=([* * ~] branches)  ~
    ::
    =/  nil   i.branches
    =/  node  i.t.branches
    |-
    ::
    ?:  (is-nil node)  $(node nil, nil node)
    ?.  (is-nil nil)   ~
    ::
    `node
  ::
  ::  Is this xray a unit? (the %unit pattern)
  ::
  ++  unit-pattern
    |^  |=  x=xray
        ^-  (unit pattern)
        =/  elem  (match-unit-type-strict (focus key.x))
        ?~  elem  ~
        `[%unit u.elem]
    ::
    ++  match-unit-type-strict
      |=  =input=xray
      ^-  (unit key)
      ::
      =/  node=(unit key)  (fork-of-nil-and-cell input-xray)
      ?~  node  ~
      ::
      =/  node-data=data  (need data:(focus u.node))
      ::
      ?.  ?=([%cell *] node-data)  ~
      ?.  (is-nil head.node-data)  ~
      =/  elem-key                 tail.node-data
      =/  elem-data                (need data:(focus elem-key))
      ?.  ?=([%face *] elem-data)  ~
      ::
      `xray.elem-data
    --
  ::
  ::  Is this xray a tree? (the %tree pattern)
  ::
  ++  tree-pattern
    |=  =input=xray
    ^-  (unit pattern)
    =/  input-key=key  key.input-xray
    =/  indata=data    (need data.input-xray)
    ?.  ?=([%fork *] indata)  ~
    =/  branches  ~(tap in set.indata)
    ?.  ?=([* * ~] branches)  ~
    =/  nil   i.branches
    =/  node  i.t.branches
    |-
    ?:  (is-nil node)  $(node nil, nil node)
    ?.  (is-nil nil)  ~
    =/  node-data=data  (need data:(focus node))
    ?.  ?=([%cell *] node-data)  ~
    ?.  (is-pair-of-refs-to input-key tail.node-data)
      ~
    =/  elem-data  (need data:(focus head.node-data))
    ?.  ?=([%face *] elem-data)  ~
    `[%tree xray.elem-data]
  ::
  ::  Is this xray a list? (a %list, %tape, %path, or %tour pattern)
  ::
  ::  `match-list` checks is a type is informally a list: Is it a
  ::  cell with (formal or informal) list in it's tail?
  ::
  ::  `match-list-type-strict` checks if a list literally has the shape
  ::  of a `list type`. It must be a loop reference and fork of two
  ::  types, one of which is the nil type and the other is a cell with a
  ::  face in it's head and loop reference as it's tail.
  ::
  ++  list-pattern
    |^  |=  x=xray
        ^-  (unit pattern)
        ::  ~&  ['list-pattern' key.x]
        =/  elem  (match-list x)
        ?~  elem  ~
        ?:  (is-atom-with-aura 'tD' u.elem)   [~ %tape]
        ?:  (is-atom-with-aura 'ta' u.elem)   [~ %path]
        ?:  (is-atom-with-aura 'c' u.elem)    [~ %tour]
        ?:  (is-atom-with-aura 'tas' u.elem)  [~ %path]
        `[%list u.elem]
    ::
    ++  match-list
      |=  =input=xray
      ^-  (unit key)
      =/  d=data  (need data.input-xray)
      ?+  d        ~
        [%face *]  (match-list (focus xray.d))
        [%fork *]  (match-list-type-strict input-xray)
        [%cell *]  =/  elem-key=(unit key)
                     ?:  ?&((is-nil tail.d) (is-atom-with-aura 'tas' head.d))
                       `head.d
                     (match-list (focus tail.d))
                   ?~  elem-key                       ~
                   ?.  (is-ref-to u.elem-key head.d)  ~
                   `u.elem-key
      ==
    ::
    ++  match-list-type-strict
      |=  =input=xray
      ^-  (unit key)
      ::
      =/  node=(unit key)  (fork-of-nil-and-cell input-xray)
      ?~  node  ~
      ::
      =/  node-data=data                   (need data:(focus u.node))
      ?.  ?=([%cell *] node-data)          ~
      ?.  (is-ref-to key.input-xray tail.node-data)  ~
      ::
      =/  elem-data                        (need data:(focus head.node-data))
      ?.  ?=([%face *] elem-data)          ~
      ::
      `xray.elem-data
    --
  ::
  ::  A %gear is any core with a cell context.
  ::
  ::  A %gate is a gear with one chapter ('') with one arm ('').
  ::
  ++  core-pattern
    |^  |=  x=xray
        ^-  (unit pattern)
        =.  x  (focus key.x)
        =/  gear  (match-gear x)
        ?~  gear  ~
        =/  gate  (match-gate x sample.u.gear batt.u.gear)
        ?^  gate  gate
        ~  ::  XX  gear
    ::
    ++  match-gear
      |=  =input=xray
      ^-  (unit [%gear sample=key context=key batt=xbattery])
      ::
      =/  input-data  (need data.input-xray)
      ?.  ?=([%core *] input-data)  ~
      =/  context-key=key  xray.input-data
      ::
      =/  context-data=data  (need data:(focus context-key))
      ?.  ?=([%cell *] context-data)  ~
      ::
      =/  sample-key=key  head.context-data
      =.  context-key     tail.context-data
      `[%gear sample-key context-key batt.input-data]
    ::
    ++  match-gate
      |=  [=input=xray sample=key batt=xbattery]
      ^-  (unit [%gate key key])
      ::
      =/  input-data  (need data.input-xray)
      ?.  ?=([%core *] input-data)  ~
      =/  chapters  ~(tap by batt)
      ::
      ?~  chapters            ~
      ?^  t.chapters          ~
      ?.  =(p.i.chapters '')  ~
      ::
      =/  arms=(list (pair term key))  ~(tap by q.q.i.chapters)
      ::
      ?~  arms            ~
      ?^  t.arms          ~
      ?.  =(p.i.arms '')  ~
      ::
      =/  product=key  q.i.arms
      ::
      `[%gate sample product]
    --
  ::
  ++  simple-nest-pattern
    |=  [ty=type pat=pattern]
    ^-  $-(xray (unit pattern))
    |=  x=xray
    ^-  (unit pattern)
    =/  subtype  (~(nest ut type.x) | ty)
    ?:(subtype `pat ~)
  ::
  ++  type-pattern  (simple-nest-pattern -:!>(*type) %type)
  ++  spec-pattern  (simple-nest-pattern -:!>(*spec) %spec)
  ++  manx-pattern  (simple-nest-pattern -:!>(*manx) %manx)
  ++  vase-pattern  (simple-nest-pattern -:!>(*vase) %vase)
  ++  hoon-pattern  (simple-nest-pattern -:!>(*hoon) %hoon)
  ++  json-pattern  (simple-nest-pattern -:!>(*json) %json)
  ++  nock-pattern  (simple-nest-pattern -:!>(*nock) %nock)
  ++  plum-pattern  (simple-nest-pattern -:!>(*plum) %plum)
  ++  skin-pattern  (simple-nest-pattern -:!>(*skin) %skin)
  ::
  ++  patterns
    ^-  (list $-(xray (unit pattern)))
    :~  tree-pattern
        list-pattern
        unit-pattern
        core-pattern
        spec-pattern
        type-pattern
        manx-pattern
        vase-pattern
        hoon-pattern
        json-pattern
        nock-pattern
        plum-pattern
        skin-pattern
    ==
  ::
  ++  xray-pats
    |=  x=xray
    ^-  (unit pattern)
    ::
    =/  i=key   key.x
    =/  t=type  type.x
    =/  d=data  (need data.x)
    ::
    ::  Atom printing works just fine using the data field.
    ?:  ?=([%atom *] d)  ~
    ::
    =/  match  patterns
    ::
    |-  ^-  (unit pattern)
    ?~  match  ~
    =/  pat  (i.match x)
    ?^  pat  pat
    $(match t.match)
    ::
  --
::
::  1. Build a list of reachable, non-reference nodes.
::  2. Build a table of references to what they reference.
::  3. Map over the type-map, and replace every value using the table from #2.
::  4. Rebuild the xrays map, only keeping xrays from set #1.
::  5. Map over the xrays, and replace every reference using the table from #2.
::
++  gc-ximage
  |=  xt=ximage
  ^-  ximage
  ::
  =/  img=xtable  xtable.xt
  ::
  |^  ::
      =/  =key          root.xt
      =/  tbl           (build-table [~ ~] key)
      =.  key           (fix-key tbl key)
      =.  type-map.img  (fix-type-map tbl type-map.img)
      =.  xrays.img     (fix-xrays tbl xrays.img)
      ~&  [%gc-results ~(wyt by type-map.img) ~(wyt by xrays.img)]
      ::
      [key img]
  ::
  +$  table  [live=(set key) refs=(map key key)]
  ::
  ::  Get all the keys in a map the correspond to a value. Expensive.
  ::
  ++  reverse-lookup
    |*  [key=mold val=mold]
    |=  [tbl=(map key val) match=val]
    ^-  (set key)
    %+  (fold (set key) (pair key val))
      [~ ~(tap by tbl)]
    |=  [acc=(set key) k=key v=val]
    ?.  =(match v)  acc
    (~(put in acc) k)
  ::
  ::  Given a node that may be a pointer, follow the chain of pointers
  ::  to a non-pointer node.
  ::
  ++  deref
    |=  [img=xtable i=key]
    ^-  key
    |-
    =/  x=xray  (focus-on img i)
    =/  d=data  (need data.x)
    ?.  ?=([%pntr *] d)  key.x
    $(i xray.d)
  ::
  ++  build-table
    |=  [tbl=table i=key]
    ^-  table
    ::
    ?:  (~(has in live.tbl) i)  tbl                     ::  already processed
    ?:  (~(has by refs.tbl) i)  tbl                     ::  already processed
    ::
    =/  x=xray  (focus-on img i)
    =/  d=data  (need data.x)
    ::
    =.  tbl
      ?.  ?=([%pntr *] d)
        tbl(live (~(put in live.tbl) i))
      tbl(refs (~(put by refs.tbl) i (deref img i)))
    ::
    %+  (fold table key)
      [tbl (xray-refs i)]
    build-table
    ::
  ++  gc-xrays
    |=  [tbl=table xrays=(map key xray)]
    ^-  _xrays
    %+  (fold (map key xray) (pair key xray))
      [*(map key xray) ~(tap by xrays)]
    |=  [acc=(map key xray) [i=key x=xray]]
    ?.  (~(has in live.tbl) i)  acc
    (~(put by acc) i x)
  ::
  ++  fix-type-map
    |=  [tbl=table =(map type key)]
    ^-  _map
    %+  (fold _map (pair type key))
      [*_map ~(tap by map)]
    |=  [acc=_map [ty=type i=key]]
    =/  dest  (~(get by refs.tbl) i)
    ?^  dest  (~(put by acc) ty u.dest)
    ?.  (~(has in live.tbl))  acc
    (~(put in acc) ty i)
  ::
  ++  fix-xrays
    |=  [tbl=table xrays=(map key xray)]
    ^-  _xrays
    %+  (fold (map key xray) (pair key xray))
      [*(map key xray) ~(tap by xrays)]
    |=  [acc=(map key xray) [i=key x=xray]]
    ?.  (~(has in live.tbl) i)  acc                     ::  Drop unused xrays
    (~(put by acc) i (fix-xray tbl x))
  ::
  ::  All the xrays which are simply references to `i`.
  ::
  ::  XX Just reverse the `refs.tbl` map first and store it in `tbl`
  ::  as another field.  This is probably too slow.
  ::
  ++  points-to
    |=  [tbl=table i=key]  ^-  (set key)
    ((reverse-lookup key key) refs.tbl i)
  ::
  ::  There is hint data on the %pntr xrays. Find all of them and collect
  ::  the hints into one place.
  ::
  ++  collect-hints
    |=  [tbl=table target=xray]  ^-  xray
    %+  (fold xray key)
      [target ~(tap in (points-to tbl key.target))]
    |=  [acc=xray ref=key]
    =/  ref-xray=xray  (focus-on img ref)
    =/  helps    ^-  (set help)    (~(uni in helps.acc) helps.ref-xray)
    =/  recipes  ^-  (set recipe)  (~(uni in recipes.acc) recipes.ref-xray)
    ::
    =/  studs    ^-  (set stud)                         ::  Type system hack
                 %+  (fold (set stud) stud)
                   [studs.acc ~(tap in studs.ref-xray)]
                 |=  [acc=(set stud) new=stud]
                 (~(put in acc) new)
    ::
    acc(helps helps, studs studs, recipes recipes)
  ::
  ::  XX `roles` contains references too, but this runs before role annotation.
  ::
  ++  fix-xray
    |=  [tbl=table x=xray]  ^-  xray
    =.  x  (collect-hints tbl x)
    %=  x
      data     `(fix-data tbl (need data.x))
      recipes  %-  ~(gas in *(set recipe))
               %+  turn  ~(tap in recipes.x)
               |=  r=recipe  (fix-recipe tbl r)
    ==
  ::
  ++  fix-data
    |=  [tbl=table d=data]  ^-  data
    ::
    =/  fix  |=(i=key (fix-key tbl i))
    ::
    ?-  d
      %noun      d
      %void      d
      [%atom *]  d
      [%cell *]  d(head (fix head.d), tail (fix tail.d))
      [%core *]  d(xray (fix xray.d), batt (fix-battery tbl batt.d))
      [%face *]  d(xray (fix xray.d))
      [%fork *]  d(set (~(gas in *(set key)) (turn ~(tap in set.d) fix)))
      [%pntr *]  d(xray (fix xray.d))
    ==
  ::
  ++  turn-battery
    |=  [b=xbattery f=$-(key key)]  ^-  xbattery
    %-  ~(run by b)
    |=  [w=what chap=(map term key)]  ^-  [what (map term key)]
    :-  w
    %-  ~(run by chap)
    |=  i=key  ^-  key
    (f i)
  ::
  ++  fix-battery
    |=  [tbl=table b=xbattery]  ^-  xbattery
    %+  turn-battery  b
    |=  i=key  ^-  key
    (fix-key tbl i)
  ::
  ++  fix-key
    |=  [tbl=table i=key]  ^-  key
    =/  res=(unit key)  (~(get by refs.tbl) i)
    ?^  res  u.res
    i
  ::
  ++  fix-recipe
    |=  [tbl=table r=recipe]  ^-  recipe
    ?-  r
      [%direct *]     r
      [%synthetic *]  r(list (turn list.r |=(i=key (fix-key tbl i))))
    ==
  ::
  ++  xray-refs
    |=  i=key
    ^-  (list key)
    =/  x=xray  (focus-on img i)
    %-  zing
    ^-  (list (list key))
    :~  ?~(data.x ~ (data-refs u.data.x))
        (zing (turn ~(tap in recipes.x) recipe-refs))
        ?~(role.x ~ (role-refs u.role.x))
    ==
  ::
  ++  recipe-refs
    |=  r=recipe
    ^-  (list key)
    ?-  r
      [%direct *]     ~
      [%synthetic *]  list.r
    ==
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
  ++  role-refs
    |=  s=role
    ^-  (list key)
    ?@  s  ~
    ?-  -.s
      %constant     ~
      %instance     ~
      %option       ~(val by map.s)
      %union        ~(val by map.s)
      %junction     ~[flat.s deep.s]
      %conjunction  ~[wide.s tall.s]
      %misjunction  ~[one.s two.s]
    ==
  ::
  ++  data-refs
    |=  d=data
    ^-  (list key)
    ?-  d
      %noun      ~
      %void      ~
      [%atom *]  ~
      [%cell *]  ~[head.d tail.d]
      [%core *]  [xray.d (battery-refs batt.d)]
      [%face *]  ~[xray.d]
      [%pntr *]  ~[xray.d]
      [%fork *]  ~(tap in set.d)
    ==
  --
::
++  xray-branches
  |=  [img=xtable i=key]
  ^-  (set key)
  ::
  :: ~&  ['ROOT' i]
  =/  acc=(set key)  ~
  =/  stk=(set key)  ~
  ::
  |-  ^-  (set key)
  ::
  ?:  (~(has in acc) i)  acc
  ?:  (~(has in stk) i)  acc
  ::
  =.  stk  (~(put in stk) i)
  ::
  =/  x=xray  (focus-on img i)
  =/  d=data  (need data.x)
  ::
  ?-  d
    %noun      (~(put in acc) i)
    %void      (~(put in acc) i)
    [%atom *]  (~(put in acc) i)
    [%cell *]  (~(put in acc) i)
    [%core *]  (~(put in acc) i)
    [%face *]  $(i xray.d)
    [%pntr *]  $(i xray.d)
    [%fork *]  %+  (fold (set key) key)
                 [acc ~(tap in set.d)]
               |=  [=(set key) =key]
               ^$(acc set, i key)
  ==
::
++  decorate-ximage-with-shapes
  |^  |=  xt=ximage
      ^-  ximage
      =/  keys  ~(tap in ~(key by xrays.xtable.xt))
      %=  xt  xtable
        %+  (fold xtable key)  [xtable.xt keys]
        |=  [st=xtable i=key]
        xtable:(xray-shape st ~ i)
      ==
  ::
  ::  The trace `tr` is used to prevent infinite forks.
  ::
  ++  xray-shape
    |=  [st=xtable tr=(set key) i=key]
    ^-  [shape =xtable]
    ::
    =/  x=xray  (focus-on st i)
    =/  dat  (need data.x)
    ?^  shape.x  [u.shape.x st]
    ::
    =^  res=shape  st
      ?-  dat
        %noun      [%noun st]
        %void      [%void st]
        [%atom *]  [%atom st]
        [%cell *]  [%cell st]
        [%core *]  [%cell st]
        [%fork *]  =/  fork-trace  (~(put in tr) i)
                   =.  set.dat  (~(dif in set.dat) tr)
                   (fork-shape st fork-trace set.dat)
        [%face *]  (xray-shape st tr xray.dat)
        [%pntr *]  !!
      ==
    ::
    =/  y=xray    x                                     ::  Type system hack.
    =.  shape.y   `res
    =.  xrays.st  (~(put by xrays.st) key.y y)
    [res st]
  ::
  ++  fork-shape
    |=  [st=xtable tr=(set key) fork=(set key)]
    ^-  [shape xtable]
    ::
    %+  (fold (pair shape xtable) key)
      [[%void st] ~(tap in fork)]
    |=  [acc=(pair shape xtable) i=key]
    ^-  [shape xtable]
    =^  res  st  (xray-shape q.acc tr i)
    [(combine p.acc res) st]
  ::
  ++  combine
    |=  [x=shape y=shape]
    ^-  shape
    ?:  =(x y)      x
    ?:  =(x %void)  y
    ?:  =(y %void)  x
    ?:  =(x %noun)  %noun
    ?:  =(y %noun)  %noun
    %junc
  --
::
++  decorate-ximage-with-roles
  |^  |=  xt=ximage  ^-  ximage
      ::
      =/  keys=(list key)  ~(tap in ~(key by xrays.xtable.xt))
      ::
      %=  xt  xtable
        %+  (fold xtable key)  [xtable.xt keys]
        |=  [st=xtable i=key]
        ^-  xtable
        xtable:(xray-role st i)
      ==
  ::
  ::  Produce an xtable focused on the xray for a given type. If the
  ::  type isn't already in the xtable, create it first.
  ::
  ::  These xrays are for fake types that we create to restructure forks,
  ::  therefore they will never by loops.
  ::
  ++  with-new-xray
    |=  [st=xtable ty=type d=data]
    ^-  [key xtable]
    =/  old=(unit key)  (~(get by type-map.st) ty)
    ?^  old  [u.old st]
    =/  key          next.st
    =/  res=xray     [key ty `d ~ ~ ~ ~ ~ ~ `%.n]
    =.  next.st      +(key)
    =.  xrays.st     (~(put by xrays.st) key.res res)
    =.  type-map.st  (~(put by type-map.st) type.res key.res)
    [key st]
  ::
  ::  Return an xtable modified to be focus on the %void type. If no
  ::  void type exists, one will be created.
  ::
  ++  void-xray
    |=  =xtable
    ^-  ximage
    (with-new-xray xtable %void %void)
  ::
  ::  Determines the role of an atom xray.
  ::
  ++  atom-role
    |=  =constant=(unit @)
    ^-  role
    ?~  constant-unit  %atom
    [%constant u.constant-unit]
  ::
  ::  Determine the role of %fork type.
  ::
  ::  First, find (or create) an xray for the union type, then call back
  ::  into `role-xray` to get it's type.
  ::
  ::  The focused xray of the resulting xtable *will* be decorated with
  ::  role information, the role is just returned for convenience.
  ::
  ++  fork-role
    |=  [st=xtable fork=(set key)]
    ^-  [role xtable]
    =^  i=key  st  (fork-xray st fork)
    (xray-role st i)
  ::
  ::  Calculate the role of a %cell xray.
  ::
  ++  cell-role
    |=  x=xray
    ^-  role
   ::
    =/  =shape  (need shape.x)
    =/  =data   (need data.x)
   ::
    =/  const  ?.  ?=([%atom *] data)  ~
               constant.data
    ::
    ?:  =(shape %cell)  %wide
    ?^  const           [%instance u.const]
    %cell
  ::
  ::  Produces an xtable updated to have role information for the xray
  ::
  ::  Produces an xtable updated to have role information for the xray
  ::  in focus.
  ::
  ::  The focused xray of the resulting xtable *will* be decorated with
  ::  role information, the role is just return for convenience.
  ::
  ++  xray-role
    |=  [st=xtable i=key]
    ^-  [=role =xtable]
    =/  x=xray  (focus-on st i)
    ::
    =/  old  role.x
    ?^  old  [u.old st]                    ::  Don't repeat work.
    ::
    :: ~&  key.x  ::   shape.x (need data.x)]
    ::
    =/  dat=data  (need data.x)
    ::
    =^  res=role  st
      ?:  ?=([~ %void] shape.x)  [%void st]
      ?:  ?=([~ %noun] shape.x)  [%noun st]
      ?-  dat
        %noun      :_  st  %noun
        %void      :_  st  %void
        [%atom *]  :_  st  (atom-role constant.dat)
        [%cell *]  :_  st  (cell-role (focus-on st head.dat))
        [%core *]  :_  st  %cell
        [%face *]  (xray-role st xray.dat)
        [%pntr *]  !!
        [%fork *]  (fork-role st (xray-branches st key.x))
      ==
    ::
    =.  xrays.st  (~(put by xrays.st) key.x x(role `res))
    [res st]
  ::
  ::  Create a new xray from a union type.  Returns an xtable focused
  ::  on the result.
  ::
  ++  fork-xray
    |=  [st=xtable fork=(set key)]
    ^-  [key xtable]
    =^  void  st  (void-xray st)
    %+  (fold ximage key)
      [[void st] ~(tap in fork)]
    |=  [xt=ximage branch=key]
    (merge xtable.xt root.xt branch)
  ::
  ::  Combine two xrays in an xtable (the one in focus and the one
  ::  referenced by `i`, producing a new xtable focused on the resulting
  ::  union.
  ::
  ::  First, we compute the role of both xrays, and then we `combine`
  ::  them.
  ::
  ++  merge
    |=  [st=xtable this=key that=key]
    ^-  [key xtable]
    :: ~&  ['merge' x y]
    =/  this-xray=xray  (focus-on st this)
    =/  that-xray=xray  (focus-on st that)
    ?:  =(%void type.this-xray)  [that st]
    ?:  =(%void type.that-xray)  [this st]
    (combine st this that)
  ::
  ::  `combine` is complicated. Let's introduce it's helper-functions first:
  ::
  ::  -simple-forks: produce a simple fork set if any.
  ::
  ++  simple-forks
    |=  [img=xtable i=key]
    ^-  (unit (set key))
    =/  x=xray  (focus-on img i)
    =/  d=data  (need data.x)
    ?.  ?=([%fork *] d)  ~
    `set.d
  ::
  ::  Given two xrays, construct their union type and return it's xray.
  ::
  ::  Returns an `xtable` focused on the resulting xray.
  ::
  ::  Using the `fork` primitive to construct a new type, get the xray
  ::  for that type. If we already have an xray for that, just return
  ::  it. Otherwise we need to create one. The `data` field for the new
  ::  xray will be (basically) the result of doing a set-merge on the
  ::  trivial-forks of both xrays.
  ::
  ++  join
    |=  [st=xtable this=key that=key]  ^-  ximage
    ?:  =(this that)  [this st]
    ::
    =/  this-xray=xray  (focus-on st this)
    =/  that-xray=xray  (focus-on st that)
    ::
    =/  ty=type    (fork `(list type)`~[type.this-xray type.that-xray])
    =/  dat=data   :-  %fork
                   ^-  (set key)
                   =/  these  (simple-forks st this)
                   =/  those  (simple-forks st that)
                   ?~  these  ?~  those  (sy this that ~)
                              (~(put in u.those) this)
                   ?~  those  (~(put in u.these) that)
                   (~(uni in u.these) u.those)
    (with-new-xray st ty dat)
  ::
  ::  =collate-union: merge union maps
  ::
  ++  collate-union
    |=  [st=xtable thick=(map atom key) thin=(map atom key)]
    ^-  [(map atom key) xtable]
    :: ~&  'collate-union'
    =/  list=(list (pair atom key))  ~(tap by thin)
    |-
    ^-  [(map atom key) xtable]
    ?~  list  [thick st]
    =/  item=(unit key)  (~(get by thick) p.i.list)
    =^  merged=key  st  ?~  item  [q.i.list st]
                        (merge-instances st p.i.list u.item q.i.list)
    =/  new-thick  (~(put by thick) p.i.list merged)
    $(list t.list, thick new-thick)
  ::
  ::  =collate-option: merge option maps
  ::
  ++  collate-option
    |=  [st=xtable thick=(map atom key) thin=(map atom key)]
    ^-  [(map atom key) xtable]
    :: ~&  'collate-option'
    =/  list=(list (pair atom key))  ~(tap by thin)
    |-
    ^-  [(map atom key) xtable]
    ?~  list  [thick st]
    =/  item=(unit key)  (~(get by thick) p.i.list)
    =^  merged=key  st  ?~  item  [q.i.list st]
                        (merge st u.item q.i.list)
    =/  new-thick  (~(put by thick) p.i.list merged)
    $(list t.list, thick new-thick)
  ::
  ::  We want to merge two cell-types that have the same head; gross.
  ::
  ::  First, get both tail types, merge them, produce a new cell type
  ::  with the merged tail.
  ::
  ++  merge-instances
    |=  [st=xtable =atom =x=key =y=key]
    ^-  [key xtable]
    ::
    =/  x-xray=xray    (focus-on st x-key)
    =/  x-data=data    (need data.x-xray)
    |-  ^-  [key xtable]
    ::
    ?:  ?=([%face *] x-data)  $(x-data (need data:(focus-on st xray.x-data)))
    ?>  ?=([%cell *] x-data)
    =/  x-tail=key      tail.x-data
    =/  head-xray=xray  (focus-on st head.x-data)
    ::
    =/  y-xray=xray     (focus-on st y-key)
    =/  y-data=data     (need data.y-xray)
    |-  ^-  [key xtable]
    ::
    ?:  ?=([%face *] y-data)  $(y-data (need data:(focus-on st xray.y-data)))
    ?>  ?=([%cell *] y-data)
    =/  y-tail=key      tail.y-data
    ::
    =^  merged-tail  st  (merge st x-tail y-tail)
    =/  tail-xray=xray   (focus-on st merged-tail)
    ::
    =/  res-ty=type    [%cell type.head-xray type.tail-xray]
    =/  res-data=data  [%cell key.head-xray key.tail-xray]
    =^  res-key  st    (with-new-xray st res-ty res-data)
    ::
    =/  res-xray=xray   (focus-on st res-key)
    =.  shape.res-xray  `%cell
    =.  role.res-xray   `[%instance atom]
    =.  xrays.st        (~(put by xrays.st) res-key res-xray)
    ::
    [key.res-xray st]
  ::
  ++  combine
    |=  [st=xtable this=key that=key]
    ^-  ximage
    ::
    |-  ^-  ximage
    ::
    ?:  =(this that)  [this st]
    ::
    =^  this-role=role  st  (xray-role st this)
    =^  that-role=role  st  (xray-role st that)
    ::
    ::  Create the join of two xrays with the specified `role`.
    ::
    =/  join-with-role
      |=  [st=xtable x=key y=key =role]
      ^-  ximage
      ::
      =/  xx  (focus-on st x)
      =/  yy  (focus-on st y)
      ::
      =^  joined=key  st  (join st x y)
      =/  j=xray          (focus-on st joined)
      =.  st              (replace-xray st j(role `role))
      [joined st]
    ::
    ::  Produce a joined node with the specified `role`.
    ::
    =/  joined
      |=  [st=xtable =role]
      ^-  ximage
      (join-with-role st this that role)
    ::
    ::  Convenience functions for creating junctions
    ::
    =/  misjunct  |=  [st=xtable x=key y=key]
                  =/  xx=xray  (focus-on st x)
                  =/  yy=xray  (focus-on st y)
                  (join-with-role st x y [%misjunction x y])
    ::
    =/  conjunct  |=  [st=xtable wide=key tall=key]
                  (join-with-role st wide tall [%conjunction wide tall])
    ::
    =/  junct     |=  [st=xtable flat=key deep=key]
                  (join-with-role st flat deep [%junction flat deep])
    ::
    ::  Join a cell with a junction.
    ::
    =/  cell-junct
      |=  [st=xtable cell=key [flat=key deep=key]]
      ^-  ximage
      =^  deep-merged  st  (merge st cell deep)
      (junct st flat deep-merged)
    ::
    ::  Join an atom with a junction.
    ::
    =/  atom-junct
      |=  [st=xtable atom=key [flat=key deep=key]]
      ^-  [key xtable]
      =^  flat-merged  st  (merge st atom flat)
      (junct st flat-merged deep)
    ::
    =/  tall-conjunct
      |=  [st=xtable out-tall=key [wide=key in-tall=key]]
      ^-  [key xtable]
      =^  new-tall  st  (merge st out-tall in-tall)
      (conjunct st wide new-tall)
    ::
    =/  conjunct-conjunct
      |=  [st=xtable [xwide=key xtall=key] [ywide=key ytall=key]]
      ^-  [key xtable]
      =^  new-wide  st  (merge st xwide ywide)
      =^  new-tall  st  (merge st xtall ytall)
      ::
      :: XX Merging the talls or the wides might produce a misjunction! In
      :: either case, the result should also be a misjunction, not a
      :: conjunction. This is wrong.
      ::
      (conjunct st new-wide new-tall)
    ::
    ?@  this-role
      ?^  that-role  $(this that, that this)
      %+  joined  st
      ^-  role
      ?:  =(this-role that-role)                      this-role
      ?:  ?=(%void this-role)                         that-role
      ?:  ?=(%void that-role)                         this-role
      ?:  |(?=(%noun this-role) ?=(%noun that-role))  %noun
      ?-  this-role
        %atom  [%junction this that]
        %cell  ?:  ?=(%wide that-role)  %cell
               [%junction that this]
        %wide  ?:  ?=(%cell that-role)  %cell
               [%junction that this]
      ==
    ::
    ::  At this point `this-role` is a constant, instance, option, union,
    ::  junction, conjunction, or a misjunction.
    ::
    ?@  that-role
      ?-  that-role
        %noun  (joined st this-role)
        %void  (joined st this-role)
        %atom  ?-  -.this-role
                 %conjunction  (junct st that this)
                 %constant     (joined st %atom)
                 %instance     (junct st that this)
                 %junction     (atom-junct st that [flat deep]:this-role)
                 %misjunction  (misjunct st that this)
                 %option       (joined st %atom)
                 %union        (junct st that this)
               ==
        %cell  ?-  -.this-role
                 %conjunction  (misjunct st this that)  ::  Can't discriminate
                 %constant     (junct st this that)
                 %instance     (misjunct st this that)  ::  Can't discriminate
                 %junction     (cell-junct st that [flat deep]:this-role)
                 %misjunction  (misjunct st this that)
                 %option       (junct st this that)
                 %union        (misjunct st this that)  ::  Invalid union
               ==
        %wide  ?-  -.this-role
                 %conjunction  (misjunct st this that)  ::  Can't discriminated
                 %constant     (junct st this that)
                 %instance     (conjunct st that this)
                 %junction     (cell-junct st that [flat deep]:this-role)
                 %misjunction  (misjunct st this that)
                 %option       (junct st this that)
                 %union        (conjunct st that this)
               ==
      ==
    ::
    ::  At this point both `this-role` and `that-role` are either a
    ::  constant, instance, option, union, junction, conjunction, or
    ::  a misjunction.
    ::
    ?:  |(?=(%misjunction -.this-role) ?=(%misjunction -.that-role))
      (misjunct st this that)
    ::
    ::  Now we know that neither role is a misjunction.
    ::
    ?-    -.this-role
        %constant
      ?-  -.that-role
        %constant     =/  this-arg=(map atom key)  [[atom.this-role this] ~ ~]
                      =/  that-arg=(map atom key)  [[atom.that-role that] ~ ~]
                      =^  res  st  (collate-option st this-arg that-arg)
                      (joined st [%option res])
        %instance     (junct st this that)
        %option       =/  that-arg=(map atom key)  map.that-role
                      =/  this-arg=(map atom key)  [[atom.this-role this] ~ ~]
                      =^  res  st  (collate-option st this-arg that-arg)
                      (joined st [%option res])
        %union        (junct st this that)
        %junction     =^  merged  st  (merge st this flat.that-role)
                      (junct st merged deep.that-role)
        %conjunction  (junct st this that)
      ==
    ::
        %instance
      ?+  -.that-role  $(this that, that this)
        %instance     =/  this-arg=(map atom key)  [[atom.this-role this] ~ ~]
                      =/  that-arg=(map atom key)  [[atom.that-role that] ~ ~]
                      =^  res  st  (collate-union st this-arg that-arg)
                      (joined st [%union res])
        %option       (junct st this that)
        %union        =/  this-arg  [[atom.this-role this] ~ ~]
                      =^  res  st   (collate-union st map.that-role this-arg)
                      (joined st [%union res])
        %junction     =^  merged  st  (merge st this deep.that-role)
                      (junct st flat.that-role merged)
        %conjunction  (tall-conjunct st this [wide tall]:that-role)
      ==
    ::
        %option
      ?+  -.that-role  $(this that, that this)
        %option       =^  res  st  (collate-option st map.this-role map.that-role)
                      (joined st [%option res])
        %union        (junct st this that)
        %junction     =^  merged  st  (merge st this flat.that-role)
                      (junct st merged deep.that-role)
        %conjunction  (junct st this that)
      ==
    ::
        %union
      ?+  -.that-role  $(this that, that this)
        %union        =^  res  st  (collate-union st map.this-role map.that-role)
                      (joined st [%union res])
        %junction     =^  merged  st  (merge st this deep.that-role)
                      (junct st flat.that-role merged)
        %conjunction  (tall-conjunct st this [tall wide]:that-role)
      ==
    ::
        %junction
      ?+  -.that-role  $(this that, that this)
        %junction     =^  flat  st  (merge st flat.this-role flat.that-role)
                      =^  deep  st  (merge st deep.this-role deep.that-role)
                      (junct st flat deep)
        %conjunction  =^  merged  st  (merge st deep.this-role that)
                      (junct st flat.this-role merged)
      ==
    ::
        %conjunction
      ?+  -.that-role  $(this that, that this)
        %conjunction  (conjunct-conjunct st [wide tall]:this-role [wide tall]:that-role)
      ==
    ==
  --
::
::  -ximage-to-spec: convert to spec
::
++  ximage-to-spec
  |=  [=top=key img=xtable]
  ^-  spec
  ::
  |^  (xray-to-spec ~ top-key)
  ::
  +$  trace  (set key)
  ::
  ++  recipe-to-spec
    |=  [tr=trace r=recipe]
    ^-  spec
    ?-  -.r
      %direct     [%like [term.r ~] ~]
      %synthetic  =/  subs  %+  turn  list.r
                            |=  =key  (xray-to-spec tr key)
                  [%make [%limb term.r] subs]
    ==
  ::
  ++  wrap-with-loop-binding
    |=  [xr=xray sp=spec]
    ^-  spec
    ?.  (need loop.xr)  sp
    =/  nm  (synthetic key.xr)
    [%bsbs [%loop nm] [[nm sp] ~ ~]]
  ::
  ++  xray-to-spec
    |=  [tr=trace i=key]
    ^-  spec
    =/  x=xray  (focus-on img i)
    =/  d=data  (need data.x)
    ?:  (~(has in tr) i)  [%loop (synthetic i)]
    ?^  recipes.x  (recipe-to-spec tr n.recipes.x)
    %+  wrap-with-loop-binding  x
    =.  tr  (~(put in tr) i)
    ^-  spec
    ?@  d  [%base d]
    ?-  -.d
      %atom  ?~  constant.d  [%base %atom aura.d]
             ?:  &(=(%n aura.d) =(`@`0 u.constant.d))  [%base %null]
             [%leaf aura.d u.constant.d]
      %cell  =/  hd  `spec`$(i head.d)
             =/  tl  `spec`$(i tail.d)
             =/  both-basic  &(=([%base %noun] hd) =([%base %noun] tl))
             ?:  both-basic      [%base %cell]
             ?:  ?=(%bscl -.tl)  [%bscl hd +.tl]
             [%bscl hd tl ~]
      %core  =/  payld  $(i xray.d)
             =/  batt   ^-  (map term spec)
                        %-  ~(run by (flatten-battery batt.d))
                        |=  =key  ^$(i key)
             ?-  r.garb.d
               %lead  [%bszp payld batt]
               %gold  [%bsdt payld batt]
               %zinc  [%bstc payld batt]
               %iron  [%bsnt payld batt]
             ==
      %pntr  !!
      %face  =/  =spec  $(i xray.d)
             ?^(face.d spec [%bsts face.d spec])
      %fork  =/  =role  (need role.x)
             |^  ?+  role
                     ~&  [%unexpected-fork-role key.x d role choices]
                     [%bswt choices]
                   %noun             [%base %noun]
                   %void             [%base %void]
                   [%option *]       [%bswt choices]
                   [%union *]        [%bscn choices]
                   [%misjunction *]  [%bswt choices]
                   [%junction *]     [%bsvt ^$(i flat.role) ^$(i deep.role)]
                   [%conjunction *]  [%bskt ^$(i wide.role) ^$(i tall.role)]
                 ==
             ::
             ++  choices
               ^-  [i=spec t=(list spec)]
               =-  ?>(?=(^ -) -)
               (turn ~(tap in set.d) |=(=key ^^$(i key)))
             --
    ==
  ::
  ::  =synthetic: given a small atom (:number), construct a coresponding
  ::  symbol using the Hebrew alphabet.
  ::
  ++  synthetic
    |=  number=@ud
    ^-  @tas
    =/  alf/(list term)
        ^~  :~  %alf  %bet  %gim  %dal  %hej  %vav  %zay  %het
                %tet  %yod  %kaf  %lam  %mem  %nun  %sam  %ayn
                %pej  %sad  %qof  %res  %sin  %tav
            ==
    ?:  (lth number 22)
      (snag number alf)
    (cat 3 (snag (mod number 22) alf) $(number (div number 22)))
  --
  ::
  ::  =flatten-battery: temporary function (XX)
  ::
  ::    $spec should have chapters but it doesn't.  So we flatten.
  ::
  ++  flatten-battery
    |=  batt=xbattery
    ^-  (map term key)
    =/  chapter-list  ~(tap by batt)
    |-  ^-  (map term key)
    ?~  chapter-list  ~
    (~(uni by q.q.i.chapter-list) $(chapter-list t.chapter-list))
--
