::
::  Here, try this:
::
::  > +hoon-printer :~  '*$@(? [a=* (unit $?(@ @ ~))])'
::                      '*$%([$a $?(^ @)] [$b ~])'
::                      '*(unit ?(%a %b))'
::                      '*(list ?(%a %b))'
::                      '*$%([%e @] [%j (list ~)])'
::                  ==
::
/?  310
!:
:-  %say
::
::  =<  render-hoon
::  =<  render-type
=<  compile-and-render-type
::  =<  render-all-hoons-referenced-inside-of-type
::
|%
::
+|  %entry-points-for-testing
::
::  Left-fold over a list.
::
++  foldl
   |*  [state=mold elem=mold]
   |=  [[st=state xs=(list elem)] f=$-([state elem] state)]
   ^-  state
   ?~  xs  st
   =.  st  (f st i.xs)
   $(xs t.xs, st st)
::
::  This is basically a `mapM` over a list using the State monad.
::
::  Another way to think about this is that it is the same as `turn`,
::  except that a state variable `st` is threaded through the
::  execution. The list is processed from left to right.
::
++  traverse-left
   |*  [a=mold b=mold s=mold]
   |=  [[xs=(list a) st=s] f=$-([a s] [b s])]
   ^-  [(list b) s]
   ?~  xs  [~ st]
   =^  r   st  (f i.xs st)
   =^  rs  st  $(xs t.xs, st st)
   [[r rs] st]
::
::  Same as `traverse-left` but executes state updates in reverse order.
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
::  Pretty-print a type given as a string.
::
++  compile-and-render-type
  |=  {^ {{tys=(list cord) ~} ~}}
  :-  %txt
  ^-  wain
  %-  zing
  %+  turn  tys
  |=  c=cord
  ^-  (list cord)
  =/  t=type  -:(ride -:!>(..zuse) c)
  =/  s=spec  %-  xray-image-to-spec
              %-  analyze-type
              t
  =/  p=plum  (spec-to-plum s)
  ^-  (list cord)
  ~(tall plume p)
::
::  Pretty-print a type.
::
++  render-type
  |=  {^ {{t=type ~} ~}}
  :-  %txt
  ^-  wain
  =/  s=spec  %-  xray-image-to-spec
              %-  decorate-xray-image-with-patterns
              %-  decorate-xray-image-with-shapes
              %-  xray-type
              t
  =/  p=plum  (spec-to-plum s)
  ~(tall plume p)
::
::  Pretty-print a hoon in tall mode using `plume`.
::
++  render-hoon
  |=  {^ {{demo=hoon ~} ~}}
  :-  %txt
  ^-  wain
  ~(tall plume (hoon-to-plum demo))
::
++  cat-patterns
  |=  xs=(list (unit pattern))
  ^-  (list pattern)
  ?~  xs    ~
  =/  more  $(xs t.xs)
  ?~  i.xs  more
  [u.i.xs more]
::
++  cat-errors
  |*  xs=(list (unit @))
  ^-  (unit @)
  |-
  ^-  (unit @)
  ?~  xs    ~
  ?^  i.xs  i.xs
  $(xs t.xs)
::
::  This is just a helper function for testing out this code.  It just digs
::  through a type and finds hoon values referenced within that type,
::  and then renders the result.
::
++  render-all-hoons-referenced-inside-of-type
  |=  {^ {{demo=type ~} ~}}
  :-  %txt
  ^-  wain
  ?.  ?=([%core *] demo)  [%zpzp ~]
  =*  tomes=(list tome)  ~(val by q.r.q.demo)
  =*  hoons=(list hoon)  (turn tomes |=(t=tome [%cltr ~(val by q.t)]))
  ~(tall plume (hoon-to-plum [%cltr hoons]))
::
+|  %utils
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
+|  %types
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
      (turn (unwrap-woof-tuple +.woof) hoon-to-plum)
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
  (turn hoon-list hoon-to-plum)
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
::  the last) when the match-list is rendered in wide mode.
::
++  matches-to-plum-list
  |=  =match=(list (pair spec hoon))
  ^-  (list plum)
  %-  add-trailing-commas-to-wide-form
  %+  turn  match-list
  |=  [=spec =hoon]
  ^-  (pair plum plum)
  [(spec-to-plum spec) (hoon-to-plum hoon)]
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
::  Render a hoon as a plum.  Given the helper functions above, this is
::  fairly straightforward.  It is a big-ass switch, though.
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
    ++  hn         hoon-to-plum
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
::  Render a hint to a plum.
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
::  Render a battery (basically a list of terms,hoon pairs to a plum).
::
++  battery-to-plum-list
  |=  =(map term hoon)
  ^-  (list plum)
  %+  turn  ~(tap by map)
  |=  [=term =hoon]
  =*  fmt  [wide=~ tall=`['' ~]]
  :+  %tree  fmt
  [term (hoon-to-plum hoon) ~]
::
::  XX Document this.
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
           [(hoon-to-plum p.spec) (turn q.spec ..$)]
    %bsbs  (core-spec-to-plum '$$' p.spec q.spec)
    %bsbr  (subtree (fixed '$|') $(spec p.spec) (hoon-to-plum q.spec) ~)
    %bscb  (hoon-to-plum p.spec)
    %bscl  :-  %sbrk
           :+  %tree
             [`[' ' `['[' ']']] `['$:' `['' '--']]]
           (turn `(list ^spec)`+.spec ..$)
    %bscn  (subtree (varying '$%' '==') (turn `(list ^spec)`+.spec ..$))
    %bsdt  (core-spec-to-plum '$.' p.spec q.spec)
    %bsld  (subtree (fixed '$<') $(spec p.spec) $(spec q.spec) ~)
    %bsbn  (subtree (fixed '$>') $(spec p.spec) $(spec q.spec) ~)
    %bshp  (subtree (fixed '$-') $(spec p.spec) $(spec q.spec) ~)
    %bskt  (subtree (fixed '$-') $(spec p.spec) $(spec q.spec) ~)
    %bsls  (subtree (fixed '$+') (stud-to-plum p.spec) $(spec q.spec) ~)
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
::  This is the pretty-printer.  Use the `flat` arm to render a plum
::  into a single line and use the `tall` arm to get a nice multi-line
::  rendering that switches to wide mode if there's enough space.
::
::  For details about how this works and what exactly it does in various
::  cases, take a look at the docs for `plum`, `plumfmt`, and at the
::  docs on the arms of this door.
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
        (weld text.this `tape`[' ' text.next])
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
          ?~(text.next text.i.render :(weld text.i.render "  " text.next))
        --
    ==
  --
+$  idx  @ud
+$  shape
  $@  $?  %atom  %cell  %noun  %void  %wide  ==
  $%  [%constant =atom]
      [%instance =atom]
      [%option =(map atom idx)]
      [%union =(map atom idx)]
      [%junction flat=idx deep=idx]
      [%conjunction wide=idx tall=idx]
      [%misjunction one=idx two=idx]
  ==
+*  battery  [item]  (map term (pair what (map term item)))
+$  recipe
  $%  [%direct =term]
      [%synthetic =term =(list idx)]
  ==
+$  pattern
  $@  ?(%hoon %manx %nock %path %plum %skin %specl %tape %tour %type %vase)
  $%  [%gate sample=idx product=idx]
      [%gear sample=idx context=idx =(battery idx)]
      [%list item=idx]
      [%tree item=idx]
      [%unit item=idx]
  ==
+$  data
  $@  ?(%noun %void)
  $%  [%atom =aura constant=(unit @)]
      [%cell head=idx tail=idx]
      [%core =garb xray=idx =(battery idx)]
      [%face face=$@(term tune) xray=idx]
      [%fork =(set idx)]
  ==
+$  xray
  $:  =idx
      =type
      data=(unit data)
      shape=(unit shape)
      pats=(set pattern)
      studs=(set stud)
      recipes=(set recipe)
      helps=(set help)
      loop=?
  ==
+$  image  (map idx xray) :: 0 is the top-level type.
::
++  xray-type
  |^  |=  ty=type
      ^-  image
      ::  %-  validate-image
      ::  %-  dedupe-image
      =/  res  (main ty *state)
      (build-image table.+.res)
  ::
  +$  table  (map type xray)
  +$  state  [count=@ud =table]
  ::
  ::  Given an `entry-table` (which we use to check if a given type is
  ::  an entry-point), produce a `loop-map`. In following analysis phases,
  ::  we will be traversing xrays and we need to lookup looks when we
  ::  come across them.  The `loop-map` structure encodes that.
  ::
  ++  build-image
    |=  t=table
    ^-  image
    %-  ~(gas by *image)
    %+  turn  ~(tap by t)
    |=  [type =xray]
    [idx.xray xray]
  ::
  ::  Create an new xray and put it in the xray table. If there's already
  ::  a stub xray under this type, replace it.  Otherwise, allocate a
  ::  new index and put it there.
  ::
  ++  with-new-xray
    |=  [ty=type d=(unit data) st=state]
    ^-  [idx state]
    ~&  [%with-new-xray ^-(cord ?@(ty ty -.ty)) ty d]
    =/  old  (~(get by table.st) ty)
    =^  idx  st
      ?^  old
        ~&  [%replacing-xray-at idx.u.old %with-type type.u.old]
        [idx.u.old st]
      =/  newidx  count.st
      =.  count.st  +(count.st)
      ~&  [%allocating-new-xray-at newidx ty]
      [newidx st]
    =/  res  `xray`[idx ty d ~ ~ ~ ~ ~ %.n]
    =.  table.st  (~(put by table.st) ty res)
    [idx st]
  ::
  ::  The main analysis code. This basically just calls out to other
  ::  helper functions based on which type of type this is.
  ::
  ++  main
    |=  [ty=type st=state]
    ^-  [idx state]
    ~&  ['main' ?@(ty ty -.ty) ty]
    =/  old  (~(get by table.st) ty)                    ::  don't loop
    ?^  old  [idx.u.old st]
    ?-  ty
      %void      (with-new-xray ty `^-(data ty) st)
      %noun      (with-new-xray ty `^-(data ty) st)
      [%atom *]  (with-new-xray ty `^-(data ty) st)
      [%cell *]  =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  hed=idx  st  $(ty p.ty)
                 =^  hed=idx  st  $(ty p.ty)
                 =^  tyl=idx  st  $(ty q.ty)
                 (with-new-xray ty `[%cell hed tyl] st)
      [%core *]  =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  d=data   st  (core [p.ty q.ty] st)
                 (with-new-xray ty `d st)
      [%face *]  =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  i=idx  st  $(ty q.ty)
                 (with-new-xray ty `[%face p.ty i] st)
      [%fork *]  =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  d=data   st  (fork p.ty st)
                 (with-new-xray ty `d st)
      [%hint *]  (hint p.ty q.ty st)                    ::  updates q.ty xray
      [%hold *]  (main ~(repo ut ty) st)
    ==
  ::
  +*  batt  [item]  (map term (pair what (map term item)))
  +*  chap  [item]  (pair term (pair what (map term item)))
  +*  arm   [item]  (pair term item)
  ::
  ::  Analyze a core.
  ::
  ++  core
    |=  [[=payload=type =coil] st=state]
    ^-  [data state]
    =^  payload-idx  st  (main payload-type st)
    =^  chapters=(list (chap idx))  st
      %+  (traverse-right (chap hoon) (chap idx) state)
        [~(tap by q.r.coil) st]
      |=  [c=(chap hoon) st=state]
      =^  l=(list (arm idx))  st
        ^-  [(list (arm idx)) state]
        %+  (traverse-right (arm hoon) (arm idx) state)
          [~(tap by q.q.c) st]
        |=  [a=(arm hoon) st=state]
        ^-  [(arm idx) state]
        =/  hold-type  [%hold [%core payload-type coil] q.a]
        =^  i=idx  st  (main hold-type st)
        [a(q i) st]
      =/  r  `(chap idx)`[p.c `what`p.q.c (~(gas by *(map term idx)) l)]
      [r st]
    =/  chaps   (~(gas by *(batt idx)) chapters)
    =/  d=data  [%core p.coil payload-idx chaps]
    [d st]
  ::
  :: XX This could be very expensive.  If this happens a lot, then we
  :: should just maintain the reverse table as well. Not hard.
  ::
  ++  xray-by-idx
    |=  [i=idx st=state]
    ^-  xray
    (~(got by (build-image table.st)) i)
  ::
  ::  Analyze a %hint type.
  ::
  ::    subject-type: subject of note
  ::    note: hint information
  ::    content-type: type of hinted content
  ::
  ++  hint
    |=  [[=subject=type =note] =payload=type st=state]
    ^-  [idx state]
    =^  i=idx  st  (main payload-type st)
    =/  x=xray  (xray-by-idx i st)
    =^  x  st
      ?-    -.note
          %help
        =.  helps.x  (~(put in helps.x) p.note)
        [x st]
          %know
        =.  studs.x  (~(put in studs.x) p.note)
        [x st]
          %made
        =^  recipe  st
          ?~  q.note  [[%direct p.note] st]
          =^  params=(list idx)  st
            |-
            ^-  [(list idx) state]
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
    =.  table.st  (~(put by table.st) type.x x)           ::  Update!
    [idx.x st]
  ::
  ::  +fork: convert a %fork $type to an $xray
  ::
  ::  set: set of union types
  ::
  ++  fork
    |=  [types=(set type) st=state]
    ^-  [data state]
    =^  xrays  st
      %+  (traverse-left type idx state)
        [~(tap in types) st]
      |=  [ty=type st=state]
      (main ty st)
    =/  d=data  [%fork (~(gas in *(set idx)) xrays)]
    [d st]
  --
::
++  decorate-xray-image-with-loops
  |=  img=image
  ^-  image
  =/  i=idx  0
  =/  trace=(set idx)  ~
  |-  ^-  image
  =/  x    (~(got by img) i)
  =/  dat  (need data.x)
  ?:  (~(has in trace) i)  (~(put by img) idx.x x(loop %.y))
  ?@  data.x  img
  =.  trace  (~(put in trace) i)
  ?-  -.dat
    %atom  img
    %cell  =.  img  $(i head.dat)
           $(i tail.dat)
    %core  =.  img  $(i xray.dat)
           %+  (foldl image idx)
             [img (battery-refs battery.dat)]
           |=  [img=image i=idx]
           ^$(img img, i i)
    %face  $(i xray.dat)
    %fork  %+  (foldl image idx)
             [img ~(tap in set.dat)]
           |=  [img=image i=idx]
           ^$(img img, i i)
  ==
::
++  battery-refs
  |=  b=(battery idx)
  ^-  (list idx)
  %-  zing
  %+  turn  ~(val by b)
  |=  [=what =(map term idx)]
  ^-  (list idx)
  ~(val by map)
::
++  decorate-xray-image-with-patterns
  |=  img=image
  ^-  image
  ~&  %+  sort  ~(val by img)
      |=  {x=xray y=xray}
      (lth idx.x idx.y)
  img ::  XX
::
++  decorate-xray-image-with-shapes
  |^  |=  img=image
      ^-  image
      %-  ~(gas by *image)
      %+  turn  ~(tap by img)
      |=  [i=idx x=xray]
      ^-  [idx xray]
      [i x(shape [~ (simple-shape img x)])]
  ::
  ++  simple-shape
    |=  [img=image x=xray]
    ^-  shape
    =/  dat  (need data.x)
    ?^  shape.x  u.shape.x
    ?@  dat  `shape`dat
    ?-  -.dat
      %atom  ?~  constant.dat  %atom
             [%constant u.constant.dat]
      %cell  =/  head-shape  $(x (~(got by img) head.dat))
             ?:  ?|  ?=(?(%cell %wide) head-shape)
                     ?=([?(%instance %union %junction %conjunction) *] head-shape)
                 ==
               %wide
             ?:  ?=([%constant *] head-shape)
               [%instance atom.head-shape]
             %cell
      %core  %cell
      %face  $(x (~(got by img) xray.dat))
      %fork  $(x (forge x ~(tap in set.dat)))
    ==
  ::
  ++  forge
    |=  [x=xray l=(list idx)]
    ^-  xray
    x(shape `%void)
  --
      ::      ?-  -.data.xray

      ::  ::
      ::  ::  -measure: calculate `shape` information on xrays.
      ::  ::
      ::  ++  measure
      ::    ::
      ::    ::  -require: produce best currently available shape
      ::    ::
      ::    ++  require
      ::      |=  [i=idx st=image]
      ::      ^-  shape
      ::      =/  x=xray  (~(got by st) i)
      ::      ?^  shape.x  u.shape.x
      ::      ?@  data.xray  `shape`data.xray
      ::      ?-  -.data.xray
      ::        %atom  ?~  constant.data.x  %atom
      ::               [%constant u.constant.data.xray]
      ::        %cell  =/  head-shape  $(xray head.data.xray)
      ::               ?:  ?|  ?=(?(%cell %wide) head-shape)
      ::                       ?=([?(%instance %union %junction %conjunction) *] head-shape)
      ::                   ==
      ::                 %wide
      ::               ?:  ?=([%constant *] head-shape)
      ::                 [%instance atom.head-shape]
      ::               %cell
      ::        %core  %cell
      ::        %face  ($ xray.data.x st)
      ::        %fork  ($ (forge ~(tap in set.data.x)))
      ::      ==
      ::    ::
      ::    ::  -original: produce $type of xray
      ::    ::
      ::    ++  original
      ::      |=  [i=idx st=image]
      ::      ^-  type
      ::      type:(~(got by st) i)
      ::    ::
      ::    ::  -fresh: produce trivial fork set if any
      ::    ::
      ::    ++  fresh
      ::      ^-  (unit (set ^xray))
      ::      ?@  xray  ~
      ::      ?@  data.xray  ~
      ::      ?.  ?=(%fork -.data.xray)  ~
      ::      ?^  entry-unit.meta.xray  ~
      ::      `set.data.xray
      ::    ::
      ::    ::  =join: compose union of two xrays, collapsing simple forks
      ::    ::
      ::    ++  join
      ::      |=  [this=^xray that=^xray]
      ::      ^-  ^xray
      ::      :-  (fork (original this) (original that) ~)
      ::      :-  *meta
      ::      ^-  data
      ::      :-  %fork
      ::      ^-  (set ^xray)
      ::      =/  this-set-unit  fresh(xray this)
      ::      =/  that-set-unit  fresh(xray that)
      ::      ?~  this-set-unit
      ::        ?~  that-set-unit
      ::          (sy this that ~)
      ::        (~(put in u.that-set-unit) this)
      ::      ?~  that-set-unit
      ::        (~(put in u.this-set-unit) that)
      ::      (~(uni in u.this-set-unit) u.that-set-unit)
      ::    ::
      ::    ::  =binary: compose binary junction/conjunction/misjunction
      ::    ::
      ::    ++  binary
      ::      |*  joint=?(%junction %conjunction %misjunction)
      ::      |=  [this=^xray that=^xray]
      ::      ^-  [shape ^xray]
      ::      [[joint this that] (join this that)]
      ::    ::
      ::    ::  -frame: computed shape with xray
      ::    ::
      ::    ++  frame
      ::      `[shape ^xray]`[require xray]
      ::    ::
      ::    ::  =merge: merge two xrays intelligently, using shape
      ::    ::
      ::    ++  merge
      ::      |=  [this=^xray that=^xray]
      ::      ^-  ^xray
      ::      ?:  ?&(?=(^ this) =(%void type.this))  that
      ::      ?:  ?&(?=(^ that) =(%void type.that))  this
      ::      =+  (combine frame(xray this) frame(xray that))
      ::      ?@(-> -> ->(shape-unit.meta `-<))                                :: TODO Wtf is this?
      ::    ::
      ::    ::  =collate: merge option maps
      ::    ::
      ::    ++  collate
      ::      |=  [thick=(map atom ^xray) thin=(map atom ^xray)]
      ::      =/  list  ~(tap by thin)
      ::      |-  ^-  (map atom ^xray)
      ::      ?~  list  thick
      ::      =/  item  (~(get by thick) p.i.list)
      ::      %=    $
      ::          list  t.list
      ::          thick
      ::        %+  ~(put by thick)
      ::          p.i.list
      ::        ?~(item q.i.list (merge u.item q.i.list))
      ::      ==
      ::    ::
      ::    ::  =forge: combine list of shape-described xrays
      ::    ::
      ::    ::  forge :: [XRay] -> XRay
      ::    ::  forge = foldl' merge [%void *meta %void]
      ::    ::
      ::    ++  forge
      ::      |=  =(list ^xray)
      ::      =/  new-xray  `^xray`[%void *meta %void]
      ::      |-  ^-  ^xray
      ::      ?~  list  new-xray
      ::      $(list t.list, new-xray (merge i.list new-xray))
      ::    ::
      ::    ::  =combine: combine shape-described xrays
      ::    ::
      ::    ++  combine
      ::      |=  [this=[=shape =^xray] that=[=shape =^xray]]
      ::      ^-  [shape ^xray]
      ::      ?:  =(this that)  this
      ::      ?@  shape.this
      ::        ?^  shape.that  $(this that, that this)
      ::        :_  (join xray.this xray.that)
      ::        ?:  =(shape.this shape.that)  shape.this
      ::        ?:  ?=(%void shape.this)  shape.that
      ::        ?:  ?=(%void shape.that)  shape.this
      ::        ?:  |(?=(%noun shape.this) ?=(%noun shape.that))  %noun
      ::        ?-  shape.this
      ::          %atom  [%junction xray.this xray.that]
      ::          %cell  ?:  ?=(%wide shape.that)
      ::                   %cell
      ::                 [%junction xray.that xray.this]
      ::          %wide  ?:  ?=(%cell shape.that)
      ::                   %cell
      ::                 [%junction xray.that xray.this]
      ::        ==
      ::      ?@  shape.that :: cell wide
      ::        ?:  ?=(%void shape.that)  this
      ::        ?:  ?=(%noun shape.that)  that
      ::        ?:  ?=(%atom shape.that)
      ::          ?+  -.shape.this
      ::                       ((binary %misjunction) xray.that xray.this)
      ::            %instance  ((binary %junction) xray.that xray.this)
      ::            %union     ((binary %junction) xray.that xray.this)
      ::            %junction  %+  (binary %junction)
      ::                         (merge xray.that flat.shape.this)
      ::                       deep.shape.this
      ::          ==
      ::        ::
      ::        ::  At this point, shape.that is either %cell, %wide, %union,
      ::        ::  or some kind of junction.
      ::        ::
      ::        ?+    -.shape.this
      ::                       ((binary %misjunction) xray.this xray.that)
      ::            %constant  ((binary %junction) xray.this xray.that)
      ::            %option    ((binary %junction) xray.this xray.that)
      ::            %junction  %+  (binary %junction)
      ::                         flat.shape.this
      ::                       (merge xray.that deep.shape.this)
      ::        ==
      ::      ?:  |(?=(%misjunction -.shape.this) ?=(%misjunction -.shape.that))
      ::        ((binary %misjunction) xray.this xray.that)
      ::      ?-    -.shape.this
      ::          %constant
      ::        ?-  -.shape.that
      ::          %constant     :_  (join xray.this xray.that)
      ::                        :-  %option
      ::                        %+  collate
      ::                          [[atom.shape.this xray.this] ~ ~]
      ::                        [[atom.shape.that xray.that] ~ ~]
      ::          %instance     ((binary %junction) xray.this xray.that)
      ::          %option       ((binary %misjunction) xray.this xray.that)
      ::          %union        ((binary %junction) xray.this xray.that)
      ::          %junction     %+  (binary %junction)
      ::                          (merge xray.this flat.shape.that)
      ::                        deep.shape.that
      ::          %conjunction  ((binary %junction) xray.this xray.that)
      ::        ==
      ::      ::
      ::          %instance
      ::        ?+  -.shape.that  $(this that, that this)
      ::          %instance     :_  (join xray.this xray.that)
      ::                        :-  %union
      ::                        %+  collate
      ::                          [[atom.shape.this xray.this] ~ ~]
      ::                        [[atom.shape.that xray.that] ~ ~]
      ::          %option       ((binary %junction) xray.this xray.that)
      ::          %union        :_  (join xray.this xray.that)
      ::                        :-  %union
      ::                        %+  collate
      ::                          map.shape.that
      ::                        [[atom.shape.this xray.this] ~ ~]
      ::          %junction     %+  (binary %junction)
      ::                          flat.shape.that
      ::                        (merge xray.this deep.shape.that)
      ::          %conjunction  %+  (binary %junction)
      ::                          wide.shape.that
      ::                        (merge xray.this tall.shape.that)
      ::        ==
      ::      ::
      ::          %option
      ::        ?+  -.shape.that  $(this that, that this)
      ::          %option       :_  (join xray.this xray.that)
      ::                        :-  %option
      ::                        (collate map.shape.this map.shape.that)
      ::          %union        ((binary %junction) xray.this xray.that)
      ::          %junction     %+  (binary %junction)
      ::                          (merge xray.this flat.shape.that)
      ::                        deep.shape.that
      ::          %conjunction  ((binary %junction) xray.this xray.that)
      ::        ==
      ::      ::
      ::          %union
      ::        ?+  -.shape.that  $(this that, that this)
      ::          %union        :_  (join xray.this xray.that)
      ::                        :-  %union
      ::                        (collate map.shape.this map.shape.that)
      ::          %junction     %+  (binary %junction)
      ::                          flat.shape.that
      ::                        (merge xray.this deep.shape.that)
      ::          %conjunction  %+  (binary %conjunction)
      ::                          wide.shape.that
      ::                        (merge xray.this tall.shape.that)
      ::        ==
      ::      ::
      ::          %junction
      ::        ?+  -.shape.that  $(this that, that this)
      ::          %junction     %+  (binary %junction)
      ::                          (merge flat.shape.this flat.shape.that)
      ::                        (merge deep.shape.this deep.shape.that)
      ::          %conjunction  %+  (binary %junction)
      ::                          flat.shape.this
      ::                        (merge deep.shape.this xray.that)
      ::        ==
      ::      ::
      ::          %conjunction
      ::        ?+  -.shape.that  $(this that, that this)
      ::          %conjunction  %+  (binary %conjunction)
      ::                          (merge wide.shape.this wide.shape.that)
      ::                        (merge tall.shape.this tall.shape.that)
      ::        ==
      ::      ==
      ::    ::
      ::    ::  Analyze an xray.
      ::    ::
      ::    ++  analyze
      ::      =<  remember
      ::      =|  loop-set=(set index)
      ::      |-
      ::      ^+  +>
      ::      ::  XX This causes a bail fail:
      ::      ::
      ::      ::      ~&  ?@  xray  ~  type.xray
      ::      ::
      ::      ::
      ::      ::  If our xray is a loop reference, analyze the xray that the
      ::      ::  reference resolves to and replace that slot in the loop map
      ::      ::  with the analyzed version
      ::      ::
      ::      ?@  xray
      ::        =/  =zray  (~(got by img) xray)
      ::        ?^  shape-unit.meta.zray  +>+
      ::        +>+(img img:complete:remember:$(xray zray))
      ::      ::
      ::      :: If we've already measured this xray, do nothing
      ::      ::
      ::      ?^  shape-unit.meta.xray  +>
      ::      ::
      ::      ::  Analyze the xrays in the recipes as well.
      ::      ::
      ::      =.  recipe-set.meta.xray
      ::        ?~  recipe-set.meta.xray  recipe-set.meta.xray
      ::        ^-  (set recipe)
      ::        %-  ~(gas in *(set recipe))
      ::        ^-  (list recipe)
      ::        =/  recipes=(list recipe)
      ::          ~(tap in ^-((set recipe) recipe-set.meta.xray))
      ::        =^  finished-items  img
      ::          ^-  [(list recipe) loop-map]
      ::          %+  (traverse-right recipe recipe loop-map)
      ::            [recipes img]
      ::          |=  [r=recipe st=loop-map]
      ::          ?-    -.r
      ::              %direct
      ::            [r st]
      ::              %synthetic
      ::            =^  new-xrays  st
      ::              %+  (traverse-right ^xray ^xray loop-map)
      ::                [list.r st]
      ::              |=  [x=^xray st=loop-map]
      ::              complete:remember:^^$(xray x, img st)
      ::            [r(list new-xrays) st]
      ::          ==
      ::        finished-items
      ::      ::
      ::      ::  If this is a loop entry point, then we're already in the process
      ::      ::  of analyzing this or we're not. If we are, then the index will
      ::      ::  be in `loop-set`: do nothing. Otherwise, store the index in the
      ::      ::  `loop-set` and continue.
      ::      ::
      ::      =*  ent  entry-unit.meta.xray
      ::      ?:  ?&  ?=(^ ent)  (~(has in loop-set) u.ent)  ==
      ::        +>
      ::      =.  loop-set  ?~  ent  loop-set
      ::                    (~(put in loop-set) u.ent)
      ::      ::
      ::      ::  If data is %noun or %void, then shape will also be %noun or %void.
      ::      ::
      ::      ?@  data.xray
      ::        =/  shape=shape  data.xray
      ::        +>+(shape-unit.meta.xray `shape)
      ::      ::
      ::      ::  Otherwise, switch on the tag.
      ::      ::
      ::      ?-    -.data.xray
      ::          %atom
      ::        +>(shape-unit.meta.xray `require)
      ::          %cell
      ::        =^  head  img  complete:remember:$(xray head.data.xray)
      ::        =^  tail  img  complete:remember:$(xray tail.data.xray)
      ::        =.  head.data.xray  head
      ::        =.  tail.data.xray  tail
      ::        +>+>(shape-unit.meta.xray `require)
      ::          %core
      ::        =^  payload  img  complete:remember:$(xray xray.data.xray)
      ::        =^  chapters  img
      ::          =*  chap  (pair term (pair what (map term ^xray)))
      ::          %+  (traverse-right chap chap loop-map)
      ::            [~(tap by battery.data.xray) img]
      ::          |=  [=chap st=loop-map]
      ::          =-  :_  ->
      ::              :+  p.chap
      ::                `what`p.q.chap
      ::              (~(gas by *(map term ^xray)) -<)
      ::          =*  arm   (pair term ^xray)
      ::          %+  (traverse-right arm arm loop-map)
      ::            [~(tap by q.q.chap) st]
      ::          |=  [=arm st=loop-map]
      ::          =^  new-xray  st  complete:remember:^^$(xray q.arm)
      ::          [arm(q new-xray) st]
      ::        =.  xray.data.xray     payload
      ::        =.  battery.data.xray  (~(gas by *battery) chapters)
      ::        +>+>(shape-unit.meta.xray `%cell)
      ::          %face
      ::        =^  body  img  complete:remember:$(xray xray.data.xray)
      ::        =.  xray.data.xray  body
      ::        +>+(shape-unit.meta.xray `require(xray body))
      ::          %fork
      ::        =^  list  img
      ::          =/  list  ~(tap in set.data.xray) :: list of possible types.
      ::          |-  ^-  [(^list ^xray) loop-map]
      ::          ?~  list  [~ img]
      ::          =^  this  img  complete:remember:^$(xray i.list)
      ::          =^  rest  img  $(list t.list)
      ::          [[this rest] img]
      ::        =/  new-xray  (forge list)
      ::        ?@  new-xray  +>+>(xray new-xray)
      ::        =.  new-xray  new-xray(entry-unit.meta entry-unit.meta.xray)
      ::        =.  new-xray  new-xray(recipe-set.meta recipe-set.meta.xray)
      ::        +>+>(xray new-xray)
      ::      ==
      ::    --
++  analyze-type
  |=  t=type
  ^-  image
  %-  decorate-xray-image-with-patterns
  %-  decorate-xray-image-with-shapes
  %-  decorate-xray-image-with-loops
  %-  xray-type
  t
::
::  -xray-image-to-spec: convert to spec
::
++  xray-image-to-spec
  |=  img=image
  ^-  spec
  ::
  =|  st=(set idx)
  =|  trace=(set idx)
  ::
  |^  (xray-to-spec 0)
  ::
  ++  recipe-to-spec
    |=  r=recipe
    ^-  spec
    ?-  -.r
      %direct     [%like [term.r ~] ~]
      %synthetic  [%make [%limb term.r] (turn list.r xray-to-spec)]
    ==
  ::
  ++  wrap-with-loop-binding
    |=  [i=idx s=spec]
    ^-  spec
    =/  nm  (synthetic i)
    [%bsbs [%loop nm] [[nm s] ~ ~]]
  ::
  ::  If we see a reference to a type that we're already processing,
  ::  then we simply generate a reference to it and make a not that we
  ::  referenced this type in a loop.
  ::
  ++  xray-to-spec
    |=  i=idx
    ^-  spec
    =/  x=xray  (~(got by img) i)
    =/  d  (need data.x)
    =/  trace=(set idx)  ~
    ?:  (~(has in trace) i)  [%loop (synthetic i)]
    =.  trace  (~(put in trace) i)
    ?^  recipes.x
      (recipe-to-spec n.recipes.x)
    (build-spec-for-xray x)

  ++  build-spec-for-xray
    |=  x=xray
    !!
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
      ::      ::
      ::      ::  If this is a loop, then add ourselves to the loop set and recurse,
      ::      ::  pretending that this is actually not a loop. Given the result of that,
      ::      ::  produce a spec that looks something like:
      ::      ::
      ::      ::      $$  alf  ++  alf  spec  --
      ::      ::
      ::      ?^  entry-unit.meta.xray
      ::        =/  idx  u.entry-unit.meta.xray
      ::        =/  new-loop-set  (~(put in loop-set) idx)
      ::        =/  =spec  $(loop-set new-loop-set, entry-unit.meta.xray ~)
      ::        :+  %bsbs  `^spec`[%loop (synthetic idx)]
      ::        [[(synthetic idx) spec] ~ ~]
      ::      ::
      ::      ?@  data.xray  [%base data.xray]
      ::      ?-  -.data.xray
      ::        %atom  ?~  constant-unit.data.xray
      ::                 [%base %atom aura.data.xray]
      ::               ?:  &(=(%n aura.data.xray) =(`@`0 u.constant-unit.data.xray))
      ::                 [%base %null]
      ::               [%leaf aura.data.xray u.constant-unit.data.xray]
      ::        %cell  =/  head  $(xray head.data.xray)
      ::               =/  tail  $(xray tail.data.xray)
      ::               ?:  &(=([%base %noun] head) =([%base %noun] tail))
      ::                 [%base %cell]
      ::               ?:  ?=(%bscl -.tail)
      ::                 [%bscl head +.tail]
      ::               [%bscl head tail ~]
      ::        %core  =/  payload  $(xray xray.data.xray)
      ::               =/  battery
      ::                 ^-  (map term spec)
      ::                 %-  ~(run by (flatten-battery battery.data.xray))
      ::                 |=  =^xray
      ::                 ^$(xray xray)
      ::               ?-  r.garb.data.xray
      ::                 %lead  [%bszp payload battery]
      ::                 %gold  [%bsdt payload battery]
      ::                 %zinc  [%bstc payload battery]
      ::                 %iron  [%bsnt payload battery]
      ::               ==
      ::        %face  =/  =spec  $(xray xray.data.xray)
      ::               ::
      ::               ::  We discard the $tune case, a $spec can't express it.
      ::               ::
      ::               ::  XX: should exist a %misjunction spec
      ::               ::
      ::               ?^(face.data.xray spec [%bsts face.data.xray spec])
      ::        %fork  =/  =shape  (need shape-unit.meta.xray)
      ::               |^  ?+  shape
      ::                       ~&  %unexpected-fork-shape  !!
      ::                     [%option *]       [%bswt choices]
      ::                     [%union *]        [%bscn choices]
      ::                     %cell             [%bswt choices] :: XX bskt?
      ::                     %noun             [%bswt choices]
      ::                     [%misjunction *]  [%bswt choices]
      ::                     [%junction *]     :+  %bsvt
      ::                                         ^$(xray flat.shape)
      ::                                       ^$(xray deep.shape)
      ::                     [%conjunction *]  :+  %bskt
      ::                                         ^$(xray wide.shape)
      ::                                       ^$(xray tall.shape)
      ::                   ==
      ::               ::
      ::               ++  choices
      ::                 ^-  [i=spec t=(list spec)]
      ::                 =-  ?>(?=(^ -) -)
      ::                 (turn ~(tap in set.data.xray) |=(=^xray ^^$(xray xray)))
      ::               --
      ::      ==
      ::    ::
      ::    ::  =flatten-battery: temporary function (XX)
      ::    ::
      ::    ::    $spec should have chapters but it doesn't.  So we flatten.
      ::    ::
      ::    ++  flatten-battery
      ::      |=  =battery
      ::      =/  chapter-list  ~(tap by battery)
      ::      |-  ^-  (map term ^xray)
      ::      ?~  chapter-list  ~
      ::      (~(uni by q.q.i.chapter-list) $(chapter-list t.chapter-list))
--
