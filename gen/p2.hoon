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
/-  naon
:-  %say
!:
::
::  =<  render-hoon
::  =<  render-type
=<  render-vase
::  compile-and-render-type
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
++  is-kernel
  |=  ty=type
  ^-  ?
  ::
  ?.  ?=([%core *] ty)  %.n
  ::
  =/  arms=(list term)  (battery-arms q.r.q.ty)
  =/  armset=(set term)  (~(gas in *(set term)) arms)
  ::
  ?:  =(-:!>(..noun.naon) ty)
    ::  ~&  'is-zuse'
    %.y
  ::
  ?:  (~(has in armset) 'sign-arvo')
    ::  ~&  'has sign-arvo'
    %.y
  ::
  ?:  (~(has in armset) 'add')
    ::  ~&  'has add'
    %.y
  ::
  %.n
::
::
++  battery-arms
  |=  =(battery hoon)
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
++  kernel-type
  ^-  type
  -:!>(..zuse)
::
::  Pretty-print a value given as a string.
::
++  render-vase
  |=  {^ {{=vase ~} ~}}
  :-  %txt
  ^-  wain
  ~(tall plume (vase-to-plum vase))
::
::  Pretty-print a value given as a string.
::
++  vase-to-plum
  |=  v=vase
  ^-  plum
  =/  t=type   p.v
  =/  n=*      q.v
  =/  i=image  (analyze-type t)
  (xray-noun-to-plum i n)
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
  =/  t=type  -:(ride -:!>(naon) c)
  ~(tall plume (type-to-plum t))
::
::  Pretty-print a type.
::
++  render-type
  |=  {^ {{=type ~} ~}}
  :-  %txt
  ^-  wain
  ~(tall plume (type-to-plum type))
::
::  Pretty-print a type.
::
++  type-to-plum
  |=  t=type
  ^-  plum
  (spec-to-plum (xray-image-to-spec (analyze-type t)))
::
::  Pretty-print a hoon in tall mode using `plume`.
::
++  render-hoon
  |=  {^ {{demo=hoon ~} ~}}
  :-  %txt
  ^-  wain
  ~(tall plume (hoon-to-plum 999 demo))
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
  ~(tall plume (hoon-to-plum 999 [%cltr hoons]))
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
::  the last) when the match-list is rendered in wide mode.
::
++  matches-to-plum-list
  |=  =match=(list (pair spec hoon))
  ^-  (list plum)
  %-  add-trailing-commas-to-wide-form
  %+  turn  match-list
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
      %+  hoon-to-plum  (dec maxdepth)
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
      (hoon-to-plum 999 q.hint)
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
  [term (hoon-to-plum 999 hoon) ~]
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
::  Shape = Noun | Void
::        | Atom | Cnst
::        | Cell
::        | Junc | Union
+$  shape  ?(%void %noun %atom %cell %junc)
::
+$  role
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
      fork=(unit (pair idx idx))
      role=(unit role)
      pats=(set pattern)
      studs=(set stud)
      recipes=(set recipe)
      helps=(set help)
      shape=(unit shape)
      loop=?
  ==
::
+$  image  (map idx xray) :: 0 is the top-level type.
::
++  tape-to-plum
  |=  =tape
  ^-  plum
  (simple-wide-plum '"' '' '"' `(list plum)`tape)
::
++  xray-noun-to-plum
  |=  [img=image n=*]
  ^-  plum
  |^  (main 0 n)
  ::
  ++  main
    |=  [i=idx n=*]
    ^-  plum
    ~&  ['xray-noun-to-plum' i n]
    =/  x=xray  (~(got by img) i)
    ~&  x
    =/  ps=(list pattern)  ~(tap in pats.x)
    ~&  ps
    ?~  ps  (render-with-data (need data.x) n)
    (render-with-pattern i.ps n)                        ::  XX What to do if two
                                                        ::  patterns match?
                                                        ::  Seems useless! Maybe
                                                        ::  the pattern
                                                        ::  detection code
                                                        ::  should be forced to
                                                        ::  just choose
                                                        ::  one pattern.
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
    |=  [elt=idx noun=*]
    ^-  plum
    ?~  noun  '~'
    =/  ns=(list *)     (tree-noun-to-list noun)
    =/  ps=(list plum)  (turn ns |=(n=* (main elt n)))
    =/  elems=plum      (rune-to-plum ':~' `'==' `['~[' ' ' ']'] ps)
    (rune-to-plum '%-' ~ `['(' ' ' ')'] ~['tree' elems])
  ::
  ++  render-list
    |=  [elt=idx noun=*]
    ^-  plum
    ?~  noun  '~'
    =/  ns=(list *)     (noun-to-list noun)
    =/  ps=(list plum)  (turn ns |=(n=* (main elt n)))
    (rune-to-plum ':~' `'==' `['~[' ' ' ']'] ps)
  ::
  ++  render-unit
    |=  [elt=idx noun=*]
    ^-  plum
    ?~  noun  '~'
    (pair-plum '~' (main elt +:n))
  ::
  ++  pair-plum
    |=  [x=plum y=plum]
    ^-  plum
    (rune-to-plum ':-' ~ `['[' ' ' ']'] ~[x y])
  ::
  ++  render-atom
    |=  [=aura atom=@]
    ^-  plum
    ~&  ['render-atom' aura atom]
    (scot aura atom)
  ::
  ++  render-const
    |=  [=aura const=@ =atom]
    ^-  plum
    ~&  'render-const'
    (cat 3 '%' (scot aura atom))
  ::
  ++  render-noun  ::  XX Where is the existing code for doing this?
    |=  [n=*]
    ^-  plum
    ?@  n  (render-atom 'ud' n)
    ~&  'render-const'
    (pair-plum (render-noun -:n) (render-noun +:n))
  ::
  ++  render-with-data
    |=  [d=data n=*]
    ^-  plum
    ~&  ['render-with-data' d n]
    ?-  d
      %void      '!!'
      %noun      (render-noun n)
      [%core *]  '%core'                                ::  XX TODO
      [%cell *]  (pair-plum (main head.d -:n) (main tail.d +:n))
                                                        ::  XX Handle
                                                        ::  n-ary tuples.
      [%atom *]  ?^  n  !!
                 ?~  constant.d  (render-atom aura.d n)
                 (render-const aura.d u.constant.d n)
      [%face *]  (main xray.d n)
      [%fork *]  '%fork'                                ::  [%fork =(set idx)]
    ==
  ::
  ++  render-with-pattern
    |=  [p=pattern n=*]
    ^-  plum
    ~&  ['render-with-pattern' p n]
    ?-  p
      %hoon      (hoon-to-plum 999 (hoon n))
      %manx      '%manx'                                ::  XX Implement
      %nock      '%nock'                                ::  XX Implement
      %path      '%path'                                ::  XX Implement
      %plum      (plum n)
      %skin      '%skin'                                ::  XX Implement
      %specl     '%specl'                               ::  XX Implement
      %tape      (tape-to-plum (tape n))
      %tour      '%tour'                                ::  XX Implement
      %type      (type-to-plum (type n))                ::  XX Can't hard types
      %vase      (vase-to-plum (vase n))                ::  XX Test this.
      [%gate *]  '%gate'                                ::  XX Implement
      [%gear *]  '%gear'                                ::  XX Implement
      [%list *]  (render-list item.p n)
      [%tree *]  (render-tree item.p n)
      [%unit *]  (render-unit item.p n)
    ==
  --
::
++  xray-type
  |=  [max-depth=@ max-size=@]
  |^  |=  ty=type
  ^-  image
      ::  %-  validate-image
      ::  %-  dedupe-image
      =/  res  (main ty *state)
      (build-image table.+.res)
  ::
  +$  table  (map type xray)
  +$  state  [count=@ud =table depth=@]
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
    ::  ~&  [%with-new-xray ^-(cord ?@(ty ty -.ty)) ty d]
    =/  old  (~(get by table.st) ty)
    =^  idx  st
      ?^  old
        ::  ~&  [%replacing-xray-at idx.u.old %with-type type.u.old]
        [idx.u.old st]
      =/  newidx  count.st
      =.  count.st  +(count.st)
      ::  ~&  [%allocating-new-xray-at newidx ty]
      [newidx st]
    =/  res  `xray`[idx ty d ~ ~ ~ ~ ~ ~ ~ %.n]
    =.  table.st  (~(put by table.st) ty res)
    [idx st]
  ::
  ::  The main analysis code. This basically just calls out to other
  ::  helper functions based on which type of type this is.
  ::
  ++  main
    |=  [ty=type st=state]
    ^-  [idx state]
    =/  old  (~(get by table.st) ty)                    ::  don't loop
    ::
    ::  ~&  ['main' depth.st ?@(ty ty -:ty)]
    ::  ~&  ~(tall plume (simple-type-to-plum ty 10))
    ::
    ?^  old  [idx.u.old st]
    ::
    ::  Track recursion depth.
    ::
    %-  |=([i=idx s=state] [i s(depth (dec depth.s))])
    =.  depth.st  +(depth.st)
    ::
    ?:  (gte depth.st max-depth)
      (with-new-xray ty `[%atom 'tas' `%deep] st)
    ?:  (gte count.st max-size)
      (with-new-xray ty `[%atom 'tas' `%wide] st)
    ::
    ?-  ty
      %void      (with-new-xray ty `^-(data ty) st)
      %noun      (with-new-xray ty `^-(data ty) st)
      [%atom *]  (with-new-xray ty `^-(data ty) st)
      [%cell *]  =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  hed=idx  st  $(ty p.ty)
                 =^  hed=idx  st  $(ty p.ty)
                 =^  tyl=idx  st  $(ty q.ty)
                 (with-new-xray ty `[%cell hed tyl] st)
      [%core *]  =/  arms=(list term)  (battery-arms q.r.q.ty)
                 =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  d=data   st  (xray-core [p.ty q.ty] st)
                 (with-new-xray ty `d st)
      [%face *]  =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  i=idx  st  $(ty q.ty)
                 (with-new-xray ty `[%face p.ty i] st)
      [%fork *]  =^  res=idx  st  (with-new-xray ty ~ st)
                 =^  d=data   st  (fork p.ty st)
                 (with-new-xray ty `d st)
      [%hint *]  (hint p.ty q.ty st)                    ::  updates q.ty xray
      [%hold *]  ::  ~&  'eval %hold type'
                 ::  ~&  'what is the type of this hoon:'
                 ::  ~&  `wain`~(tall plume (hoon-to-plum q.ty))
                 ::  ~&  q.ty
                 ::  ~&  'with a context of type?'
                 ::  ~&  `wain`~(tall plume (simple-type-to-plum p.ty 10))
                 ::  ~&  '<repo>'
                 =^  res  st  (main ~(repo ut ty) st)
                 ::  ~&  '</repo>'
                 [res st]
    ==
  ::
  ++  sexp-plum
    |=  [sym=cord kids=(list plum)]
    ^-  plum
    =/  head=cord     (cat 3 '(' sym)
    =/  headspc=cord  (cat 3 head ' ')
    =/  symcol=cord  (cat 3 sym ':')
    =/  fmt=plumfmt   [[~ ' ' [~ headspc ')']] [~ symcol [~ '' '']]]
    ?~  kids  (cat 3 '(' (cat 3 sym ')'))
    [%sbrk [%tree fmt kids]]
  ::
  ++  simple-type-to-plum
    =/  armsfmt=plumfmt  [[~ ' ' [~ '(' ')']] ~]
    ::
    |^  main
    ::
    ++  arms
      |=  =coil
      ^-  plum
      =/  arms  (battery-arms q.r.coil)
      =.  arms  (turn arms |=(c=cord ?:(=('' c) '$' c)))
      ?:  (gte (lent arms) 50)  'KERNEL'
      (sexp-plum 'arms' (chapters-to-plum-list q.r.coil))

    ++  main
      |=  [ty=type maxdepth=@ud]
      ^-  plum
      ?:  =(0 maxdepth)  'DEEP'
      =/  d  (dec maxdepth)
      ?-  ty
        %void      '!!'
        %noun      '*'
        [%atom *]  (sexp-plum 'atom' p.ty ?~(q.ty '~' (scot %ud u.q.ty)) ~)
        [%cell *]  (sexp-plum 'cons' (main p.ty d) (main q.ty d) ~)
        [%core *]  =/  payload  (sexp-plum 'payload' (main p.ty d) ~)
                   (sexp-plum 'core' (arms q.ty) payload ~)
        [%face *]  (sexp-plum 'face' (type-face-to-plum p.ty) (main q.ty d) ~)
        [%fork *]  =/  forks  %+  turn  ~(tap in p.ty)  |=(t=type (main t d))
                   (sexp-plum 'fork' forks)
        [%hint *]  (sexp-plum 'hint' 'hint' (main q.ty d) ~)
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
  ++  xray-arm
    |=  [st=state =payload=type =coil x=(arm hoon)]
    ^-  [(arm idx) state]
    =.  r.p.coil  %gold
    =.  q.p.coil  %dry
    ~&  'XRAY-ARM'
    ~&  'hoon'
    ~&  ~(tall plume (hoon-to-plum 999 q.x))
    ~&  'payload-type'
    ~&  ~(tall plume (simple-type-to-plum payload-type 10))
    ~&  'coil-context'
    ~&  ~(tall plume (simple-type-to-plum q.coil 10))
    =/  thunk  [%hold [%core payload-type coil] q.x]
    =^  i=idx  st  (main thunk st)
    [x(q i) st]
  ::
  ::  Analyze a core.
  ::
  ++  xray-core
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
        |=  [=(arm hoon) st=state]
        (xray-arm st payload-type coil arm)
      =/  r  `(chap idx)`[p.c `what`p.q.c (~(gas by *(map term idx)) l)]
      [r st]
    =/  chaps  (~(gas by *(batt idx)) chapters)
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
  ?@  dat  img
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
++  trace-xray-image
  |=  img=image
  ^-  image
  ~&  %+  sort  ~(val by img)
      |=  [x=xray y=xray]
      (lth idx.x idx.y)
  img
::
++  decorate-xray-image-with-patterns
  |=  img=image
  ^-  image
  |^  %-  ~(gas by *image)
      %+  turn  ~(tap by img)
      |=  [i=idx x=xray]
      ^-  [idx xray]
      [i x(pats (xray-pats img idx.x))]
  ::
  ++  type-nests-under
    |=  t1=type
    ^-  $-(type ?)
    |=  t2=type
    ^-  ?
    (~(nest ut t1) | t2)
  ::
  ++  is-hoon  (type-nests-under -:!>(*hoon))
  ++  is-manx  (type-nests-under -:!>(*manx))
  ++  is-nock  (type-nests-under -:!>(*nock))
  ++  is-plum  (type-nests-under -:!>(*plum))
  ++  is-skin  (type-nests-under -:!>(*skin))
  ::  ++  is-path  (type-nests-under -:!>(*path))
  ::
  ++  reflect  ~(got by img)
  ++  is-nil
    |=  i=idx
    ^-  ?
    =/  =data  (need data:(reflect i))
    ?+  data  %.n
      [%atom *]  =(data [%atom ~.n `0])
      [%face *]  $(i xray.data)
    ==
  ::
  ++  is-atom-with-aura
    |=  [c=cord i=idx]
    ^-  ?
    =/  =data  (need data:(reflect i))
    ?+  data  %.n
      [%atom *]  =(data [%atom aura=c constant-unit=~])
      [%face *]  $(i xray.data)
    ==
  ::
  ++  unit-of-what
    |=  =input=xray
    ^-  (unit idx)
    =/  input-idx=idx  idx.input-xray
    =/  indata=data    (need data.input-xray)
    ?.  ?=([%fork *] indata)  ~
    =/  branches  ~(tap in set.indata)
    ?.  ?=([* * ~] branches)  ~
    =/  nil   i.branches
    =/  node  i.t.branches
    |-
    ?:  (is-nil node)  $(node nil, nil node)
    ?.  (is-nil nil)  ~
    =/  node-data=data  (need data:(reflect node))
    ?.  ?=([%cell *] node-data)  ~
    ?.  (is-nil head.node-data)  ~
    =/  elem-data  (need data:(reflect tail.node-data))
    ?.  ?=([%face *] elem-data)  ~
    `xray.elem-data
  ::
  ++  tree-of-what
    |^  |=  =input=xray
        ^-  (unit idx)
        ?.  loop.input-xray  ~
        =/  input-idx=idx  idx.input-xray
        =/  indata=data    (need data.input-xray)
        ?.  ?=([%fork *] indata)  ~
        =/  branches  ~(tap in set.indata)
        ?.  ?=([* * ~] branches)  ~
        =/  nil   i.branches
        =/  node  i.t.branches
        |-
        ?:  (is-nil node)  $(node nil, nil node)
        ?.  (is-nil nil)  ~
        =/  node-data=data  (need data:(reflect node))
        ?.  ?=([%cell *] node-data)  ~
        ?.  (is-references-to-tree input-idx tail.node-data)  ~
        =/  elem-data  (need data:(reflect head.node-data))
        ?.  ?=([%face *] elem-data)  ~
        ::  ~&  type.input-xray
        `xray.elem-data
    ::
    ++  is-references-to-tree
      |=  [=loop=idx =cell=idx]
      ^-  ?
      =/  =data  (need data:(reflect cell-idx))
      ?.  ?=([%cell *] data)  %.n
      |-
      ?:  &(=(loop-idx head.data) =(loop-idx tail.data))  %.y
      =/  headdata  (need data:(reflect head.data))
      ?:  ?=([%face *] headdata)  $(head.data xray.headdata)
      =/  taildata  (need data:(reflect tail.data))
      ?:  ?=([%face *] taildata)  $(tail.data xray.taildata)
      %.n
      ::  $@(~ [n=node l=(tree node) r=(tree node)])
    --
  ::
  ++  list-of-what
    |^  |=  =input=xray
        ^-  (unit idx)
        ?.  loop.input-xray  ~
        =/  input-idx=idx  idx.input-xray
        =/  indata=data    (need data.input-xray)
        ?.  ?=([%fork *] indata)  ~
        =/  branches  ~(tap in set.indata)
        ?.  ?=([* * ~] branches)  ~
        =/  nil   i.branches
        =/  node  i.t.branches
        |-
        ?:  (is-nil node)  $(node nil, nil node)
        ?.  (is-nil nil)  ~
        =/  node-data=data  (need data:(reflect node))
        ?.  ?=([%cell *] node-data)  ~
        ?.  (is-reference-to-loop input-idx tail.node-data)  ~
        =/  elem-data  (need data:(reflect head.node-data))
        ?:  ?=([%face *] elem-data)  `xray.elem-data
        `head.node-data
    ::
    ++  is-reference-to-loop
      |=  [=loop=idx =ref=idx]
      ^-  ?
      ?:  =(loop-idx ref-idx)  %.y
      =/  =data  (need data:(reflect ref-idx))
      ?:  ?=([%face *] data)  $(ref-idx xray.data)
      %.n
    --
  ::
  ++  is-type  (type-nests-under -:!>(*type))
  ++  is-vase  (type-nests-under -:!>(*vase))
  ++  is-gate  (type-nests-under -:!>(*$-(* *)))
  ::
  ++  xray-pats
    |=  [img=image i=idx]
    ^-  (set pattern)
    ::
    =/  x=xray  (reflect i)
    =/  t=type  type.x
    =/  d=data  (need data.x)
    ::
    %-  ~(gas in *(set pattern))
    ::
    ::  Atom printing works just fine without all this shit.
    ?:  ?=([%atom *] d)  ~
    ::
    =/  tree-elem  (tree-of-what x)
    ?^  tree-elem  ~[[%tree u.tree-elem]]
    ::
    =/  unit-elem  (unit-of-what x)
    ?^  unit-elem  ~[[%unit u.unit-elem]]
    ::
    =/  list-elem  (list-of-what x)
    ?^  list-elem
      ?:  (is-atom-with-aura 'tD' u.list-elem)  ~[%tape]
      ?:  (is-atom-with-aura 'ta' u.list-elem)  ~[%path]
      ?:  (is-atom-with-aura 'c' u.list-elem)   ~[%tour]
      ~[[%list u.list-elem]]
    ::
    %-  zing
    :~  ?.  (is-gate t)  ~  ~[[%gate 0 0]]  :: XX
        ?.  (is-hoon t)  ~  ~[%hoon]
        ?.  (is-manx t)  ~  ~[%manx]
        ?.  (is-nock t)  ~  ~[%nock]
        ?.  (is-plum t)  ~  ~[%plum]
        ?.  (is-skin t)  ~  ~[%skin]
        ?.  (is-type t)  ~  ~[%type]
        ?.  (is-vase t)  ~  ~[%vase]
    ==
  --
::
++  gc-image
  |=  input=image
  ^-  image
  |^  =/  result  *image
      =/  i=idx   0
      |-
      ?:  (~(has by result) i)  result
      =/  x=xray  (~(got by input) i)
      =.  result  (~(put by result) i x)
      %+  (foldl image idx)
        [result (xray-refs i)]
      |=  [=image =idx]
      ^$(result image, i idx)
  ::
  ++  xray-refs
    |=  i=idx
    ^-  (list idx)
    =/  x=xray  (~(got by input) i)
    %-  zing
    ^-  (list (list idx))
    :~  ?~(data.x ~ (data-refs u.data.x))
        (zing (turn ~(tap in recipes.x) recipe-refs))
        ?~(role.x ~ (role-refs u.role.x))
    ==
  ::
  ++  recipe-refs
    |=  r=recipe
    ^-  (list idx)
    ?-  r
      [%direct *]     ~
      [%synthetic *]  list.r
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
  ++  role-refs
    |=  s=role
    ^-  (list idx)
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
    ^-  (list idx)
    ?-  d
      %noun      ~
      %void      ~
      [%atom *]  ~
      [%cell *]  ~[head.d tail.d]
      [%core *]  [xray.d (battery-refs battery.d)]
      [%face *]  ~[xray.d]
      [%fork *]  ~(tap in set.d)
    ==
  --
::
++  decorate-xray-image-with-shapes
  |^  |=  st=image
      ^-  image
      =/  keys  ~(tap in ~(key by st))
      %+  (foldl image idx)  [st keys]
      |=  [st=image i=idx]
      =/  x  (~(got by st) i)
      =/  shape  (xray-shape st i)
      (~(put by st) i x(shape `shape))
  ::
  ++  xray-shape
    |=  [st=image i=idx]
    ^-  shape
    ::
    =/  x=xray  (~(got by st) i)
    =/  dat  (need data.x)
    =/  old  shape.x
    ?^  old  u.old
    ::
    ?-  dat
      %noun      %noun
      %void      %void
      [%atom *]  %atom
      [%cell *]  %cell
      [%core *]  %cell
      [%fork *]  (fork-shape st set.dat)
      [%face *]  (xray-shape st xray.dat)               ::  XX duplicated work
    ==
  ::
  ++  fork-shape
    |=  [img=image branches=(set idx)]
    ^-  shape
    %+  (foldl shape idx)  [%void ~(tap in branches)]
    |=  [s=shape =idx]
    ^-  shape
    (combine s (xray-shape img idx))                    ::  XX duplicated work
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
++  decorate-xray-image-with-roles
  |^  |=  img=image
      ^-  image
      =<  xrays
      ^-  ximage
      =/  keys=(list idx)  ~(tap in ~(key by img))
      =/  init=ximage      (image-to-ximage img)
      %+  (foldl ximage idx)  [init keys]
      |=  [acc=ximage =idx]
      ^-  ximage
      =.  focus.acc  idx
      =^  =role  acc  (xray-role acc)
      acc
  ::
  +$  ximage  [focus=idx next=idx xrays=image =type=(map type idx)]
  ::
  ::  Builds the ximage.
  ::
  ++  image-to-ximage
    |=  img=image
    ^-  ximage
    %+  (foldl ximage xray)
      [init=[0 0 img ~] elems=~(val by img)]
    |=  [acc=ximage x=xray]
    ^-  ximage
    =.  type-map.acc  (~(put by type-map.acc) type.x idx.x)
    =.  next.acc      (max +(idx.x) next.acc)
    acc
  ::
  ++  focus                                               ::  XX rename
    |=  xi=ximage
    ^-  xray
    =/  res=(unit xray)  (~(get by xrays.xi) focus.xi)
    ?~  res  ~&  ['internal error: invalid xray reference' focus.xi]
             !!
    u.res
  ::
  ::
  ::  Produce an ximage focused on the xray for a given type. If the
  ::  type isn't already in the ximage, create it first.
  ::
  ++  with-new-xray
    |=  [st=ximage ty=type d=data]
    ^-  ximage
    =/  old=(unit idx)  (~(get by type-map.st) ty)
    ?^  old  st(focus u.old)
    =/  idx          next.st
    =/  res=xray     [idx ty `d ~ ~ ~ ~ ~ ~ ~ %.n]
    =.  next.st      +(idx)
    =.  xrays.st     (~(put by xrays.st) idx.res res)
    =.  type-map.st  (~(put by type-map.st) type.res idx.res)
    =.  focus.st     idx
    st(focus idx)
  ::
  ::  Return an ximage modified to be focus on the %void type. If no
  ::  void type exists, one will be created.
  ::
  ++  void-xray
    |=  st=ximage
    (with-new-xray st %void %void)
  ::
  ::  Determines the role of an atom xray.
  ::
  ::  This is trivial enough that it should probably be done inline,
  ::  but it's a nice first example of what the *-role arms do.
  ::
  ++  atom-role
    |=  [st=ximage =constant=(unit @)]
    ^-  [role ximage]
    ?~  constant-unit  [%atom st]
    [[%constant u.constant-unit] st]
  ::
  ::  Determine the role of %fork type.
  ::
  ::  First, find (or create) an xray for the union type, then call back
  ::  into `role-xray` to get it's type.
  ::
  ::  The focused xray of the resulting ximage *will* be decorated with
  ::  role information, the role is just return for convenience.
  ::
  ++  fork-role
    |=  [st=ximage fork=(set idx)]
    ^-  [role ximage]
    (xray-role (fork-xray st fork))
  ::
  ::  Calculate the role of a %cell xray.
  ::
  ++  cell-role
    |=  [st=ximage head=idx]
    ^-  [role ximage]
   ::
    =/  x=xray  (focus st(focus head))
    =/  =shape  (need shape.x)
    =/  =data   (need data.x)
   ::
    =/  is-wide  =(shape %cell)
    ::
    =/  const  ?.  ?=([%atom *] data)  ~
               constant.data
    ::
    :_  st
    ?:  =(shape %cell)  %wide
    ?^  const           [%instance u.const]
    %cell
  ::
  ::  Produces an ximage updated to have role information for the xray
  ::
  ::  Produces an ximage updated to have role information for the xray
  ::  in focus.
  ::
  ::  The focused xray of the resulting image *will* be decorated with
  ::  role information, the role is just return for convenience.
  ::
  ++  xray-role
    |=  st=ximage
    ^-  [role ximage]
    =/  x=xray  (focus st)
    =/  old  role.x
    ?^  old  [u.old st]
    ::
    ::  Hack to prevent infinite loops.
    ::
    ::  =.  role.x  `%thunk
    ::  =.  xrays.st  (~(put by xrays.st) idx.x x)
    ::
    %-  |=  [res=role st=ximage]
        =.  xrays.st  (~(put by xrays.st) idx.x x(role `res))
        =.  focus.st  idx.x
        [res st]
    =/  dat  (need data.x)
    ::  ~&  ['xray-role' focus.st]
    ::  ~&  ['data' dat]
    ?-  dat
      %noun      [%noun st]
      %void      [%void st]
      [%atom *]  (atom-role st constant.dat)
      [%cell *]  (cell-role st head.dat)
      [%core *]  [%cell st]
      [%face *]  (xray-role st(focus xray.dat))        ::  same as nested role
      [%fork *]  (fork-role st set.dat)
    ==
  ::
  ::  Create a new xray from a union type.  Returns an ximage focused
  ::  on the result.
  ::
  ++  fork-xray
    |=  [st=ximage fork=(set idx)]
    ^-  ximage
    =.  st  (void-xray st)
    %+  (foldl ximage idx)
      [st ~(tap in fork)]
    |=  [=ximage =idx]
    (merge ximage focus.ximage idx)
  ::
  ::  Combine two xrays in an ximage (the one in focus and the one
  ::  referenced by `i`, producing a new ximage focused on the resulting
  ::  union.
  ::
  ::  First, we compute the role of both xrays, and then we `combine`
  ::  them.
  ::
  ++  merge
    |=  [st=ximage x=idx y=idx]
    ^-  ximage
    =/  this=xray  (focus st(focus x))
    =/  that=xray  (focus st(focus y))
    =^  this-role  st  (xray-role st(focus x))        ::  Is this needed?
    =^  that-role  st  (xray-role st(focus y))        ::  Is this needed?
    ?:  =(%void type.this)  st(focus idx.that)
    ?:  =(%void type.that)  st(focus idx.this)
    (combine st(focus idx.this) idx.that)
  ::
  ::  `combine` is complicated. Let's introduce it's helper-functions first:
  ::
  ::  -simple-forks: produce a simple fork set if any.
  ::
  ::  XX In the old code, we didn't produce one for loop entry points. Why
  ::  was that? Do we need to reintroduce that logic?
  ::
  ++  simple-forks
    |=  xi=ximage
    ^-  (unit (set idx))
    =/  x=xray  (focus xi)
    ?:  loop.x  ~
    =/  d=data  (need data.x)
    ?.  ?=([%fork *] d)  ~
    `set.d
  ::
  ::  Given two xrays, construct their union type and return it's xray.
  ::
  ::  Returns an `ximage` focused on the resulting xray.
  ::
  ::  Using the `fork` primitive to construct a new type, get the xray
  ::  for that type. If we already have an xray for that, just return
  ::  it. Otherwise we need to create one. The `data` field for the new
  ::  xray will be (basically) the result of doing a set-merge on the
  ::  trivial-forks of both xrays.
  ::
  ++  join
    |=  [st=ximage i=idx]
    ^-  ximage
    ::  ~&  ['join' focus.st i]
    ?:  =(focus.st i)  st
    =/  this=xray  (focus st)
    =/  that=xray  (focus st(focus i))
    =/  ty=type    (fork `(list type)`~[type.this type.that])
    =/  dat=data   :-  %fork
                   ^-  (set idx)
                   =/  these  (simple-forks st(focus idx.this))
                   =/  those  (simple-forks st(focus idx.that))
                   ?~  these  ?~  those  (sy idx.this idx.that ~)
                              (~(put in u.those) idx.this)
                   ?~  those  (~(put in u.these) idx.that)
                   (~(uni in u.these) u.those)
    (with-new-xray st ty dat)
  ::
  ::  =collate: merge option maps
  ::
  ++  collate
    |=  [st=ximage thick=(map atom idx) thin=(map atom idx)]
    ^-  [(map atom idx) ximage]
    ::  ~&  'collate'
    =/  list  ~(tap by thin)
    |-
    ^-  [(map atom idx) ximage]
    ?~  list  [thick st]
    =/  item=(unit idx)  (~(get by thick) p.i.list)
    =^  merged=idx  st  ?~  item  [q.i.list st]
                        =.  st  (merge st u.item q.i.list)
                        [focus.st st]
    =/  new-thick  (~(put by thick) p.i.list merged)
    $(list t.list, thick new-thick)
  ::
  ::  Huge amounts of nasty-ass logic that is probably wrong.
  ::
  ++  combine
    |=  [st=ximage target=idx]
    ^-  ximage
    ::
    ::  First let's do some setup. Get indicies for this, that, and the
    ::  joined type.
    ::
    =/  this      focus.st
    =/  that      target
    ::
    |-
    ^-  ximage
    ::
    ::  ~&  ['combine' focus.st target]
    ::
    ?:  =(this that)  st

    =^  this-role  st  (xray-role st(focus this))
    =^  that-role  st  (xray-role st(focus that))
    ::
    ::  Create the join of two xrays with the specified `role`.
    ::
    =/  join-with-role
      |=  [st=ximage x=idx y=idx =role]
      ^-  ximage
      ::
      =/  xx  (focus st(focus x))
      =/  yy  (focus st(focus y))
      ::  ~&  ['join-with-role' x y role]
      ::  ~&  [x '=' (need data.xx) (need role.xx)]
      ::  ~&  [y '=' (need data.yy) (need role.yy)]
      ::
      =.  st       (join st(focus x) y)
      =/  x=xray   (focus st)
      =.  role.x  `role
      =.  xrays.st  (~(put by xrays.st) idx.x x)
      st
    ::
    ::  Produce a joined node with the specified `role`.
    ::
    =/  joined
      |=  [st=ximage =role]
      ^-  ximage
      (join-with-role st this that role)
    ::
    ::  Convenience functions for creating junctions
    ::
    =/  misjunct  |=  [st=ximage x=idx y=idx]
                  ~&  ['MISJUNCTION' x y]
                  =/  xx=xray  (focus st(focus x))
                  =/  yy=xray  (focus st(focus y))
                  ~&  [x '=' (need data.xx) (need role.xx)]
                  ~&  [y '=' (need data.yy) (need role.yy)]
                  ::  ?:  %.y  !!
                  (join-with-role st x y [%misjunction x y])
    ::
    =/  conjunct  |=  [st=ximage wide=idx tall=idx]
                  (join-with-role st wide tall [%conjunction wide tall])
    ::
    =/  junct     |=  [st=ximage flat=idx deep=idx]
                  (join-with-role st flat deep [%junction flat deep])
    ::
    ::  Join a cell with a junction.
    ::
    =/  cell-junct
      |=  [st=ximage cell=idx [flat=idx deep=idx]]
      ^-  ximage
      =.  st  (merge st cell deep)
      =/  deep-merged  focus.st
      (junct st flat deep-merged)
    ::
    ::  Join an atom with a junction.
    ::
    =/  atom-junct
      |=  [st=ximage atom=idx [flat=idx deep=idx]]
      ^-  ximage
      =.  st  (merge st atom flat)
      =/  flat-merged  focus.st
      (junct st flat-merged deep)
    ::
    =/  tall-conjunct
      |=  [st=ximage out-tall=idx [wide=idx in-tall=idx]]
      ^-  ximage
      =.  st  (merge st out-tall in-tall)
      =/  new-tall  focus.st
      (conjunct st wide new-tall)
    ::
    =/  conjunct-conjunct
      |=  [st=ximage [xwide=idx xtall=idx] [ywide=idx ytall=idx]]
      =.  st  (merge st xwide ywide)
      =/  new-wide  focus.st
      =/  st  (merge st xtall ytall)
      =/  new-tall  focus.st
      :: XX Merging the talls or the wides might produce a misjunction! In
      :: either case, the result should also be a misjunction, not a
      :: conjunction. This is wrong.
      (conjunct st new-wide new-tall)
    ::
    ::  Alright, let's get into it! Are you ready?
    ::
    ?@  this-role
      ?^  that-role  $(this that, that this)
      %+  joined  st
      ^-  role
      ?:  =(this-role that-role)                       this-role
      ?:  ?=(%void this-role)                          that-role
      ?:  ?=(%void that-role)                          this-role
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
        %constant     =/  this-arg=(map atom idx)  [[atom.this-role this] ~ ~]
                      =/  that-arg=(map atom idx)  [[atom.that-role that] ~ ~]
                      =^  res  st  (collate st this-arg that-arg)
                      (joined st [%option res])
        %instance     (junct st this that)
        %option       =/  that-arg=(map atom idx)  map.that-role
                      =/  this-arg=(map atom idx)  [[atom.this-role this] ~ ~]
                      =^  res  st  (collate st this-arg that-arg)
                      (joined st [%option res])
        %union        (junct st this that)
        %junction     =.  st  (merge st this flat.that-role)
                      =/  merged  focus.st
                      (junct st merged deep.that-role)
        %conjunction  (junct st this that)
      ==
    ::
        %instance
      ?+  -.that-role  $(this that, that this)
        %instance     =/  this-arg=(map atom idx)  [[atom.this-role this] ~ ~]
                      =/  that-arg=(map atom idx)  [[atom.that-role that] ~ ~]
                      =^  res  st  (collate st this-arg that-arg)
                      (joined st [%union res])
        %option       (junct st this that)
        %union        =/  this-arg  [[atom.this-role this] ~ ~]
                      =^  res  st   (collate st map.that-role this-arg)
                      (joined st [%union res])
        %junction     =.  st  (merge st this deep.that-role)
                      =/  merged  focus.st
                      (junct st flat.that-role merged)
        %conjunction  (tall-conjunct st this [wide tall]:that-role)
      ==
    ::
        %option
      ?+  -.that-role  $(this that, that this)
        %option       =^  res  st  (collate st map.this-role map.that-role)
                      (joined st [%option res])
        %union        (junct st this that)
        %junction     =.  st  (merge st this flat.that-role)
                      =/  merged  focus.st
                      (junct st merged deep.that-role)
        %conjunction  (junct st this that)
      ==
    ::
        %union
      ?+  -.that-role  $(this that, that this)
        %union        =^  res  st  (collate st map.this-role map.that-role)
                      (joined st [%union res])
        %junction     =.  st  (merge st this deep.that-role)
                      =/  merged  focus.st
                      (junct st flat.that-role merged)
        %conjunction  (tall-conjunct st this [tall wide]:that-role)
      ==
    ::
        %junction
      ?+  -.that-role  $(this that, that this)
        %junction     =.  st  (merge st flat.this-role flat.that-role)
                      =/  flat  focus.st
                      =.  st  (merge st deep.this-role deep.that-role)
                      =/  deep  focus.st
                      (junct st flat deep)
        %conjunction  =.  st  (merge st deep.this-role that)
                      =/  merged  focus.st
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
++  analyze-type
  |=  t=type
  ^-  image
  %-  trace-xray-image
  %-  gc-image
  %-  decorate-xray-image-with-roles
  %-  decorate-xray-image-with-shapes
  %-  decorate-xray-image-with-patterns
  %-  decorate-xray-image-with-loops
  ::  %-  trace-xray-image
  %-  (xray-type 999.999 999.999)
  t
::
::  -xray-image-to-spec: convert to spec
::
++  xray-image-to-spec
  |=  img=image
  ^-  spec
  ::
  |^  (xray-to-spec ~ 0)
  ::
  +$  trace  (set idx)
  ::
  ++  recipe-to-spec
    |=  [tr=trace r=recipe]
    ^-  spec
    ?-  -.r
      %direct     [%like [term.r ~] ~]
      %synthetic  =/  subs  %+  turn  list.r
                            |=  =idx  (xray-to-spec tr idx)
                  [%make [%limb term.r] subs]
    ==
  ::
  ++  wrap-with-loop-binding
    |=  [xr=xray sp=spec]
    ^-  spec
    ?.  loop.xr  sp
    =/  nm  (synthetic idx.xr)
    [%bsbs [%loop nm] [[nm sp] ~ ~]]
  ::
  ::  If we see a reference to a type that we're already processing,
  ::  then we simply generate a reference to it and make a not that we
  ::  referenced this type in a loop.
  ::
  ++  xray-to-spec
    |=  [tr=trace i=idx]
    ^-  spec
    =/  x=xray  (~(got by img) i)
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
                        %-  ~(run by (flatten-battery battery.d))
                        |=  =idx  ^$(i idx)
             ?-  r.garb.d
               %lead  [%bszp payld batt]
               %gold  [%bsdt payld batt]
               %zinc  [%bstc payld batt]
               %iron  [%bsnt payld batt]
             ==
      %face  =/  =spec  $(i xray.d)
             ?^(face.d spec [%bsts face.d spec])
      %fork  =/  =role  (need role.x)
             |^  ?+  role
                     ~&  %unexpected-fork-role
                     ~&  [d role choices]
                     !!
                   [%option *]       [%bswt choices]
                   [%union *]        [%bscn choices]
                   %wide             [%bswt choices] :: XX bskt?
                   %cell             [%bswt choices] :: XX bskt?
                   %noun             [%bswt choices]
                   [%misjunction *]  [%bswt choices]
                   [%junction *]     [%bsvt ^$(i flat.role) ^$(i deep.role)]
                   [%conjunction *]  [%bskt ^$(i wide.role) ^$(i tall.role)]
                 ==
             ::
             ++  choices
               ^-  [i=spec t=(list spec)]
               =-  ?>(?=(^ -) -)
               (turn ~(tap in set.d) |=(=idx ^^$(i idx)))
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
    |=  batt=(battery idx)
    ^-  (map term idx)
    =/  chapter-list  ~(tap by batt)
    |-  ^-  (map term idx)
    ?~  chapter-list  ~
    (~(uni by q.q.i.chapter-list) $(chapter-list t.chapter-list))


:::::::::::::  HACK  :::::::::::::

++  show-hoon
  |^  hoon-to-wain
  ::
  +|  %entry-points-for-testing
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
  ++  hoon-to-wain
    |=  =hoon
    ^-  wain
    ~(tall plume (hoon-to-plum 999 hoon))
  ::
  ::  Pretty-print a hoon in tall mode using `plume`.
  ::
  ++  render-hoon
    |=  {^ {{demo=hoon ~} ~}}
    :-  %txt
    ^-  wain
    ~(tall plume (hoon-to-plum 999 demo))
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
    ~(tall plume (hoon-to-plum 999 [%cltr hoons]))
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
  ::  the last) when the match-list is rendered in wide mode.
  ::
  ++  matches-to-plum-list
    |=  =match=(list (pair spec hoon))
    ^-  (list plum)
    %-  add-trailing-commas-to-wide-form
    %+  turn  match-list
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
      ++  hn         |=(h=hoon (hoon-to-plum (dec maxdepth) h))
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
        %-  hn
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
        (hoon-to-plum 999 q.hint)
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
    [term (hoon-to-plum 999 hoon) ~]
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
              ?:  ?&(?=(^ wideresult) (lte length.u.wideresult 80))
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
  +$  loop-map  (map index zray)
  +$  wray  [=meta =data]
  +$  zray  [=type wray]
  +$  xray  $@(index zray)
  +$  notebook  [=xray xray-loops=loop-map]
  +$  index  @ud
  +$  role
    $@  $?  %atom  %cell  %noun  %void  %wide  ==
    $%  [%constant =atom]
        [%instance =atom]
        [%option =(map atom xray)]
        [%union =(map atom xray)]
        [%junction flat=xray deep=xray]
        [%conjunction wide=xray tall=xray]
        [%misjunction one=xray two=xray]
    ==
  +$  battery  (map term (pair what (map term xray)))
  +$  meta
    $:  =role=(unit role)
        =pattern=(set pattern)
        =standard=(set stud)
        =entry=(unit index)
        =recipe=(set recipe)
        =comment=(set help)
    ==
  +$  recipe
    $%  [%direct =term]
        [%synthetic =term =(list xray)]
    ==
  ::
  +$  xrayed-type  [=xray table=loop-map]
  +$  data
    $@  ?(%noun %void)
    $%  [%atom =aura =constant=(unit @)]
        [%cell head=xray tail=xray]
        [%core =garb =xray =battery]
        [%face face=$@(term tune) =xray]
        [%fork =(set xray)]
    ==
  ::
  ++  xray-type
    |^  |=  ty=type
        ^-  xrayed-type
        =+  [xray st]=(entry [~ ty] *state)
        %-  validate-xrayed-type
        ::  %-  dedupe-xrayed-type
        [xray (build-loop-map table.st)]
    ::
    +$  entry-table  (map type [=index =zray])
    +$  state        [count=@ud table=entry-table]
    ::
    +*  chap  [a]  (pair term (pair what (map term a)))
    +*  arm   [a]  (pair term a)
    ::
    ++  validate-xrayed-type
      |=  xt=xrayed-type
      ^-  xrayed-type
      ?.  (valid-xrayed-type xt)  xt :: XX
      xt
    ::
    ::  Given an `entry-table` (which we use to check if a given type is
    ::  an entry-point), produce a `loop-map`. In following analysis phases,
    ::  we will be traversing xrays and we need to lookup looks when we
    ::  come across them.  The `loop-map` structure encodes that.
    ::
    ++  build-loop-map
      |=  table=entry-table
      ^-  (map index zray)
      %-  ~(gas by *(map index zray))
      %+  turn  ~(tap by table)
      |=  [type [=index =zray]]
      [index zray]
    ::
    ::  This is just a small change to reduce the size of xrays. There is
    ::  no semantic change, but there's probably a performance win to be
    ::  had by doing this everywhere (instead of just at the top-level
    ::  node) since xrays often end up having a fair amount of duplicated
    ::  structure.
    ::
    ++  dedupe-xrayed-type
      |=  xt=xrayed-type
      ^-  xrayed-type
      =/  x  xray.xt
      ?@  x  xt
      =/  e  entry-unit.meta.x
      ?~  e  xt
      xt(xray u.e)
    ::
    ::  -entry: analyze at possible entry point
    ::
    ::  This analyzes a type that might be the entry-point for a loop.
    ::
    ::  - If this is already in the `loop-table`, just return it's index.
    ::  - If the type is in the type-trace then , further up the call stack,
    ::    we're already trying analyzing this same type. We found a
    ::    loop! Create a stub entry in the loop-table, and the code
    ::    further up the call stack (the one that's trying to analyze this
    ::    same type) will see the stub entry and replace it with the actual
    ::    xray value.
    ::  - Otherwise jump to `main`, the code that handles the rest of the
    ::    analysis logic.
    ::  - Finally, if main returned something besides a loop index but
    ::    somewhere further down the call added this type to the table, then
    ::    replace the stub xray that they put there, and set the `entry-unit`
    ::    value on the result xray to the loop index.
    ::
    ++  entry
      |=  [[tr=trace ty=type] st=state]
      ^-  [xray state]
      =/  old  (~(get by table.st) ty)
      ?^  old  [index.u.old st]
      ?:  (~(has in tr) ty)
        =/  result  `xray`count.st
        =/  stub    [ty [count.st *zray]]
        =.  st      [+(count.st) (~(put by table.st) stub)]
        [result st]
      =^  result  st  (main [(~(put in tr) ty) ty] st)
      ?@  result  [result st]
      =/  new  (~(get by table.st) ty)
      ?~  new  [result st]
      =*  idx  index.u.new
      =.  entry-unit.meta.result  `idx
      ?<  (~(has by table.st) idx)
      =.  table.st  (~(put by table.st) ty [idx result])
      [result st]
    ::
    ::  The main analysis code. This basically just calls out to other
    ::  helper functions based on which type of type this is.
    ::
    ++  main
      |=  [[tr=trace ty=type] st=state]
      ^-  [xray state]
      ?-  ty
        %void      [[ty *meta ty] st]
        %noun      [[ty *meta ty] st]
        [%atom *]  [[ty *meta ty] st]
        [%cell *]  =^  hed  st  $(ty p.ty)
                   =^  tyl  st  $(ty q.ty)
                   =*  wray  [*meta [%cell hed tyl]]
                   [[ty wray] st]
        [%core *]  =^  wray  st  (core [tr p.ty q.ty] st)
                   [[ty wray] st]
        [%face *]  =^  xray  st  $(ty q.ty)
                   =*  wray  [*meta %face p.ty xray]
                   [[ty wray] st]
        [%hint *]  =^  wray  st  (hint [tr p.ty q.ty] st)
                   [[ty wray] st]
        [%fork *]  =^  wray  st  (fork [tr p.ty] st)
                   [[ty wray] st]
        [%hold *]  (entry [tr ~(repo ut ty)] st)
      ==
    ::
    ::  Analyze a core.
    ::
    ++  core
      |=  [[tr=trace =payload=type =coil] st=state]
      ^-  [wray state]
      =^  payload-xray  st  (main [tr payload-type] st)
      =^  chapters=(list (chap xray))  st
        %+  (traverse-right (chap hoon) (chap xray) state)
          [~(tap by q.r.coil) st]
        |=  [chap=(chap hoon) st=state]
        =-  :_  ->
            :+  p.chap  `what`p.q.chap  (~(gas by *(map term xray)) -<)
        %+  (traverse-right (arm hoon) (arm xray) state)
          [~(tap by q.q.chap) st]
        |=  [arm=(arm hoon) st=state]
        =/  hold-type  [%hold [%core payload-type coil] q.arm]
        =^  xray  st  (main [tr hold-type] st)
        [arm(q xray) st]
      =*  chaps   (~(gas by *(map term (pair what (map term xray)))) chapters)
      =*  result  `wray`[*meta [%core p.coil payload-xray chaps]]
      [result st]
    ::
    ::  True if a coil contains no arms.
    ::
    ++  empty-coil
      |=  =coil
      ^-  ?
      =/  battery  q.r.coil
      =(~ battery)
    ::
    ::  Analyze a %hint type.
    ::
    ::    subject-type: subject of note
    ::    note: hint information
    ::    content-type: type of hinted content
    ::
    ++  hint
      |=  [[tr=trace [=subject=type =note] =payload=type] st=state]
      ^-  [wray state]
      =*  get-xray-by-loop-index  ~(got by (build-loop-map table.st))
      =^  result=xray  st  (main [tr payload-type] st)
      |-
      ^-  [wray state]
      ?@  result  $(result (get-xray-by-loop-index result))
      ?-    -.note
          %help
        =.  comment-set.meta.result
          (~(put in comment-set.meta.result) p.note)
        [+.result st]
          %know
        =.  standard-set.meta.result
          (~(put in standard-set.meta.result) p.note)
        [+.result st]
          %made
        =^  =recipe  st
          ?~  q.note  [[%direct p.note] st]
          =-  [`recipe`[%synthetic p.note -<] `state`->]
          |-
          ^-  [(list xray) state]
          ?~  u.q.note  [~ st]
          =*  tsld  [%tsld [%limb %$] [%wing i.u.q.note]]
          =*  part  (~(play ut subject-type) tsld)
          =^  this  st  (entry [tr part] st)
          =^  more  st  $(u.q.note t.u.q.note)
          [[this more] st]
        =.  recipe-set.meta.result
          (~(put in recipe-set.meta.result) recipe)
        [+.result st]
      ==
    ::
    ::  +fork: convert a %fork $type to an $xray
    ::
    ::  set: set of union types
    ::
    ++  fork
      |=  [[tr=trace =type=(set type)] st=state]
      ^-  [wray state]
      =^  xrays  st
        %+  (traverse-left type xray state)
          [~(tap in type-set) st]
        |=  [ty=type st=state]
        (main [tr ty] st)
      :_  st  `wray`[*meta %fork (~(gas in *(set xray)) xrays)]
    ::
    ::  Validates that an `xrayed-type` is internally consistent.
    ::  For example, validate that all loop references resolve.
    ::
    ++  valid-xrayed-type
      |^  |=  xt=xrayed-type
          ^-  ?
          =/  err  (find-error-anywhere xt)
          ?~  err  %.y
          ~&  `(unit cord)`err
          :: ~&  xray.xt
          :: ~&  table.xt
          %.n
      ::
      ++  check-lookup
        |=  [idx=@ tbl=(map index xray)]
        ^-  (unit @t)
        =/  mbx=(unit xray)  (~(get by tbl) idx)
        ?~  mbx  [~ %loop-index-not-in-table]
        =*  res  u.mbx
        ?@  res  [~ %loop-index-resolves-to-another-loop-index]
        ~
      ::
      ++  check-entry-point
        |=  [idx=@ xt=xrayed-type]
        ?@  xray.xt  [~ %entry-point-is-loop-index]
        =/  mbe  (check-lookup idx table.xt)
        ?^  mbe  mbe
        =/  ent  entry-unit.meta.xray.xt
        ?~  ent  [~ %reference-to-non-entry-point]
        ?.  =(idx u.ent)
          [~ %reference-to-invalid-entry-point]
        =/  res  (~(got by table.xt) idx)
        ?.  =(res xray.xt)
          [~ %inconsistent-entry-point]
        ~
      ::
      ++  check-metadata
        |=  xt=xrayed-type
        ^-  (unit @t)
        ?@  xray.xt  ~
        =/  entry  entry-unit.meta.xray.xt
        ?~  entry  ~
        (check-entry-point u.entry xt)
      ::
      ++  find-error-anywhere
        |=  xt=xrayed-type
        =/  mbe  (find-error xt)
        ?^  mbe  mbe
        %-  cat-errors
        %+  turn  ~(tap by table.xt)
        |=  [idx=@ x=xray]
        =/  mbe  (find-error xt(xray x))
        ?^  mbe  mbe
        (check-entry-point idx xt(xray x))
      ::
      ++  find-error
        |=  xt=xrayed-type
        ^-  (unit @t)
        ?@  xray.xt  (check-lookup xray.xt table.xt)
        =/  metaerr  (check-metadata xt)
        ?^  metaerr  metaerr
        %-  cat-errors
        %+  turn  (subxrays xray.xt)
        |=  x=xray  (find-error xt(xray x))
      ++  subxrays
        |=  x=xray
        ^-  (list xray)
        ?@  x  ~
        =/  d  data.x
        ?+    d
            ~
          [%cell *]  ~[head.d tail.d]
          [%core *]  [xray.d (subxrays-in-battery battery.d)]
          [%face *]  ~[xray.d]
          [%fork *]  ~(tap in set.d)
        ==
      ++  subxrays-in-battery
        |=  b=battery
        ^-  (list xray)
        %-  zing
        %+  turn  ~(val by b)
        |=  [* =(map term xray)]
        ^-  (list xray)
        ~(val by map)
      --
    --
  ::
  +$  trace  (set type)
  ::
  --
--


::  Data = Noun | Void
::       | Atom | Cnst @
::       | Cell Data Data
::       | Fork [Data]
::
::  Shape = Noun | Void
::        | Atom | Cnst
::        | Cell
::        | Junc
::
::  :: Data -> Shape
::  shape Noun       = Noun
::  shape Void       = Void
::  shape Atom       = Atom
::  shape (Cnst _)   = Cnst
::  shape (Cell a b) = Cell
::  shape (Fork bs)  = foldl forkShape Void (map forkShape bs)
::
::  :: Shape -> Shape -> Shape
::  forkShape X    X    = X
::  forkShape Noun _    = Noun
::  forkShape Void x    = x
::  forkShape Cnst Cnst = Atom
::  forkShape Atom Cnst = Atom
::  forkShape Atom Cell = Junc
::  forkShape Junc _    = Junc
::
::  :: Data -> Unit Role
::  role Noun        = ~
::  role Void        = ~
::  role (Atom ~)    = ~
::  role (Atom c)    = ~
::  role (Cell hd _) = cellRole (shape hd)
::  role (Fork x y)  = forkRole (shape x, role x) (shape y, role y)
::
::  :: Shape -> Unit Role
::  cellRole Cell = Wide
::  cellRole Atom = Tall
::  cellRole Cnst = Instance
::  cellRole _    = ~
::
::  :: (Shape,Role) -> (Shape,Role) -> Role
::  forkRole =
::      Option  <- option + option
::      Union   <- union  + union
::      Conjunc <- tall   + wide
::      Junc    <- atom   + cell
::      Misjunc <- otherwise
::    where
::      option = role==Option || role==Instance
::      union  = shape==Cnst  || role==Union
::      atom   = shape==Atom  || shape==Cnst
::      cell   = shape==Cell
::      tall   = role==Tall
::      wide   = role==Wide
::      cell   = shape==Cell
