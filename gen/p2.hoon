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
::  # Cleanup Work
::
::  - XX pattern annotation stack overflows on big examples.
::
::  - XX shape annotation stack overflows on big examples.
::
::  - XX For now, path literals are not rendered as paths. /a/path
::    expands to `[a [path ~]]`, which does not have a list type anywhere
::    inside of it. If we make the list pattern matcher accept this type
::    of thing, then we instead get annoying behaviors with in tuples
::    that end with null. For example [1 "str" ~] renders as [1 ~["str"]],
::    which is not what we want either. This is going to need a
::    more-specific heuristic change.
::
::  - XX `hint` information gets deleted during gc. The problem is that
::    if we write the metadata to a node that is also a pointer (a %hold
::    for example), it will be deleted. We can't just "write it to the right
::    place, since we don't know what this is yet. That's the whole reason
::    why this bullshit was needed in the first place.
::
::  - XX The `manx` data type has no represent raw text.
::
::    There must be some undocumented convention for how to do
::    this. Figure out what it is and implement it.
::
::  - XX The pattern matching code is basically brute-force.
::
::    If it turns out to be a performance bottleneck, there's lots of
::    low-hanging fruit there.
::
::  - XX The loop detection code infinite loops when trying to process
::    the kernel.
::
::  - XX Lists of nil values are not recognized as lists. Why?
::
::  - XX The pattern matching code has a lot of repeated logic.
::
::    This made sense before when I wasn't sure that this approach was
::    going to pan out, but now it really needs to be refactored.
::
/?  310
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
+|  %examples
::

++  type-example
  ^-  type
  -:!>(`(unit (list cord))`~)
::
++  xray-the-kernel-example
  |%  ++  x  ~  --
::
++  cores-example
  |^  :*
          [%core trivial-core-example]
          [%gate gate-example]
          [%core core-example]
          [%add ..add]
          [%biff ..biff]
          [%egcd ..egcd]
          [%po ..po]
          [%musk ..musk]
          [%zuse ..zuse]
          [%full ..full]
          [%zuse ..zuse]
      ==
  ::
++  trivial-core-example
  =>  ~
  |%  ++  x  3  --
::
++  core-example
  =>  [=gate-example]
  |%
  ++  dup  gate-example
  ++  const
    |=  x=*  ^-  $-(* *)
    |=  *    ^-  *
    x
  --
::
++  gate-example
  =>  ~
  |=  x=@ud
  ^-  [@ud @ud]
  [x x]
--
::
++  test-example
  :*
      [%type -:!>(`(unit (list tape))`~)]
      [%zeros `(list @)`~[0 0]]
      `?`%.y
  ==
::
++  hoon-example
  ^-  hoon
  :+  %brcn  ~
  %-  ~(gas by *(map term tome))
  ^-  (list (pair term tome))
  :_  ~
  ^-  (pair term tome)
  :-  'chapter'
  ^-  tome
  :-  `what`~
  %-  ~(gas by *(map term hoon))
  ^-  (list (pair term hoon))
  :_  ~
  :-  'arm'
  :+  %brts  `spec`[%bsts 'x' [%base [%atom ~.ud]]]
  :-  %clsg
  ~[[%wing ~['x']] [%$ 0]]
::
++  show-example
  |^  :*  [~ %.y %.n 1 0x2 ~ ~.knot 'cord' %const]
          :*  [%tape "a tape"]
              [%path /path/literal `path`/typed/path]
              [%unit `(unit @)`[~ 9]]
              [%list [`?`%.y `(list ?)`~[%.y %.n %.y]]]
              %nice
          ==
          [%hoon hoon-example]
          [%type -:!>(`(unit (list tape))`~)]
          [%json-and-xml json-example xml-example]
          %cool
      ==
  ::
++  xml-example
  |^  ^-  manx
      :-  ['json' ~]
      :~  (json-to-xml json-example)
      ==
  ++  json-to-xml
    |=  j=json
    ^-  manx
    ?-  j
      ~       [['nil' ~] ~]
      [%a *]  [['array' ~] (turn p.j json-to-xml)]
      [%b *]  [['bool' ~[['val' ?:(p.j "true" "false")]]] ~]
      [%o *]  [['obj' ~] (turn ~(tap by p.j) pair)]
      [%n *]  [['num' ~[[['n' 'val'] (trip p.j)]]] ~]
      [%s *]  [[p.j ~] ~]
    ==
  ++  pair
    |=  [t=@t j=json]
    ^-  manx
    [['slot' ~[['key' (trip t)]]] ~[(json-to-xml j)]]
  --
::
++  json-example
  ^-  json
  |^  ob2
  ++  nil  ~
  ++  yes  [%b %.y]
  ++  nah  [%b %.n]
  ++  foo  'foo'
  ++  bar  'bar'
  ++  baz  'baz'
  ++  one  [%n '1']
  ++  ten  [%n '10']
  ++  mil  [%n '100000']
  ++  arr  [%a ~[one ten mil]]
  ++  ar2  [%a ~[arr yes nah nil]]
  ++  obj  [%o (~(gas by *(map @t json)) ~[[foo mil] [baz arr]])]
  ++  ob2  [%o (~(gas by *(map @t json)) ~[[foo ar2] [bar obj] [baz yes]])]
  ++  ar3  [%a ~[arr obj ob2 one ten mil yes nah nil]]
  --
--
::
+|  %entry-points-for-testing
::
::  Left-fold over a list.
::
++  foldl
   |*  [state=mold elem=mold]
   |=  [[st=state xs=(list elem)] f=$-([state elem] state)]
   ^-  state
   |-
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
  ?:  =(-:!>(..zuse) ty)
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
  |=  {^ {{v=vase ~} ~}}
  :-  %txt
  ^-  wain
  ::
  =.  v  !>(xray-the-kernel-example)  ::  YY
  ::
  =/  t=type   p.v
  =/  n=*      q.v
  ::
  ~&  %start-xraying-type
  =/  i=image  ((xray-type 999.999 999.999) t)
  ~&  %done-xraying-type
  ::
  ~&  %start-xray-gc
  =.  i  (gc-image i)
  ~&  %done-with-xray-gc
  ::
  ::  =.  i  (trace-xray-image i)
  ::
  ~&  %start-loop-detection
  =.  i  (decorate-xray-image-with-loops i)
  ~&  %done-with-loop-detection
  ::
  ::  =.  i  (trace-xray-image i)
  ::
  ::  ~&  %start-pattern-annotation
  ::  =.  i  (decorate-xray-image-with-patterns i)
  ::  ~&  %done-with-pattern-annotation
  ::
  ::  =.  i  (trace-xray-image i)
  ::
  ::  ~&  %start-shape-annotation
  ::  =.  i  (decorate-xray-image-with-shapes i)
  ::  ~&  %done-with-shape-annotation
  ::
  ::  =.  i  (trace-xray-image i)
  ::
  ::  ~&  %start-role-annotation
  ::  =.  i  (decorate-xray-image-with-roles i)
  ::  ~&  %done-with-role-annotation
  ::
  ::  =.  i  (trace-xray-image i)
  ::
  ~&  %start-converting-to-plum
  =/  =plum  (xray-noun-to-plum i n)
  ~&  %done-converting-to-plum
  ::
  ~(tall plume plum)
  ::
  ::  ~(tall plume (vase-to-plum example))
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
  =/  t=type  -:(ride -:!>(..zuse) c)
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
  =/  img=image  (analyze-type t)
  (spec-to-plum (xray-image-to-spec focus.img img))
::
::  Pretty-print a hoon in tall mode using `plume`.
::
++  render-hoon
  |=  {^ {{demo=hoon ~} ~}}
  :-  %txt
  ^-  wain
  ~(tall plume (hoon-to-plum 999 demo))
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
  =/  fmt  [wide=`['  ' ~] tall=`['' ~]]
  :-  %sbrk
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
::
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
::
+*  battery  [item]  (map term (pair what (map term item)))
::
+$  recipe
  $%  [%direct =term]
      [%synthetic =term =(list idx)]
  ==
::
+$  pattern
  $@  ?(%hoon %manx %json %nock %path %plum %skin %spec %tape %tour %type %vase)
  $%  [%gate sample=idx product=idx]
      [%gear sample=idx context=idx =(battery idx)]
      [%list item=idx]
      [%tree item=idx]
      [%unit item=idx]
  ==
::
+$  data
  $@  ?(%noun %void)
  $%  [%atom =aura constant=(unit @)]
      [%cell head=idx tail=idx]
      [%core =garb xray=idx =(battery idx)]
      [%face face=$@(term tune) xray=idx]
      [%fork =(set idx)]
      [%pntr xray=idx]
  ==
::
+$  xray
  $:  =idx
      =type
      data=(unit data)
      fork=(unit (pair idx idx))
      role=(unit role)
      pats=(unit pattern)
      studs=(set stud)
      recipes=(set recipe)
      helps=(set help)
      shape=(unit shape)
      loop=(unit ?)
  ==
::
+$  image
  $:  focus=idx
      next=idx
      xrays=(map idx xray)
      =type=(map type idx)
  ==
::
::  Create an new xray and put it in the xray table. If there's already
::  a stub xray under this type, replace it.  Otherwise, allocate a
::  new index and put it there.
::
++  post-xray
  |=  [img=image ty=type d=(unit data)]
  ^-  image
  ::
  =/  old  (~(get by type-map.img) ty)
  ::
  =^  idx=idx  img  ?^  old  [u.old img]
                    =/  newidx  next.img
                    =.  next.img  +(next.img)
                    [newidx img]
  =/  x=xray  [idx ty d ~ ~ ~ ~ ~ ~ ~ ~]
  =.  xrays.img     (~(put by xrays.img) idx x)
  =.  type-map.img  (~(put by type-map.img) ty idx)
  img(focus idx)
::
::  Create an new xray and put it in the xray table. If there's already
::  a stub xray under this type, replace it.  Otherwise, allocate a
::  new index and put it there.
::
++  replace-xray
  |=  [img=image x=xray]
  ^-  image
  img(xrays (~(put by xrays.img) idx.x x))
::
++  set-xray-data
  |=  [img=image i=idx d=data]
  ^-  image
  =/  x=xray  (focus-on img i)
  (replace-xray img x(data `d))
::
::  Return an image modified to be focus on the %void type. If no
::  void type exists, one will be created.
::
++  void-xray
  |=  img=image
  ^-  image
  (post-xray img %void `%void)
::
++  empty-image
  ^-  image
  [0 0 ~ ~]
::
++  focus-on
  |=  [img=image i=idx]
  ^-  xray
  =/  res=(unit xray)  (~(get by xrays.img) i)
  ?~  res  ~&  ['internal error: invalid xray reference' i]  !!
  u.res
::
++  deref
  |=  [img=image i=idx]
  ^-  idx
  |-
  =/  x=xray  (focus-on img i)
  =/  d=data  (need data.x)
  ?.  ?=([%pntr *] d)  idx.x
  $(i xray.d)
::
++  focus
  |=  img=image
  ^-  xray
  (focus-on img focus.img)
::
++  tape-to-plum
  |=  =tape
  ^-  plum
  (simple-wide-plum '"' '' '"' `(list plum)`tape)
::
++  xray-noun-to-plum
  |=  [img=image n=*]
  ^-  plum
  |^  (main focus.img n)
  ::
  ++  main
    |=  [i=idx n=*]
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
    |=  [i=idx n=*]
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
    |=  [i=idx n=*]
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
    |=  [i=idx d=data n=*]
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
      [%core *]  (core-to-plum garb.d xray.d battery.d)
      [%fork *]  (render-fork i n)
    ==
  ::
  ++  render-fork
    |=  [i=idx n=*]
    ^-  plum
    ::
    =/  x=xray  (focus-on img i)
    ?~  role.x  ~&  x  '%evil-fork'
    =/  r=role  u.role.x
    ::
    ?+  r  '%bad-fork'
      [%union *]        '%union'                        ::  XX TODO
      [%option *]
        =/  pairs=(list (pair atom idx))  ~(tap by map.r)
        |-
        ?~  pairs  !!
        ?.  =(p.i.pairs n)  $(pairs t.pairs)
        (main q.i.pairs n)
      [%junction *]     '%junction'                     ::  XX TODO
      [%conjunction *]  '%conjunction'                  ::  XX TODO
      [%misjunction *]  '%misjunction'                  ::  XX TODO
    ==
  ::
  ++  render-gate
    |=  [=sample=idx =product=idx]
    ^-  plum
    %+  hoon-to-plum  999
    :+  %brts                                           ::  {$brts spec hoon}
      (xray-image-to-spec sample-idx img)
    :+  %kthp
      (xray-image-to-spec product-idx img)
    [%wing ~['...']]                                    ::  XX TODO
  ::
  ++  core-to-plum
    |=  [=garb xray=idx =(battery idx)]
    ^-  plum
    ::
    =/  cvt-arms
      |=  m=(map term idx)
      ^-  (map term hoon)
      %-  ~(gas by *(map term hoon))
      %+  turn  ~(tap by m)
      |=  [t=term i=idx]
      =.  t  ?:(=('' t) '$' t)
      ^-  [term hoon]
      [t [%wing ~['...']]]                              ::  XX TODO
    ::
    =/  batt=(map term tome)
      %-  ~(gas by *(map term tome))
      %+  turn  ~(tap by battery)
      |=  [nm=term w=what arms=(map term idx)]
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
  ++  manx-to-plum
    |=  [[=mane =mart] =marl]
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
    ::  Note that `kidfmt` uses "the ace-ace rune" (scare quotes) to
    ::  get indentation.
    ++  childs  [%tree kidfmt (turn marl manx-to-plum)]
    ++  kidfmt  ^-  plumfmt  :-  `['' `['' '']]  `['  ' `['' '']]
    ::
    ++  topfmt  =/  widetopstr  (cat 3 topstr ' ')
                :-  wide=[~ ' ' [~ widetopstr '>']]
                    tall=[~ topstr [~ '' '>']]
    ++  topstr  (cat 3 '<' tagstr)
    ++  atribs  (turn mart attr-to-plum)
    ::
    ++  endtag  (cat 3 '</' (cat 3 tagstr '>'))
    ++  endfmt  [[~ '' [~ '</' '>']] ~]
    ::
    ++  atrfmt  [[~ '="' [~ '' '"']] ~]                 ::  XX Escaping
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
++  xray-type
  |=  [max-depth=@ max-size=@]
  |^  |=  ty=type
      ^-  image
      =/  st=state  [empty-image 0 ~]
      =^  result  st  (main ty st)
      img.st(focus result)
  ::
  +$  state  [img=image depth=@ trace=(list cord)]
  ::
  ::  Create an new xray and put it in the xray table. If there's already
  ::  a stub xray under this type, replace it.  Otherwise, allocate a
  ::  new index and put it there.
  ::
  ++  with-new-xray
    |=  [st=state ty=type]
    ^-  [idx state]
    =/  img  (post-xray img.st ty ~)
    [focus.img st(img img)]
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
    ^-  [idx state]
    ::
    =/  old  (~(get by type-map.img.st) ty)             ::  don't loop
    ?^  old  [u.old st]
    ::
    =^  res=idx  st  (with-new-xray st ty)
    ::
    =/  trace=(list cord)
      ?:  (lte (lent trace.st) 26)
        (flop trace.st)
      %-  zing  :~  (scag 13 (flop trace.st))
                    ~['...']
                    (scag 13 trace.st)
                ==
    ::
    ::  ~&  [depth.st res ?@(ty ty -:ty) trace]  XX  TRACE
    ::
    ::
    ::  Track recursion depth.
    ::
    %-  |=([i=idx s=state] [i s(depth (dec depth.s))])
    =.  depth.st  +(depth.st)
    ::
    ^-  [idx state]
    ::
    :-  res
    ?-  ty
      %void      st(img (set-xray-data img.st res %void))
      %noun      st(img (set-xray-data img.st res %noun))
      [%atom *]  st(img (set-xray-data img.st res `data`ty))
      ::
      [%cell *]  =^  hed=idx  st  (main p.ty st)
                 =^  tyl=idx  st  (main q.ty st)
                 =.  img.st  (set-xray-data img.st res [%cell hed tyl])
                 st
      ::
      [%core *]  =/  arms=(list term)  (battery-arms q.r.q.ty)
                 =^  d=data   st       (xray-core [p.ty q.ty] st)
                 =.  img.st            (set-xray-data img.st res d)
                 st
      ::
      [%face *]  =^  i=idx  st   (main q.ty st)
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
    ::  ~&  [depth.st (cat 3 'arm=' p.x)]
    =/  arm-name  ?:(=(p.x '') '$' p.x)
    =.  trace.st  [arm-name trace.st]
    =.  r.p.coil  %gold
    =.  q.x
      ?.  =(%wet q.p.coil)  q.x
      [%rock p=%tas q='wet-arm']

    ::  ~&  'XRAY-ARM'
    ::  ~&  'hoon'
    ::  ~&  ~(tall plume (hoon-to-plum 999 q.x))
    ::  ~&  'payload-type'
    ::  ~&  ~(tall plume (simple-type-to-plum payload-type 10))
    ::  ~&  'coil-context'
    ::  ~&  ~(tall plume (simple-type-to-plum q.coil 10))
    =/  thunk  [%hold [%core payload-type coil] q.x]
    =^  i=idx  st  (main thunk st)
    =.  trace.st  +:trace.st
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
  |=  init=image
  ^-  image
  ::
  =/  all-indicies=(list idx)  ~(tap in ~(key by xrays.init))
  %+  (foldl image idx)
    [init all-indicies]
  |=  [img=image i=idx]  ^-  image
  ::
  =/  trace=(set idx)  ~
  ::
  |-  ^-  image
  ::
  =/  x    (focus-on img i)
  =/  dat  (need data.x)
  ::
  ?.  =(~ loop.x)  img                                  ::  don't repeat work
  ::
  =.  img
    ?:  (~(has in trace) i)                             ::  found loop
      (replace-xray img x(loop `%.y))
    ?@  dat  img                                        ::  no references
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
      %pntr  !!                                         ::  gc before this
      %fork  %+  (foldl image idx)
               [img ~(tap in set.dat)]
             |=  [img=image i=idx]
             ^$(img img, i i)
    ==
  =.  x  (focus-on img i)                          ::  get updated xray
  ?^  loop.x  img                                       ::  loop found
  (replace-xray img x(loop `%.n))                       ::  no loop found
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
  ~&  ['focus=' focus.img]
  ~&  %+  sort  ~(tap by xrays.img)
      |=  [[xi=idx x=xray] [yi=idx y=xray]]
      (lth xi yi)
  img
::
++  decorate-xray-image-with-patterns
  |=  img=image
  ^-  image
  |^  =/  pairs  %+  turn  ~(tap by xrays.img)
                 |=  [i=idx x=xray]
                 ^-  [idx xray]
                 [i x(pats (xray-pats x))]
      img(xrays (~(gas by *(map idx xray)) pairs))
  ::
  ::  XX Stop using this. Use `simple-nest-pattern` instead.
  ::
  ++  type-nests-under
    |=  t1=type
    ^-  $-(type ?)
    |=  t2=type
    ^-  ?
    (~(nest ut t1) | t2)
  ::
  ++  is-hoon  (type-nests-under -:!>(*hoon))
  ++  is-json  (type-nests-under -:!>(*json))
  ++  is-nock  (type-nests-under -:!>(*nock))
  ++  is-plum  (type-nests-under -:!>(*plum))
  ++  is-skin  (type-nests-under -:!>(*skin))
  ::
  ++  focus  |=(i=idx (focus-on img i))
  ::
  ++  is-nil
    |=  i=idx
    ^-  ?
    =/  =data  (need data:(focus i))
    ?+  data  %.n
      [%atom *]  =(data [%atom ~.n `0])
      [%face *]  $(i xray.data)
    ==
  ::
  ::  Is `ref`, dereferencing faces, a loop-reference to `target`?
  ::
  ++  is-ref-to
    |=  [target=idx ref=idx]
    ^-  ?
    ::
    =.  target  (deref img target) ::  XX
    =.  ref     (deref img ref)    ::  XX
    ::
    ?:  =(target ref)  %.y
    =/  =data  (need data:(focus ref))
    ?:  ?=([%face *] data)  $(ref xray.data)
    %.n
  ::
  ++  is-pair-of-refs-to
    |=  [target=idx cell=idx]
    ^-  ?
    |-
    ::
    =.  target  (deref img target) ::  XX
    =.  cell    (deref img cell)   ::  XX
    ::
    =/  =data  (need data:(focus cell))
    ?:  ?=([%face *] data)  $(cell xray.data)
    ?.  ?=([%cell *] data)  %.n
    ?.  (is-ref-to target head.data)  %.n
    ?.  (is-ref-to target tail.data)  %.n
    %.y
  ::
  ++  is-atom-with-aura
    |=  [c=cord i=idx]
    ^-  ?
    =/  =data  (need data:(focus i))
    ?+  data  %.n
      [%atom *]  =(data [%atom aura=c constant-unit=~])
      [%face *]  $(i xray.data)
    ==
  ::
  ::  Is this xray a unit? (the %unit pattern)
  ::
  ++  unit-pattern
    |^  |=  x=xray
        ^-  (unit pattern)
        =/  elem  (match-unit-type-strict (focus idx.x))
        ?~  elem  ~
        `[%unit u.elem]
    ::
    ++  match-unit-type-strict
      |=  =input=xray
      ^-  (unit idx)
      =/  input-idx=idx  idx.input-xray
      =/  indata=data    (need data.input-xray)
      ::
      ?.  ?=([%fork *] indata)  ~
      =/  branches              ~(tap in set.indata)
      ?.  ?=([* * ~] branches)  ~
      ::
      =/  nil   i.branches
      =/  node  i.t.branches
      |-
      ?:  (is-nil node)  $(node nil, nil node)
      ?.  (is-nil nil)   ~
      ::
      =/  node-data=data           (need data:(focus node))
      ?.  ?=([%cell *] node-data)  ~
      ?.  (is-nil head.node-data)  ~
      =/  elem-idx                 tail.node-data
      =/  elem-data                (need data:(focus elem-idx))
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
    =.  input-xray  (focus idx.input-xray)
    =/  input-idx=idx  (deref img idx.input-xray)
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
    ?.  (is-pair-of-refs-to input-idx (deref img tail.node-data))
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
        ::  ~&  ['list-pattern' idx.x]
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
      ^-  (unit idx)
      =.  input-xray  (focus idx.input-xray)
      ::  ~&  ['match-list' idx.input-xray (need data.input-xray)]  ::  TRACE
      =/  d=data  (need data.input-xray)
      ?+  d        ~  ::  ~&  ['match-list' idx.input-xray 'failure']  ~
        [%face *]  (match-list (focus xray.d))
        [%fork *]  (match-list-type-strict input-xray)
        [%cell *]  =/  elem-idx=(unit idx)
                     ?:  ?&((is-nil tail.d) (is-atom-with-aura 'tas' head.d))
                       ::  ~&  ['match-list' idx.input-xray 'looks-like-path']
                       `head.d
                     (match-list (focus tail.d))
                   ?~  elem-idx                       ~
                   ?.  (is-ref-to u.elem-idx head.d)  ~
                   `u.elem-idx
      ==
    ::
    ++  match-list-type-strict
      |=  =fork=xray
      ^-  (unit idx)
      ::  ~&  ['match-list-type-strict' idx.fork-xray (need data.fork-xray)]  ::  TRACE
      =/  fork=idx     (deref img idx.fork-xray)
      =/  indata=data  (need data.fork-xray)
      ::
      ?.  ?=([%fork *] indata)  ~
      =/  branches              ~(tap in set.indata)
      ?.  ?=([* * ~] branches)  ~
      ::
      ::  ~&  ['match-list-type-strict' idx.fork-xray 'is-a-2fork']
      ::
      =/  nil   i.branches
      =/  node  i.t.branches
      |-
      ?:  (is-nil node)  $(node nil, nil node)
      ?.  (is-nil nil)  ~
      ::
      ::  ~&  ['match-list-type-strict' idx.fork-xray 'is-a-nil-and-noun']
      ::
      =/  node-data=data                   (need data:(focus node))
      ?.  ?=([%cell *] node-data)          ~
      ::  ~&  ['match-list-type-strict' idx.fork-xray 'is-a-nil-and-cell']
      ?.  (is-ref-to fork tail.node-data)  ~
      ::  ~&  ['match-list-type-strict' idx.fork-xray 'loops']
      =/  elem-data                        (need data:(focus head.node-data))
      ?.  ?=([%face *] elem-data)          ~
      ::  ~&  ['match-list-type-strict' idx.fork-xray 'has-face. success!']
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
        =.  x  (focus idx.x)
        =/  gear  (match-gear x)
        ?~  gear  ~
        =/  gate  (match-gate x sample.u.gear battery.u.gear)
        ?~  gate  gear
        gate
    ::
    ++  match-gear
      |=  =input=xray
      ^-  (unit [%gear sample=idx context=idx =(battery idx)])
      ::
      =/  input-data  (need data.input-xray)
      ?.  ?=([%core *] input-data)  ~
      =/  context-idx=idx  xray.input-data
      ::
      =/  context-data=data  (need data:(focus context-idx))
      ?.  ?=([%cell *] context-data)  ~
      ::
      =/  sample-idx=idx  (deref img head.context-data)
      =.  context-idx     (deref img tail.context-data)
      `[%gear sample-idx context-idx battery.input-data]
    ::
    ++  match-gate
      |=  [=input=xray sample=idx =(battery idx)]
      ^-  (unit [%gate idx idx])
      ::
      =/  input-data  (need data.input-xray)
      ?.  ?=([%core *] input-data)  ~
      =/  chapters  ~(tap by battery)
      ::
      ?~  chapters            ~
      ?^  t.chapters          ~
      ?.  =(p.i.chapters '')  ~
      ::
      =/  arms=(list (pair term idx))  ~(tap by q.q.i.chapters)
      ::
      ?~  arms            ~
      ?^  t.arms          ~
      ?.  =(p.i.arms '')  ~
      ::
      =/  product=idx  q.i.arms
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
  ::
  ++  xray-pats
    |=  x=xray
    ^-  (unit pattern)
    ::
    =.  x  (focus idx.x) :: XX
    ::
    =/  i=idx   idx.x
    =/  t=type  type.x
    =/  d=data  (need data.x)
    ::
    ::  Atom printing works fine just using the data field.
    ?:  ?=([%atom *] d)  ~
    ::
    =/  tree-pat  (tree-pattern x)
    ?^  tree-pat  tree-pat
    ::
    =/  unit-pat  (unit-pattern x)
    ?^  unit-pat  unit-pat
    ::
    =/  core-pat  (core-pattern x)
    ?^  core-pat  core-pat
    ::
    =/  list-pat  (list-pattern x)
    ?^  list-pat  list-pat
    ::
    =/  spec-pat  (spec-pattern x)
    ?^  spec-pat  spec-pat
    ::
    =/  type-pat  (type-pattern x)
    ?^  type-pat  type-pat
    ::
    =/  manx-pat  (manx-pattern x)
    ?^  manx-pat  manx-pat
    ::
    =/  vase-pat  (vase-pattern x)
    ?^  vase-pat  vase-pat
    ::
    ?:  (is-hoon t)  `%hoon
    ?:  (is-json t)  `%json
    ?:  (is-nock t)  `%nock
    ?:  (is-plum t)  `%plum
    ?:  (is-skin t)  `%skin
    ~
  --
::
::  1. Build a list of reachable, non-reference nodes.
::  2. Build a table of references to what they reference.
::  3. Map over the type-map, and replace every value using the table from #2.
::  4. Rebuild the xrays map, only keeping xrays from set #1.
::  5. Map over the xrays, and replace every reference using the table from #2.
::
++  gc-image
  |=  img=image
  ^-  image
  ::
  |^  =/  tbl  (build-table [~ ~] focus.img)
      ::  ~&  ~(tap by refs.tbl)
      =.  focus.img  (fix-idx tbl focus.img)
      ::
      =.  type-map.img  (fix-type-map tbl type-map.img)
      ::  ~&  ~(tap by type-map.img)
      =.  xrays.img  (fix-xrays tbl xrays.img)
      ::  ~&  ~(tap by xrays.img)
      ::  ?:  %.y  !!
      ::
      :: ~&  [%deleted ~(tap in ~(key by refs.tbl))]
      ~&  [%gc-results ~(wyt by type-map.img) ~(wyt by xrays.img)]
      ::
      ::  ~&  ~(tap by refs.tbl)
      ::
      img
  ::
  +$  table  [live=(set idx) refs=(map idx idx)]
  ::
  ++  build-table
    |=  [tbl=table i=idx]
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
    %+  (foldl table idx)
      [tbl (xray-refs i)]
    build-table
    ::
  ++  gc-xrays
    |=  [tbl=table xrays=(map idx xray)]
    ^-  _xrays
    %+  (foldl (map idx xray) (pair idx xray))
      [*(map idx xray) ~(tap by xrays)]
    |=  [acc=(map idx xray) [i=idx x=xray]]
    ?.  (~(has in live.tbl) i)  acc
    (~(put by acc) i x)
  ::
  ++  fix-type-map
    |=  [tbl=table =(map type idx)]
    ^-  _map
    %+  (foldl _map (pair type idx))
      [*_map ~(tap by map)]
    |=  [acc=_map [ty=type i=idx]]
    =/  dest  (~(get by refs.tbl) i)
    ?^  dest  (~(put by acc) ty u.dest)
    ?.  (~(has in live.tbl))  acc
    (~(put in acc) ty i)
  ::
  ++  fix-xrays
    |=  [tbl=table xrays=(map idx xray)]
    ^-  _xrays
    %+  (foldl (map idx xray) (pair idx xray))
      [*(map idx xray) ~(tap by xrays)]
    |=  [acc=(map idx xray) [i=idx x=xray]]
    ?.  (~(has in live.tbl) i)  acc                     ::  Drop unused xrays
    (~(put by acc) i (fix-xray tbl x))
  ::
  ::  XX Do roles too (but this runs before role annotation for now)
  ::
  ++  fix-xray
    |=  [tbl=table x=xray]  ^-  xray
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
    =/  fix  |=(i=idx (fix-idx tbl i))
    ::
    ?-  d
      %noun      d
      %void      d
      [%atom *]  d
      [%cell *]  d(head (fix head.d), tail (fix tail.d))
      [%core *]  d(xray (fix xray.d), battery (fix-battery tbl battery.d))
      [%face *]  d(xray (fix xray.d))
      [%fork *]  d(set (~(gas in *(set idx)) (turn ~(tap in set.d) fix)))
      [%pntr *]  d(xray (fix xray.d))
    ==
  ::
  ++  turn-battery
    |*  item=mold
    |=  [b=(battery item) f=$-(item item)]  ^-  (battery item)
    %-  ~(run by b)
    |=  [w=what chap=(map term item)]  ^-  [what (map term item)]
    :-  w
    %-  ~(run by chap)
    |=  i=item  ^-  item
    (f i)
  ::
  ++  fix-battery
    |=  [tbl=table b=(battery idx)]  ^-  (battery idx)
    %+  (turn-battery idx)  b
    |=  i=idx  ^-  idx
    (fix-idx tbl i)
  ::
  ++  fix-idx
    |=  [tbl=table i=idx]  ^-  idx
    =/  res=(unit idx)  (~(get by refs.tbl) i)
    ?^  res  u.res
    i
  ::
  ++  fix-recipe
    |=  [tbl=table r=recipe]  ^-  recipe
    ?-  r
      [%direct *]     r
      [%synthetic *]  r(list (turn list.r |=(i=idx (fix-idx tbl i))))
    ==
  ::
  ++  xray-refs
    |=  i=idx
    ^-  (list idx)
    =/  x=xray  (focus-on img i)
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
      [%pntr *]  ~[xray.d]
      [%fork *]  ~(tap in set.d)
    ==
  --
::
++  decorate-xray-image-with-shapes
  |^  |=  st=image
      ^-  image
      =/  keys  ~(tap in ~(key by xrays.st))
      %+  (foldl image idx)  [st keys]
      |=  [st=image i=idx]
      image:(xray-shape st i)
  ::
  ++  xray-shape
    |=  [st=image i=idx]
    ^-  [shape =image]
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
        [%fork *]  (fork-shape st set.dat)
        [%face *]  (xray-shape st xray.dat)
        [%pntr *]  !!
      ==
    ::
    =/  y=xray    x                                     ::  Type system hack.
    =.  shape.y   `res
    =.  xrays.st  (~(put by xrays.st) idx.y y)
    [res st]
  ::
  ++  fork-shape
    |=  [st=image fork=(set idx)]
    ^-  [shape image]
    ::
    %+  (foldl (pair shape image) idx)
      [[%void st] ~(tap in fork)]
    |=  [acc=(pair shape image) i=idx]
    ^-  [shape image]
    =^  res  st  (xray-shape q.acc i)
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
++  decorate-xray-image-with-roles
  |^  |=  init=image
      ^-  image
      =/  result
        =/  keys=(list idx)  ~(tap in ~(key by xrays.init))
        %+  (foldl image idx)  [init keys]
        |=  [acc=image =idx]
        ^-  image
        =.  focus.acc  idx
        =^  =role  acc  (xray-role acc)
        acc
      result(focus focus.init)
  ::
  ::  Produce an image focused on the xray for a given type. If the
  ::  type isn't already in the image, create it first.
  ::
  ::  These xrays are for fake types that we create to restructure forks,
  ::  therefore they will never by loops.
  ::
  ++  with-new-xray
    |=  [st=image ty=type d=data]
    ^-  image
    =/  old=(unit idx)  (~(get by type-map.st) ty)
    ?^  old  st(focus u.old)
    =/  idx          next.st
    =/  res=xray     [idx ty `d ~ ~ ~ ~ ~ ~ ~ `%.n]
    =.  next.st      +(idx)
    =.  xrays.st     (~(put by xrays.st) idx.res res)
    =.  type-map.st  (~(put by type-map.st) type.res idx.res)
    =.  focus.st     idx
    st(focus idx)
  ::
  ::  Return an image modified to be focus on the %void type. If no
  ::  void type exists, one will be created.
  ::
  ++  void-xray
    |=  st=image
    (with-new-xray st %void %void)
  ::
  ::  Determines the role of an atom xray.
  ::
  ::  This is trivial enough that it should probably be done inline,
  ::  but it's a nice first example of what the *-role arms do.
  ::
  ++  atom-role
    |=  [st=image =constant=(unit @)]
    ^-  [role image]
    ?~  constant-unit  [%atom st]
    [[%constant u.constant-unit] st]
  ::
  ::  Determine the role of %fork type.
  ::
  ::  First, find (or create) an xray for the union type, then call back
  ::  into `role-xray` to get it's type.
  ::
  ::  The focused xray of the resulting image *will* be decorated with
  ::  role information, the role is just return for convenience.
  ::
  ++  fork-role
    |=  [st=image fork=(set idx)]
    ^-  [role image]
    (xray-role (fork-xray st fork))
  ::
  ::  Calculate the role of a %cell xray.
  ::
  ++  cell-role
    |=  [st=image head=idx]
    ^-  [role image]
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
  ::  Produces an image updated to have role information for the xray
  ::
  ::  Produces an image updated to have role information for the xray
  ::  in focus.
  ::
  ::  The focused xray of the resulting image *will* be decorated with
  ::  role information, the role is just return for convenience.
  ::
  ++  xray-role
    |=  st=image
    ^-  [role image]
    =/  x=xray  (focus st)
    =/  old  role.x
    ?^  old  [u.old st]
    ::
    ::  Hack to prevent infinite loops.
    ::
    ::  =.  role.x  `%thunk
    ::  =.  xrays.st  (~(put by xrays.st) idx.x x)
    ::
    %-  |=  [res=role st=image]
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
      [%pntr *]  !!
      [%fork *]  (fork-role st set.dat)
    ==
  ::
  ::  Create a new xray from a union type.  Returns an image focused
  ::  on the result.
  ::
  ++  fork-xray
    |=  [st=image fork=(set idx)]
    ^-  image
    =.  st  (void-xray st)
    %+  (foldl image idx)
      [st ~(tap in fork)]
    |=  [=image =idx]
    (merge image focus.image idx)
  ::
  ::  Combine two xrays in an image (the one in focus and the one
  ::  referenced by `i`, producing a new image focused on the resulting
  ::  union.
  ::
  ::  First, we compute the role of both xrays, and then we `combine`
  ::  them.
  ::
  ++  merge
    |=  [st=image x=idx y=idx]
    ^-  image
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
  ::  XX I did! I don't remember why, though. I should probably delete
  ::  this logic and test.
  ::
  ++  simple-forks
    |=  xi=image
    ^-  (unit (set idx))
    =/  x=xray  (focus xi)
    ?:  (need loop.x)  ~
    =/  d=data  (need data.x)
    ?.  ?=([%fork *] d)  ~
    `set.d
  ::
  ::  Given two xrays, construct their union type and return it's xray.
  ::
  ::  Returns an `image` focused on the resulting xray.
  ::
  ::  Using the `fork` primitive to construct a new type, get the xray
  ::  for that type. If we already have an xray for that, just return
  ::  it. Otherwise we need to create one. The `data` field for the new
  ::  xray will be (basically) the result of doing a set-merge on the
  ::  trivial-forks of both xrays.
  ::
  ++  join
    |=  [st=image i=idx]
    ^-  image
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
    |=  [st=image thick=(map atom idx) thin=(map atom idx)]
    ^-  [(map atom idx) image]
    ::  ~&  'collate'
    =/  list  ~(tap by thin)
    |-
    ^-  [(map atom idx) image]
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
    |=  [st=image target=idx]
    ^-  image
    ::
    ::  First let's do some setup. Get indicies for this, that, and the
    ::  joined type.
    ::
    =/  this      focus.st
    =/  that      target
    ::
    |-
    ^-  image
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
      |=  [st=image x=idx y=idx =role]
      ^-  image
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
      |=  [st=image =role]
      ^-  image
      (join-with-role st this that role)
    ::
    ::  Convenience functions for creating junctions
    ::
    =/  misjunct  |=  [st=image x=idx y=idx]
                  ~&  ['MISJUNCTION' x y]
                  =/  xx=xray  (focus st(focus x))
                  =/  yy=xray  (focus st(focus y))
                  ~&  [x '=' (need data.xx) (need role.xx)]
                  ~&  [y '=' (need data.yy) (need role.yy)]
                  ::  ?:  %.y  !!
                  (join-with-role st x y [%misjunction x y])
    ::
    =/  conjunct  |=  [st=image wide=idx tall=idx]
                  (join-with-role st wide tall [%conjunction wide tall])
    ::
    =/  junct     |=  [st=image flat=idx deep=idx]
                  (join-with-role st flat deep [%junction flat deep])
    ::
    ::  Join a cell with a junction.
    ::
    =/  cell-junct
      |=  [st=image cell=idx [flat=idx deep=idx]]
      ^-  image
      =.  st  (merge st cell deep)
      =/  deep-merged  focus.st
      (junct st flat deep-merged)
    ::
    ::  Join an atom with a junction.
    ::
    =/  atom-junct
      |=  [st=image atom=idx [flat=idx deep=idx]]
      ^-  image
      =.  st  (merge st atom flat)
      =/  flat-merged  focus.st
      (junct st flat-merged deep)
    ::
    =/  tall-conjunct
      |=  [st=image out-tall=idx [wide=idx in-tall=idx]]
      ^-  image
      =.  st  (merge st out-tall in-tall)
      =/  new-tall  focus.st
      (conjunct st wide new-tall)
    ::
    =/  conjunct-conjunct
      |=  [st=image [xwide=idx xtall=idx] [ywide=idx ytall=idx]]
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
  %-  decorate-xray-image-with-roles
  %-  decorate-xray-image-with-shapes
  %-  decorate-xray-image-with-patterns
  %-  decorate-xray-image-with-loops
  %-  gc-image
  %-  trace-xray-image
  %-  (xray-type 999.999 999.999)
  t
  ::
::
::  -xray-image-to-spec: convert to spec
::
++  xray-image-to-spec
  |=  [=top=idx img=image]
  ^-  spec
  ::
  |^  (xray-to-spec ~ top-idx)
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
    ?.  (need loop.xr)  sp
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
                        %-  ~(run by (flatten-battery battery.d))
                        |=  =idx  ^$(i idx)
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