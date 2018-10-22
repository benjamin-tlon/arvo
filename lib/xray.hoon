::  # Type Analysis
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
::  - XX It should be possible to wrote a `traverse-battery` routine.
::
::  - XX Finish the logic for printing a noun given a fork.
::
::  - XX Find some way to test the shit out of the fork logic.
::
/?  310
::
/-  *xray
::
|^  ^-  $:  ximage-to-spec=$-(=ximage =spec)
            xray-type=$-([@ type] ximage)
            focus-on=$-([xtable key] xray)
        ==
    [ximage-to-spec analyze-type-and-decorate focus-on]
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
                 ::  ~&  q.ty
                 ::  ~&  'with a context of type?'
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
      %+  (traverse (chap hoon) (chap key) state)
        [~(tap by q.r.coil) st]
      |=  [c=(chap hoon) st=state]
      =^  l=(list (arm key))  st
        ^-  [(list (arm key)) state]
        %+  (traverse (arm hoon) (arm key) state)
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
    =/  subtype  (~(nest ut ty) | type.x)
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
    |=  head=xray
    ^-  role
   ::
    =/  =shape  (need shape.head)
    =/  =data   (need data.head)
   ::
    =/  const  ?.  ?=([%atom *] data)  ~
               constant.data
    ::
    ?:  =(shape %cell)  %wide
    ?^  const           [%instance u.const]
    %tall
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
        [%core *]  :_  st  %wide  ::  The head of a core is a battery
                                  ::  which is always a cell.
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
        %tall  ?:  ?=(%wide that-role)  %tall
               [%junction that this]
        %wide  ?:  ?=(%tall that-role)  %tall
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
        %tall  ?-  -.this-role
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
