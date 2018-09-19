/?  310
!:
:-  %say
::  render-hoon
=<  render-type
::  render-all-hoons-referenced-inside-of-type
|%
::
+|  %entry-points-for-testing
::
::  This is like `turn`, except that a state variable `st` is threaded
::  through the entire execution. The list is processed from
::  left-to-right.
::
::  This is basically the same as `mapM` in Haskell, but specialized
::  specialized for lists and the State monad.
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
::  This is a wrapper for `traverse-left` that tries to infer it's arguments.
::
++  traverse-left-auto
   |*  [[xs=(list) st=*] f=$-(^ ^)]
   ^-  [(list _-:*f) _st]
   ?~  xs  [~ st]
   =/  t  (traverse-left _i.xs _-:*f _st)
   (t [xs st] f)
::
::  Same as `traverse-left-auto` but executes state updates in reverse order.
::
++  traverse-right-auto
   |*  [[xs=(list) st=*] f=$-(^ ^)]
   ^-  [(list _-:*f) _st]
   ?~  xs  [~ st]
   =/  t  (traverse-right _i.xs _-:*f _st)
   (t [xs st] f)
::
::  Pretty-print a type.
::
++  render-type
  |=  {^ {{subject=type ~} ~}}
  :-  %txt
  ^-  wain
  =/  xt=xrayed-type  (xray-type subject)
  ?.  (valid-xrayed-type xt)
    ~&  [%invalid-xrayed-type xt]
    !!
  ~
+$  loop-map  (map index zray)
+$  wray  [=meta =data]
+$  zray  [=type =wray]
+$  xray  $@(index zray)
+$  notebook  [=xray =loop-map]
+$  index  @ud
+$  shape
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
  $:  =shape=(unit shape)
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
+$  pattern
  $@  ?(%hoon %manx %nock %path %plum %skin %specl %tape %tour %type %vase)
  $%  [%gate sample=xray product=xray]
      [%gear sample=xray context=xray =battery]
      [%list item=xray]
      [%tree item=xray]
      [%unit item=xray]
  ==
+$  xrayed-type  [=xray table=(map index xray)]
+$  data
  $@  ?(%noun %void)
  $%  [%atom =aura =constant=(unit @)]
      [%cell head=xray tail=xray]
      [%core =garb =xray =battery]
      [%face face=$@(term tune) =xray]
      [%fork =(set xray)]
  ==
::
++  valid-xrayed-type
  |^  enter
  ++  enter
    |=  xt=xrayed-type
    ^-  ?
    =/  err  (find-error-anywhere xt)
    ?~  err  %.y
    ~&  err
    %.n
  ++  collapse-errors
    |=  es=(list (unit @t))
    ^-  (unit @t)
    ?~  es    ~
    ?^  i.es  i.es
    $(es t.es)
  ++  check-lookup
    |=  [idx=@ tbl=(map index xray)]
    ^-  (unit @t)
    =/  mbx=(unit xray)  (~(get by tbl) idx)
    ?~  mbx  [~ 'LOOP INDEX NOT IN TABLE']
    =*  res  u.mbx
    ?@  res  [~ 'LOOP INDEX RESOLVES TO ANOTHER LOOP INDEX']
    ~
  ++  check-entry-point
    |=  [idx=@ xt=xrayed-type]
    ?@  xray.xt  [~ 'ENTRY POINT IS LOOP INDEX']
    =/  mbe  (check-lookup idx table.xt)
    ?^  mbe  mbe
    =/  ent  entry-unit.meta.wray.xray.xt
    ?~  ent  [~ 'ENTRY POINT WITH EMPTY `entry-unit`']
    ?.  =(idx u.ent)
      [~ 'LOOP INDEX RESOLVES TO XRAY WITH MISMATCHING `entry-unit`']
    =/  res  (~(got by table.xt) idx)
    ?.  =(res xray.xt)
      [~ '`entry-unit` IS SET BUT DOES NOT SELF-RESOLVE']
    ~
  ::
  ++  check-metadata
    |=  xt=xrayed-type
    ^-  (unit @t)
    ?@  xray.xt  ~
    =/  entry  entry-unit.meta.wray.xray.xt
    ?~  entry  ~
    (check-entry-point u.entry xt)
  ::
  ++  subxrays-in-battery
    |=  b=battery
    ^-  (list xray)
    %-  zing
    %+  turn  ~(val by b)
    |=  [* =(map term xray)]
    ^-  (list xray)
    ~(val by map)
  ::
  ++  subxrays
    |=  x=xray
    ^-  (list xray)
    ?@  x  ~
    =/  d  data.wray.x
    ?+    d
        ~
      [%cell *]  ~[head.d tail.d]
      [%core *]  [xray.d (subxrays-in-battery battery.d)]
      [%face *]  ~[xray.d]
      [%fork *]  ~(tap in set.d)
    ==
  ::
  ++  find-error-anywhere
    |=  xt=xrayed-type
    =/  mbe  (find-error xt)
    ?^  mbe  mbe
    %-  collapse-errors
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
    %-  collapse-errors
    %+  turn  (subxrays xray.xt)
    |=  x=xray  (find-error xt(xray x))
  --
:: +$  wray  [=meta =data]
:: +$  zray  [=type =wray]
:: +$  xray  $@(index zray)

::
++  xray-type
  |^  enter
  ::
  +$  trace        (set type)
  +$  entry-table  (map type [=index =zray])
  +$  state        [count=@ud table=entry-table]
  ::
  +*  chap  [a]  (pair term (pair what (map term a)))
  +*  arm  [a]   (pair term a)
  ::
  ++  trav-chaps  (traverse-right (chap hoon) (chap xray) state)
  ++  trav-arms   (traverse-right (arm hoon) (arm xray) state)
  ::
  ++  enter
    |=  ty=type
    ^-  xrayed-type
    :: ~&  [%enter ty]
    |^  =+  [xray st]=(entry [~ ty] *state)
        [xray (build-loop-map table.st)]
    ::
    ::  Given an `entry-table` (which we use to check if a given type is
    ::  an entry-point), produce a `loop-map`. In following analysis phases,
    ::  we will be traversing xrays and we need to lookup looks when we
    ::  come across them.  The `loop-map` structure encodes that.
    ::
    ++  build-loop-map
      |=  table=entry-table
      :: ~&  [%build-loop-map table]
      ^-  (map index zray)
      %-  ~(gas by *(map index zray))
      %+  turn  ~(tap by table)
      |=  [type [=index =zray]]
      [index zray]
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
      :: ~&  [%entry ty]
      =/  old  (~(get by table.st) ty)
      ?^  old  [index.u.old st]
      ?:  (~(has in tr) ty)
        =/  result  `xray`count.st
        =/  stub    [ty [count.st *zray]]
        =.  st      ^-(state [+(count.st) (~(put by table.st) stub)])
        [result st]
      =^  result  st  (main [(~(put in tr) ty) ty] st)
      ?@  result  [result st]
      =/  new  (~(get by table.st) ty)
      ?~  new  [result st]
      =*  idx  index.u.new
      =.  entry-unit.meta.wray.result  `idx
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
      :: ~&  [%main ty]
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
      :: ~&  [%core payload-type coil]
      =^  payload-xray  st  (main [tr payload-type] st)
      =^  chapters=(list (chap xray))  st
        %+  trav-chaps  [~(tap by q.r.coil) st]
        |=  [chap=(chap hoon) st=state]
        =-  :_  ->
            :+  p.chap  `what`p.q.chap  (~(gas by *(map term xray)) -<)
        %+  trav-arms  [~(tap by q.q.chap) st]
        |=  [arm=(arm hoon) st=state]
        =*  hold-type  [%hold [%core payload-type coil] q.arm]
        =^  xray  st  (main [tr hold-type] st)
        [arm(q xray) st]
      =*  chaps   (~(gas by *(map term (pair what (map term xray)))) chapters)
      =*  result  `wray`[*meta [%core p.coil payload-xray chaps]]
      [result st]
    ::
    ::  Analyze a %hint type.
    ::  subject-type: subject of note
    ::  note: hint information
    ::  content-type: type of hinted content
    ::  content-type: type of hinted content
    ++  hint
      |=  [[tr=trace [=subject=type =note] =payload=type] st=state]
      ^-  [wray state]
      :: ~&  [%hint subject-type note payload-type]
      ::
      =*  get-xray-by-loop-index  ~(got by (build-loop-map table.st))
      =^  result=xray  st  (main [tr payload-type] st)
      ::
      |-
      ^-  [wray state]
      ?@  result  $(result (get-xray-by-loop-index result))
      ?-    -.note
          %help
        =.  comment-set.meta.wray.result
          (~(put in comment-set.meta.wray.result) p.note)
        [wray.result st]
          %know
        =.  standard-set.meta.wray.result
          (~(put in standard-set.meta.wray.result) p.note)
        [wray.result st]
          %made
        =^  =recipe  st
          ?~  q.note
            [[%direct p.note] st]
          =-  [`recipe`[%synthetic p.note -<] `state`->]
          |-
          ^-  [(list xray) state]
          ?~  u.q.note  [~ st]
          =*  wut  [%tsld [%limb %$] [%wing i.u.q.note]]
          =*  part  (~(play ut subject-type) wut)
          =^  this  st  (entry [tr part] st)
          =^  more  st  $(u.q.note t.u.q.note)
          [[this more] st]
        =.  recipe-set.meta.wray.result
          (~(put in recipe-set.meta.wray.result) recipe)
        [wray.result st]
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
        %+  traverse-left-auto  [~(tap in type-set) st]
        |=  [ty=type st=state]
        (main [tr ty] st)
      :_  st  `wray`[*meta %fork (~(gas in *(set xray)) xrays)]
    --
  --
--
