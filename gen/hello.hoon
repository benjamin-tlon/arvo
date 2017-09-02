::  "Hello world" sample generator
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  !:
:-  %say
|=  *
:-  %noun
^%
=-  [%hello test]
|%

  ::
  ++  redo  !:
  |=  ::  ref: naming reference 
      ::
      ref/span

  ::                                                    ::  
  ++  redo  !:                                          ::  refurbish
    |=  $:  ::  ref: raw payload
            ::
            ref/span
        ==
        ::  sut reformatted 
        ::
    ^-  span
    ::  equal reference tells us nothing
    ::
    ?:  =(sut ref)  sut
    ::  errors here imply subject/reference mismatch
    ::
    ~|  %redo-match 

++  test
  :*  (snag:nx 2 3 4 5 ~)
      (le:nx 1 2 3 4 ~)
      (nop:nx 1 2 3 4 ~)
      `(list (weld:nx [[1 2 3 3 ~] [4 5 6 ~]])
  ==
::
++  nx
  |%
  ::                                                      ::
  ++  le                                                  ::  construct list
    |*  a/(list)
    ^+  =<  $
      |%  +-  $  ?:(*? ~ [i=(snag 0 a) t=$])
      --
    a
  ::                                                      ::
  ++  my                                                  ::  construct map
    |*  a/(list (pair))
    =>  .(a ^+((le a) a))
    (~(gas by `(map _p.i.-.a _q.i.-.a)`~) a)
  ::                                                      ::
  ++  si                                                  ::  construct set
    |*  a/(list)
    =>  .(a ^+((le a) a))
    (~(gas in `(set _i.-.a)`~) a)
  ::                                                      ::
  ++  snag                                                ::  index
    |*  {a/@ b/(list)}
    ~!  b
    ?~  b
      ~|('snag-fail' !!)
    ?:  =(0 a)  i.b
    $(b t.b, a (dec a))
  ::                                                      ::
  ++  weld                                                ::  concatenate
    |*  {x/(list) y/(list)}
    ~!  .
    ::  =>  .(x (le x))
    [x y]
  ::
  ++  nop
    |*  {a/(list *)}
    ~!  a
    =>  .(a ^+((le a) a))
    a
  --
::::
::++  jo                                                  ::  json reparser
::  =>  |%  ++  grub  (unit *) 
::          ++  fist  $-(json grub)
::      --
::  |%
::  ++  ar                                                ::  array as list
::    |*  wit/fist
::    |=  jon/json
::    ?.  ?=({$a *} jon)  ~
::    %-  zl
::    |-  
::    ?~  p.jon  ~
::    [i=(wit i.p.jon) t=$(p.jon t.p.jon)]
::  ::
::  ++  at                                                ::  array as tuple
::    |*  wil/(pole fist)
::    |=  jon/json
::    ?.  ?=({$a *} jon)  ~
::    =+  raw=((at-raw wil) p.jon)
::    ?.((za raw) ~ (some (zp raw)))
::  ::
::  ++  at-raw                                            ::  array as tuple
::    |*  wil/(pole fist)
::    |=  jol/(list json)
::    ?~  wil  ~
::    :-  ?~(jol ~ (-.wil i.jol))
::    ((at-raw +.wil) ?~(jol ~ t.jol))
::  ::
::  ++  bo                                                ::  boolean
::    |=(jon/json ?.(?=({$b *} jon) ~ [~ u=p.jon]))
::  ::
::  ++  bu                                                ::  boolean not
::    |=(jon/json ?.(?=({$b *} jon) ~ [~ u=!p.jon]))
::  ::
::  ++  ci                                                ::  maybe transform
::    |*  {poq/gate wit/fist}
::    |=  jon/json
::    (biff (wit jon) poq)
::  ::
::  ++  cu                                                ::  transform
::    |*  {poq/gate wit/fist}
::    |=  jon/json
::    (bind (wit jon) poq)
::  ::
::  ++  da                                                ::  UTC date
::    |=  jon/json
::    ?.  ?=({$s *} jon)  ~
::    (bind (stud p.jon) |=(a/date (year a)))
::  ::
::  ++  di                                                ::  millisecond date
::    %+  cu
::      |=  a/@u  ^-  @da
::      (add ~1970.1.1 (div (mul ~s1 a) 1.000))
::    ni
::  ::
::  ++  mu                                                ::  true unit
::    |*  wit/fist
::    |=  jon/json
::    ?~(jon (some ~) (bind (wit jon) some))
::  ::
::  ++  ne                                                ::  number as real
::    |=  jon/json
::    ^-  (unit @rd)
::    !!
::  ::
::  ++  ni                                                ::  number as integer
::    |=  jon/json 
::    ?.  ?=({$n *} jon)  ~
::    (rush p.jon dem)
::  ::
::  ++  no                                                ::  number as cord
::    |=  jon/json
::    ?.  ?=({$n *} jon)  ~
::    (some p.jon)
::  ::
::  ++  of                                                ::  object as frond
::    |*  wer/(pole {cord fist})
::    |=  jon/json
::    ?.  ?=({$o {@ *} $~ $~} jon)  ~
::    |-
::    ?~  wer  ~
::    ?:  =(-.-.wer p.n.p.jon)  
::      ((pe -.-.wer +.-.wer) q.n.p.jon)
::    ((of +.wer) jon)
::  ::
::  ++  ot                                                ::  object as tuple
::    |*  wer/(pole {cord fist})
::    |=  jon/json
::    ?.  ?=({$o *} jon)  ~
::    =+  raw=((ot-raw wer) p.jon)
::    ?.((za raw) ~ (some (zp raw)))
::  ::
::  ++  ot-raw                                            ::  object as tuple
::    |*  wer/(pole {cord fist})
::    |=  jom/(map @t json)
::    ?~  wer  ~
::    =+  ten=(~(get by jom) -.-.wer)
::    [?~(ten ~ (+.-.wer u.ten)) ((ot-raw +.wer) jom)]
::  ::
::  ++  om                                                ::  object as map
::    |*  wit/fist
::    |=  jon/json
::    ?.  ?=({$o *} jon)  ~
::    (zm (~(run by p.jon) wit))
::  ::
::  ++  op                                                ::  parse keys of map
::    |*  {fel/rule wit/fist}
::    %+  cu  my
::    %-  ci  :_  (om wit)
::    |=  a/(map cord _(need *wit))
::    ^-  (unit (list _[(wonk *fel) (need *wit)]))
::    %-  zl
::    %+  turn  (~(tap by a))
::    |=  {a/cord b/_(need *wit)}
::    =+  nit=(rush a fel) 
::    ?~  nit  ~
::    (some [u.nit b])
::  ::
::  ++  pe                                                ::  prefix
::    |*  {pre/* wit/fist}
::    (cu |*(* [pre +<]) wit)
::  ::
::  ++  sa                                                ::  string as tape
::    |=  jon/json
::    ?.(?=({$s *} jon) ~ (some (trip p.jon)))
::  ::
::  ++  so                                                ::  string as cord
::    |=  jon/json
::    ?.(?=({$s *} jon) ~ (some p.jon))
::  ::
::  ++  su                                                ::  parse string
::    |*  sab/rule
::    |=  jon/json
::    ?.  ?=({$s *} jon)  ~
::    (rush p.jon sab)
::  ::
::  ++  ul  |=(jon/json ?~(jon (some ~) ~))               ::  null
::  ++  za                                                ::  full unit pole
::    |*  pod/(pole (unit))
::    ?~  pod  &
::    ?~  -.pod  |
::    (za +.pod)
::  ::
::  ++  zl                                                ::  collapse unit list
::    |*  lut/(list (unit))
::    ?.  |-  ^-  ?
::        ?~(lut & ?~(i.lut | $(lut t.lut)))
::      ~
::    %-  some
::    |-
::    ?~  lut  ~
::    [i=u:+.i.lut t=$(lut t.lut)]
::  ::
::  ++  zp                                                ::  unit tuple
::    |*  but/(pole (unit))
::    ?~  but  !!
::    ?~  +.but  
::      u:->.but
::    [u:->.but (zp +.but)]
::  ::
::  ++  zm                                                ::  collapse unit map
::    |*  lum/(map term (unit))
::    ?:  (~(rep by lum) |=({{@ a/(unit)} b/_|} |(b ?=($~ a))))
::      ~
::    (some (~(run by lum) need))
::  --
--
