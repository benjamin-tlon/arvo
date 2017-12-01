

=>  |%
    ++  tope                                            ::  topographic type
      $@  $?  %&                                        ::  cell or atom
              %|                                        ::  atom
          ==                                            ::
      (pair tope tope)                                  ::  cell
    --
++  al
  ~%    %al
      +>+
    ==
      %bunt  bunt
      %whip  whip
    ==
  =+  :*  top=`tope`& 
          dom=`axis`1
          wat=*what
      ==
  |_  mod/tile
          dom/axis
      ==
  ++  home  
    ::  express a hoon against the original subject
    ::
    |=(gen/hoon ^-(hoon ?:(=(1 dom) gen [%tsgr [%$ dom] gen])))
  ::
  ++  hail
    ::  add help
    ::
    |=  gen/hoon
    ^-  hoon
    ?~(wat gen [%docs u.wat gen])
  ::
  ++  default
    ::  produce a hoon that makes the model's default value, untyped
    ::
    |-  ^-  hoon
    ?-    mod
        {^ *}
      [$(mod -.mod) $(mod +.mod)]
    ::
        {$axil *}
      ?+  p.mod  [%sand %$ 0]
        $cell  [[%sand %$ 0] [%sand %$ 0]]
        $void  [%zpzp ~]
      ==
    ::
        {$bark *}
      $(mod q.mod)
    ::
        {$herb *}
      =+  cys=~(boil ap p.mod)
      ?:  ?=($herb -.cys)
        (home [%tsgl [%limb %$] p.mod])
      $(mod cys)
    ::  
        {$deet *}
      $(mod q.mod)
    ::
        {$fern *}
      |-  ^-  hoon
      ?~(t.p.mod ^$(mod i.p.mod) $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod))
    ::
        {$kelp *}
      |-  ^-  hoon
      ?~(t.p.mod ^$(mod i.p.mod) $(i.p.mod i.t.p.mod, t.p.mod t.t.p.mod))
    ::
        {$leaf *}
      [%rock p.mod q.mod]
    ::
        {$plow *}
      $(mod q.mod)
    ::
        {$reed *}
      $(mod p.mod)
    ::
        {$vine *}
      $(mod q.mod)
    ::
        {$weed *}
      (home p.mod)
    ==
  ::
  ++  sample
    ::  operate against one sample of the subject
    ::
    |_  $:  ::  axe: axis to sample
            ::  top: topographic type of sample
            ::
            axe/axis
            top/tope 
        ==
    ++  match 
      ::  match one choice from list
      ::
      |=  $:  ::  sed: kind of rune
              ::  one: first choice
              ::  rep: rest of choices
              ::
              one/tile
              rep/tile
          ==
      ::  if no other choices, construct head
      ::
      ?~  rep  construct(mod one)
    ++
        
    ++  assemble
  ::
  ++  construct
    ::  constructor at arbitrary sample
    ::
    ^-  hoon
    ?-    mod
        {^ *}
      ::  apply and clear help
      ::
      %-  hail
      =.  wat  ~
      ::  check topography of sample
      ::
      ?^  top
        ::  if known cell, descend directly
        ::
        :-  $(mod -.mod, top p.top, axe (peg axe 2))
        $(mod +.mod, top q.top, axe (peg axe 3))
      ::  otherwise, build constructor gate
      ::
      :+  %tsgr
        :^  %brts  ~^~
          [%base %noun]
        =:  dom  (peg 7 dom)
            axe  6
            top  &
          ==
        [$(mod -.mod) $(mod +.mod)]
      ::  boc: call constructor
      ::  but: default, cast to call
      ::
      =/  boc/hoon  [%limb %$]
      =/  but/hoon  [%ktls boc default]
      ?:  top
        ::  may be atom or cell; default or construct
        ::
        [%wtpt [[%& axe] ~] but boc]
      ::  must be atom; construct
      ::
      but
    ::
        {%fern *}
      ::  pit: core construction loop
      ::
      =/  pit/hoon
      |-  ^-  hoon
      ::  default to the last 
      ::
      ?~  t.p.mod  ^$(mod i.p.mod)
      =/  mor/hoon  $(mod 

      
      =.  dom  (peg dom 7)
      
      ?-  top
        %|  
        {$axil *}
      =
      ?-  p.
      ?-  top
        
    ==
  --
