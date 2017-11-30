

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
          gom=`axis`1
          wat=*what
      ==
  |_  mod/tile
          gom/axis
      ==
  ++  home  
    ::  express a hoon against the original subject
    ::
    |=(gen/hoon ^-(hoon ?:(=(1 gom) gen [%tsgr [%$ gom] gen])))
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
  ++  bu
  ::
  ++  construct
    ::  produce a constructor that works against axis
    ::
    |=  $:  axe/axis
            top/tope 
        ==
    ^-  hoon
    ?-    mod
        {^ *}
      =<  
      
      ?-  top
        %|  
        {$axil *}
      =
      ?-  p.
      ?-  top
        
    ==
  --
