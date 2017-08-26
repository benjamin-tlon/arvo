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
=-  %hello
|%
++  nl
  ::                                                      ::
  ++  ly                                                  ::  
    |*  a/(list)
      ^+  =<  $
        |%  +-  $  ?:(*? ~ [i=(snag 0 a) t=$])
    --
     
    ^+  ?~  a
          ~
        $(
    a

  ++  hymo                                                ::  homogenize
    |*  x/(list)
    ?~  x
      ~
    [i=(snyg 0 x) t=$(x t.x)]
  ::
  ++  snag                                                ::  index
    ~/  %snag
    |*  {a/@ b/(list)}
    ?~  b
      ~|('snag-fail' !!)
    ?:  =(0 a)  i.b
    $(b t.b, a (dec a))
  ::
  ++  mylt                                                ::  map from pair list
    |*  u/(list)
    =+  foo=(hymo u)
    (wele foo)
  ::
  ++  wele                                                ::  concatenate
    |*  q/(list)
    0
  ::
  ++  welo                                                ::  concatenate
    ~/  %weld
    |*  {a/(list) b/(list)}
    =>  .(a `(ly a)`a, b `(ly b)`b)
    
    ^.(homo a), b ^.(homo b))
    =-  ^.(homo -)
    |-
    ?~  a  b
    [i=i.a t=$(a t.a)]
  --
