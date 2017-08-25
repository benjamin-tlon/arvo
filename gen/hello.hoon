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
++  hymo                                                ::  homogenize
  |*  x/(list)
  ?~  x
    ~
  [i=(snyg 0 x) t=$(x t.x)]
::
++  snyg                                                ::  index
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
--
