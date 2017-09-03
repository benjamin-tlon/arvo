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
=<  %hello
|%
++  ut
  =+  :*  fan=*(set {span twig})
          rib=*(set {span span twig})
          vet=`?`&
          fab=`?`&
      ==
  =+  sut=`span`%noun
  |%
  ++  nest
    |=  {tel/? ref/span}
    ^-  ?
    !!
  ::
  ++  peek
    |=  {way/?($read $rite $both $free) axe/axis}
    ^-  span
    !!
  ::
  ++  repo  `span`!!
  ++  tool  $@(term tomb)                               ::  name enhancement
  ::                                                    ::
  ++  miss  !:                                          ::  nonintersection
    |=  $:  ::  ref: symmetric span
            ::
            ref/span
        ==
    ::  intersection of sut and ref is empty
    ::
    ^-  ?
    =|  gil/(set (set span))
    =<  dext
    |%
    ++  dext
      ^-  ?
      ::  
      ?:  =(ref sut)
        (nest(sut %void) | sut)
      ?-  sut
        $void      &
        $noun      (nest(sut %void) | ref)
        {$atom *}  sint
        {$cell *}  sint
        {$core *}  sint(sut [%cell p.sut %noun])
        {$fork *}  %+  levy  (~(tap in p.sut))
                   |=(span dext(sut +<))
        {$face *}  dext(sut q.sut) 
        {$hold *}  =+  (~(gas in *(set span)) `(list span)`[sut ref ~])
                   ?:  (~(has in gil) -)
                      &
                   %=  dext
                     sut  repo
                     gil  (~(put in gil) -)
      ==           ==
    ++  sint
      ?+  ref      dext(sut ref, ref sut)
        {$atom *}  ?.  ?=({$atom *} sut)  &
                   ?&  ?=(^ q.ref)
                       ?=(^ q.sut)
                       !=(q.ref q.sut)
                   ==
        {$cell *}  ?.  ?=({$cell *} sut)  &
                   ?&  dext(sut p.sut, ref p.ref)
                       dext(sut q.sut, ref q.ref)
      ==           ==
    --
  --
--
