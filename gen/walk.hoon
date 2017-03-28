::  "Hello world" sample generator
::
::::  /hoon/hello/gen
  ::
/?    310
::
::::
  ::
:-  %say
|=  *
=<  [%noun %walk]
|%
::
++  ap                                                  ::  twig engine
  |_  gen/twig
  ++  dock                                              ::  apply document
    |=  doc/cord
    |^  ^-  (unit twig)
        =/  rah  ^-  (each (pair term cord) cord)
                 (rash doc very)
        ?-(-.rah $& (rave p.rah), $| (graf p.rah))
    ::
    ++  graf                                            ::  apply general doc
      |=  doc/cord
      ^-  (unit twig)
      =-  ?^  q.dep  `[%help [doc ~] gen]
          `p.dep
      ^=  dep
      ^-  (pair twig (unit cord))
      %+  (walk (unit cord))
        `doc
      |=  {gen/twig vit/(unit cord)}
      ^-  (unit (pair twig (unit cord)))
      ::  XX stub
      ~
    ::
    ++  rave                                            ::  apply variable doc
      |=  {cog/term doc/cord}
      ^-  (unit twig)
      =-  ?^  q.dep  ~
          `p.dep
      ^=  dep
      ^-  (pair twig (unit (pair term cord)))
      %+  (walk (unit (pair term cord)))
        `[cog doc]
      |=  {gen/twig vit/(unit (pair term cord))}
      ^-  (unit (pair twig (unit (pair term cord))))
      ::  XX stub
      ~
    ::
    ++  very                                            ::  variable cord rule
      %+  pick
        ;~(plug sym (cook crip ;~(pfix ;~(plug col ace) (star prn))))
      (cook crip (star prn))
    --
  ::
  ++  walk                                              ::  forward traverse
    |*  life/mold
    |=  $:  vit/life
            mac/$-({twig life} (unit (pair twig life)))
        ==
    ^-  {twig life}
    =/  gun  (mac gen vit)
    ?^  gun  u.gun
    |^  ^-  {twig life}
        ?:  ?=(^ -.gen)
          %.(gen double)
        ?-  -.gen
          $$      (compose -.gen %.(+.gen default))
          $base   (compose -.gen %.(+.gen default))
          $bunt   (compose -.gen %.(+.gen single))
          $bust   (compose -.gen %.(+.gen default))
          $dbug   (compose -.gen %.(+.gen single-pre))
          $hand   (compose -.gen %.(+.gen default))
          $knit   (compose -.gen %.(+.gen nifty))
          $leaf   (compose -.gen %.(+.gen default))
          $limb   (compose -.gen %.(+.gen default))
          $lost   (compose -.gen %.(+.gen single))
          $rock   (compose -.gen %.(+.gen default))
          $sand   (compose -.gen %.(+.gen default))
          $tell   (compose -.gen %.(+.gen repeat))
        ::::::::::::::::::
          $tune   (compose -.gen %.(+.gen default))
          $wing   (compose -.gen %.(+.gen default))
          $yell   (compose -.gen %.(+.gen default))
          $claw   (compose -.gen %.(+.gen default))
          $shoe   (compose -.gen %.(+.gen default))
          $bank   (compose -.gen %.(+.gen default))
          $book   (compose -.gen %.(+.gen default))
          $lamb   (compose -.gen %.(+.gen default))
          $bush   (compose -.gen %.(+.gen default))
          $pick   (compose -.gen %.(+.gen default))
          $coat   (compose -.gen %.(+.gen default))
          $door   (compose -.gen %.(+.gen default))
          $gasp   (compose -.gen %.(+.gen default))
          $core   (compose -.gen %.(+.gen default))
          $trap   (compose -.gen %.(+.gen default))
          $cork   (compose -.gen %.(+.gen default))
          $loop   (compose -.gen %.(+.gen default))
          $port   (compose -.gen %.(+.gen default))
          $gill   (compose -.gen %.(+.gen default))
          $gate   (compose -.gen %.(+.gen default))
          $tray   (compose -.gen %.(+.gen default))
          $scon   (compose -.gen %.(+.gen default))
          $conq   (compose -.gen %.(+.gen default))
          $cons   (compose -.gen %.(+.gen default))
          $cont   (compose -.gen %.(+.gen default))
          $conl   (compose -.gen %.(+.gen default))
          $conp   (compose -.gen %.(+.gen default))
          $keep   (compose -.gen %.(+.gen default))
          $lace   (compose -.gen %.(+.gen default))
          $call   (compose -.gen %.(+.gen default))
          $bake   (compose -.gen %.(+.gen default))
          $calq   (compose -.gen %.(+.gen default))
          $calt   (compose -.gen %.(+.gen default))
          $open   (compose -.gen %.(+.gen default))
          $make   (compose -.gen %.(+.gen default))
          $wish   (compose -.gen %.(+.gen default))
          $bump   (compose -.gen %.(+.gen default))
          $nock   (compose -.gen %.(+.gen default))
          $same   (compose -.gen %.(+.gen default))
          $deep   (compose -.gen %.(+.gen default))
          $iron   (compose -.gen %.(+.gen default))
          $ward   (compose -.gen %.(+.gen default))
          $like   (compose -.gen %.(+.gen default))
          $cast   (compose -.gen %.(+.gen default))
          $zinc   (compose -.gen %.(+.gen default))
          $burn   (compose -.gen %.(+.gen default))
          $name   (compose -.gen %.(+.gen default))
          $lead   (compose -.gen %.(+.gen default))
          $help   (compose -.gen %.(+.gen default))
          $show   (compose -.gen %.(+.gen default))
          $lurk   (compose -.gen %.(+.gen default))
          $fast   (compose -.gen %.(+.gen default))
          $funk   (compose -.gen %.(+.gen default))
          $thin   (compose -.gen %.(+.gen default))
          $hint   (compose -.gen %.(+.gen default))
          $poll   (compose -.gen %.(+.gen default))
          $memo   (compose -.gen %.(+.gen default))
          $dump   (compose -.gen %.(+.gen default))
          $ddup   (compose -.gen %.(+.gen default))
          $warn   (compose -.gen %.(+.gen default))
          $peep   (compose -.gen %.(+.gen default))
          $wad    (compose -.gen %.(+.gen default))
          $nub    (compose -.gen %.(+.gen default))
          $dip    (compose -.gen %.(+.gen default))
          $fry    (compose -.gen %.(+.gen default))
          $new    (compose -.gen %.(+.gen default))
          $fix    (compose -.gen %.(+.gen default))
          $var    (compose -.gen %.(+.gen default))
          $rev    (compose -.gen %.(+.gen default))
          $set    (compose -.gen %.(+.gen default))
          $huh    (compose -.gen %.(+.gen default))
          $rap    (compose -.gen %.(+.gen default))
          $nip    (compose -.gen %.(+.gen default))
          $per    (compose -.gen %.(+.gen default))
          $sip    (compose -.gen %.(+.gen default))
          $pin    (compose -.gen %.(+.gen default))
          $tow    (compose -.gen %.(+.gen default))
          $aka    (compose -.gen %.(+.gen default))
          $use    (compose -.gen %.(+.gen default))
          $or     (compose -.gen %.(+.gen default))
          $case   (compose -.gen %.(+.gen default))
          $if     (compose -.gen %.(+.gen default))
          $lest   (compose -.gen %.(+.gen default))
          $ifcl   (compose -.gen %.(+.gen default))
          $deny   (compose -.gen %.(+.gen default))
          $sure   (compose -.gen %.(+.gen default))
          $deft   (compose -.gen %.(+.gen default))
          $and    (compose -.gen %.(+.gen default))
          $ifat   (compose -.gen %.(+.gen default))
          $ifno   (compose -.gen %.(+.gen default))
          $fits   (compose -.gen %.(+.gen default))
          $not    (compose -.gen %.(+.gen default))
          $twig   (compose -.gen %.(+.gen default))
          $wrap   (compose -.gen %.(+.gen default))
          $spit   (compose -.gen %.(+.gen default))
          $code   (compose -.gen %.(+.gen default))
          $need   (compose -.gen %.(+.gen default))
          $fail   (compose -.gen %.(+.gen default))
        ==
    ++  compose
      |*  {sem/@tas out/{* life}}
      ^-  {twig life}
      [[sem -.out] +.out]
    ::
    ++  default
      |*  *
      [+< vit]
    ::
    ++  single
      |=  p/twig
      ^^$(gen p)
    ::
    ++  single-pre
      |*  bud/{p/* q/twig}
      =^  one  vit  ^^$(gen q.bud)
      [[p.bud one] vit]
    ::
    ++  double
      |=  bud/{p/twig q/twig}
      =^  one  vit  ^^$(gen p.bud)
      =^  two  vit  ^^$(gen q.bud)
      [[one two] vit]
    ::
    ++  triple
      |=  bud/{p/twig q/twig r/twig}
      =^  one  vit  ^^$(gen p.bud)
      =^  two  vit  ^^$(gen q.bud)
      =^  tri  vit  ^^$(gen r.bud)
      [[one two] vit]
    ::
    ++  repeat
      |=  bud/{p/(list twig)}
      =|  out/(list twig)
      |-  ^-  {(list twig) life}
      ?~  p.bud
        [(flop out) vit]
      =^  nex  vit  ^^^$(gen i.p.bud)
      $(p.bud t.p.bud, out [nex out])
    ::
    ++  nifty
      |=  bud/{p/(list woof)}
      =|  wof/(list woof)
      |-  ^-  (pair (list woof) life)
      ?~  p.bud  [(flop wof) vit]
      =^  ned  vit
        ?-  i.p.bud
          @   [i.p.bud vit]
          ^   =^  deg  vit  ^^^$(gen p.i.p.bud)
              [[~ deg] vit]
        ==
      $(p.bud t.p.bud, wof [ned wof])
    --
  --
--
