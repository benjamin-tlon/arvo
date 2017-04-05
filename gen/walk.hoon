!:
::::
  ::
:-  %say
|=  $:  {now/@da * bec/beak}
        *
    ==
=<  :-  %noun
    %hello
|%
++  docs
  |%
  ::  forward line
  ::
  ++  apex
    %+  cook
      |=  $:  a/(unit cord)
              b/(unit (pair cord (list sect)))
              c/(list (pair (pair term cord) (list sect)))
          ==
      [a b c]
    ;~  plug
      (punt (into head))
      (punt body)
      (star fill)
    ==
  ::
  ::
  ++  body
    ;~  plug
      (into line)
      (rant text)
    ==
  ::  backward line
  ::
  ++  apse
    (exit ;~(pose fine line))
  ::
  ::  null: blank line
  ::  line: prose line
  ::  code: code line
  ::  text: text line
  ::  fine: definition line
  ::
  ++  line  (cook crip ;~(plug prz (star prn)))
  ++  head  ;~(pfix ;~(plug tar tar ace ace cen) sym)
  ++  text  (cook |=(a/pica a) (pick line code))
  ++  code  (cook crip ;~(pfix ;~(plug ace ace ace ace) (star prn)))
  ++  null  (star ace)
  ++  fine  ;~(plug sym (cook crip ;~(pfix ;~(plug col ace) (star prn))))
  ::
  ::  lean: line delimited
  ::
  ++  lean
    |*  gyf/rule
    |*  bod/rule
    (ifix [;~(plug col gyf ace ace) (just `@`10)] bod)
  ::
  ::  into: :> to end of line, consuming following space.
  ::
  ++  into  
    |*  bod/rule
    ;~(sfix ((lean gar) bod) (punt gap))
  ::
  ::  exit: :< to end of line, not consuming following space.
  ::
  ++  exit
    |*  bod/rule
    ;~(sfix ((lean gal) bod) (punt gap))
  ::
  ::  fill: full definition
  ::
  ++  fill
    ;~(plug fine (rant ;~(pfix ;~(plug ace ace) text)))
  ::
  ::  rant: series of sections.
  ::
  ++  rant
    |*  sec/rule
    ;~  pose
      ;~  pfix 
        (into null) 
        (star ;~(sfix (plus (into sec)) (into null)))
      ==
      (easy ~)
    ==
  --
--
