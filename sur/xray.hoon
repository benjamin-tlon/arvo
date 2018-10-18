/?  310
::
|%
::
+$  ximage  [root=key =xtable]
::
+$  key  @
::
+$  xtable  [next=key xrays=(map key xray) =type=(map type key)]
::
+$  shape  ?(%void %noun %atom %cell %junc)
::
+$  xray
  $:  =key
      =type
      data=(unit data)
      role=(unit role)
      pats=(unit pattern)
      studs=(set stud)
      recipes=(set recipe)
      helps=(set help)
      shape=(unit shape)
      loop=(unit ?)
  ==
::
+$  role
  $@  $?  %atom  %cell  %noun  %void  %wide  ==
  $%  [%constant =atom]
      [%instance =atom]
      [%option =(map atom key)]
      [%union =(map atom key)]
      [%junction flat=key deep=key]
      [%conjunction wide=key tall=key]
      [%misjunction one=key two=key]
  ==
::
+$  xbattery  (map term (pair what (map term key)))
::
+$  pattern
  $@  ?(%hoon %manx %json %nock %path %plum %skin %spec %tape %tour %type %vase)
  $%  [%gate sample=key product=key]
      [%gear sample=key context=key batt=xbattery]
      [%list item=key]
      [%tree item=key]
      [%unit item=key]
  ==
::
+$  data
  $@  ?(%noun %void)
  $%  [%atom =aura constant=(unit @)]
      [%cell head=key tail=key]
      [%core =garb xray=key batt=xbattery]
      [%face face=$@(term tune) xray=key]
      [%fork =(set key)]
      [%pntr xray=key]
  ==
::
+$  recipe
  $%  [%direct =term]
      [%synthetic =term =(list key)]
  ==
::
--
