#! /usr/local/bin/tcsh -f
if ($#argv != 0) then
  set case = $argv[1];
else 
  set c = `pwd`; set case = `echo $c | sed -e 's/+/\//g'`;
endif
 echo "case = $case";
setenv STARFPE NO
echo $case | grep NODEBUG
if (! $?) then
  setenv NODEBUG yes
else
 if ($?NODEBUG) unsetenv NODEBUG   
endif
echo $case | grep x8664
if (! $?) then
  setenv USE_64BITS 1
else 
  setenv USE_64BITS 0
endif
switch ($case) 
  case "*482*" : 
    setup gcc482   
  breaksw
  case "*492*" : 
    setup gcc492 
  breaksw
  case "*520*" : 
    setup gcc520 
  breaksw
  case "*521*" : 
    setup gcc521 
  breaksw
  case "*620*" : 
    setup gcc620 
  breaksw
endsw

switch ($case) 
  case "*15j*" :   
    setenv STAR_LEVEL SL15j;       
  breaksw
  case "*SL15*":   
    setenv STAR_LEVEL DEV2/SL15;   
  breaksw
  case "*TFG16a*": 
    setenv STAR_LEVEL DEV2/TFG16a; 
  breaksw
  case "*TFG16b*": 
    setenv STAR_LEVEL DEV2/TFG16b; 
  breaksw
  case "*TFG16d*": 
    setenv STAR_LEVEL TFG16d; 
  breaksw
  case "*devC*":   
    setenv STAR_LEVEL DEV2/devC
    breaksw
  case "*eval*":   
    setenv STAR_LEVEL DEV2/eval
    breaksw
  case "*.DEV2*":  
    setenv STAR_LEVEL .DEV2
    breaksw
  default:
    breaksw
endsw
#if ($USE_64BITS == 1) then
#  setup  64b
#else
#  setup  32b
#endif
source $GROUP_DIR/.starver $STAR_LEVEL
setenv test "DEBUG"
if ($?NODEBUG) setenv test "NODEBUG"
echo "STAR = $STAR, $test";
which gcc
