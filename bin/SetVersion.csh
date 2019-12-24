#! /usr/local/bin/tcsh -f
if ($#argv != 0) then
  set case = $argv[1];
else 
  set c = `pwd`; set case = `echo $c | sed -e 's/+/\//g'`;
endif
 echo "case = $case";
setenv STARFPE NO
echo $case | egrep -i '(NODEBUG|opt)' 
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
  case "*gcc4*" : 
    setup gcc4   
  breaksw
  case "*gcc5*" : 
    setup gcc5 
  breaksw
  case "*gcc6*" : 
    setup gcc6 
  breaksw
  case "*gcc7*" : 
    setup gcc7 
  breaksw
  case "*gcc8*" : 
    setup gcc8 
  breaksw
  case "*icc*" : 
    setup icc
  breaksw
    default:
    setup gcc
endsw

switch ($case) 
  case "*SL18f*" :   
#   source $STAR_PATH/.DEV2/unsetupDEV2.csh
    setenv STAR_LEVEL SL18f;       
    breaksw 
 case "*SL18h*" :   
#   source $STAR_PATH/.DEV2/unsetupDEV2.csh
    setenv STAR_LEVEL SL18h;       
  breaksw
  case "*TFG17o*": 
    setenv STAR_LEVEL TFG17o; 
  breaksw
  case "*TFG17q*":   
    setenv STAR_LEVEL TFG17q;   
  breaksw
  case "*TFG17q*":   
    setenv STAR_LEVEL TFG17q;   
  breaksw
  case "*TFG17u*":   
    setenv STAR_LEVEL TFG17u;   
  breaksw
  case "*TFG16v*": 
    setenv STAR_LEVEL TFG16v; 
  breaksw
  case "*TFG16d*": 
    setenv STAR_LEVEL TFG16d; 
  breaksw
  case "*TFG16e*": 
    setenv STAR_LEVEL TFG16e; 
  breaksw
  case "*TFG16f*": 
    setenv STAR_LEVEL TFG16f; 
  breaksw
  case "*TFG19a*": 
    setenv STAR_LEVEL TFG19a; 
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
STAR_LEVELS

