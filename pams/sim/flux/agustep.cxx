#include "StarCallf77.h" 
#include "St_geant_Maker/St_geant_Maker.h"
#define    agustep	 F77_NAME(agustep,AGUSTEP)
extern "C" void type_of_call agustep() {
  St_geant_Maker::usflux();
};
