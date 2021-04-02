#include "Dpmjet3.h"

//
// Declare several f77 functions/subroutines
//

#define dpmjet_init     F77_NAME(dpmjet_init,DPMJET_INIT)     
#define dpmjet_aa_event F77_NAME(dpmjet_aa_event,DPMJET_AA_EVENT)     

extern "C" void   type_of_call  dpmjet_init(int *particle, float* energy, int* seed);
extern "C" void   type_of_call  dpmjet_aa_event();

void DpmjetInit(int *particle, float* energy, int* seed) {dpmjet_init(particle,energy,seed);}
void DpmjetAAEvent() {dpmjet_aa_event();} 
