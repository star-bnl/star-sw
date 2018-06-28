#ifndef ST_PICO_THREE_VECTOR_D_H
#define ST_PICO_THREE_VECTOR_D_H

#include "StPicoThreeVector.h"

#if defined(_VANILLA_ROOT_)
typedef StPicoThreeVector<double> StThreeVectorD;
#else
typedef StPicoThreeVector<double> StPicoThreeVectorD;
#endif

#endif
