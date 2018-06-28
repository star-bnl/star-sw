#ifndef ST_PICO_THREE_VECTOR_F_H
#define ST_PICO_THREE_VECTOR_F_H

#include "StPicoThreeVector.h"

#if defined(_VANILLA_ROOT_)
typedef StPicoThreeVector<float> StThreeVectorF;
#else
typedef StPicoThreeVector<float> StPicoThreeVectorF;
#endif

#endif
