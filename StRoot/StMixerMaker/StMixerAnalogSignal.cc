/*****************************************************************
 *
 * $Id: StMixerAnalogSignal.cc,v 1.1 2000/02/16 21:02:07 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsAnalogSignal.cc
 *
 *****************************************************************
 * Description:  
 *
 *****************************************************************
 *
 ******************************************************************/
#include <iostream.h>
#include "StMixerAnalogSignal.hh"

StMixerAnalogSignal::StMixerAnalogSignal(float t, float amp)
{
    mAnalogSignal.first  = t;
    mAnalogSignal.second = amp;
}

StMixerAnalogSignal::StMixerAnalogSignal()
{
    mAnalogSignal.first  = 0;
    mAnalogSignal.second = 0;
}

StMixerAnalogSignal::~StMixerAnalogSignal() { /* nopt */ }


// Non-member Function for printing
// template<class T>
ostream& operator<<(ostream& os, const StMixerAnalogSignal& sig)
{
    return os << '(' << sig.time() << ", " << sig.amplitude() << ')';
}

