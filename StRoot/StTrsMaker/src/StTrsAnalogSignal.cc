/*****************************************************************
 *
 * $Id: StTrsAnalogSignal.cc,v 1.2 1998/11/13 21:30:53 lasiuk Exp $
 *
 * Author: brian Nov 1, 1998
 *
 *****************************************************************
 * Description:  
 *
 *****************************************************************
 *
 * $Log: StTrsAnalogSignal.cc,v $
 * Revision 1.2  1998/11/13 21:30:53  lasiuk
 * << operator
 *
 * Revision 1.2  1998/11/13 21:30:53  lasiuk
 * << operator
 *
 * Revision 1.1  1998/11/10 17:12:23  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/01 13:47:05  lasiuk
 * Initial Revision
 *
 ******************************************************************/
#include <iostream.h>
#include "StTrsAnalogSignal.hh"

StTrsAnalogSignal::StTrsAnalogSignal(float t, float amp)
{
    mAnalogSignal.first  = t;
    mAnalogSignal.second = amp;
}

StTrsAnalogSignal::StTrsAnalogSignal()
{
    mAnalogSignal.first  = 0;
    mAnalogSignal.second = 0;
}

StTrsAnalogSignal::~StTrsAnalogSignal() { /* nopt */}


// Non-member Function for printing
//template<class T>
ostream& operator<<(ostream& os, StTrsAnalogSignal& sig)
{
    return os << '(' << sig.amplitude() << ", " << sig.time() << ')';
}

