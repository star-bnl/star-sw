/*****************************************************************
 *
 * $Id: StTrsAnalogSignal.cc,v 1.5 2003/12/24 13:44:52 fisyak Exp $
 *
 * Author: brian Nov 1, 1998
 *
 *****************************************************************
 * Description:  
 *
 *****************************************************************
 *
 * $Log: StTrsAnalogSignal.cc,v $
 * Revision 1.5  2003/12/24 13:44:52  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.4  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  1999/01/15 11:02:57  lasiuk
 * modify << operator for STL use
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
#include <Stiostream.h>
#include "StTrsAnalogSignal.hh"

StTrsAnalogSignal::StTrsAnalogSignal(float t, float amp, int id) 
{
    mId                  = id;
    mAnalogSignal.first  = t;
    mAnalogSignal.second = amp;
}

StTrsAnalogSignal::StTrsAnalogSignal()
{
    mId                  = 0;
    mAnalogSignal.first  = 0;
    mAnalogSignal.second = 0;
}

StTrsAnalogSignal::~StTrsAnalogSignal() { /* nopt */}


// Non-member Function for printing
//template<class T>
ostream& operator<<(ostream& os, const StTrsAnalogSignal& sig)
{
    return os << '(' << sig.time() << ", " << sig.amplitude() << ')';
}

