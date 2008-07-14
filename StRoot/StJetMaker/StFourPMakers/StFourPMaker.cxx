// $Id: StFourPMaker.cxx,v 1.7 2008/07/14 19:59:54 tai Exp $
#include "StFourPMaker.h"
ClassImp(StFourPMaker)


void StFourPMaker::Clear(const Option_t* o)
{
    tracks.clear();
    return StMaker::Clear();
}
