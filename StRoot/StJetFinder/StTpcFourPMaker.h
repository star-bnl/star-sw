/***************************************************************************
 *
 * $Id: StTpcFourPMaker.h,v 1.1 2003/04/04 21:36:06 thenry Exp $
 * $Log: StTpcFourPMaker.h,v $
 * Revision 1.1  2003/04/04 21:36:06  thenry
 * Creates Four Vector list from the TPC tracks for use with the StJetMaker
 *
 * Revision 1.0  2003/02/27 21:38:10  thenry
 * Created by Thomas Henry
 *
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a list of Four Momentums from the TPC
 * corresponding to charged particles.
 *
 ***************************************************************************/
#ifndef StTpcFourPMaker_h
#define StTpcFourPMaker_h
#include "StFourPMaker.h"

class StTpcFourPMaker : public StFourPMaker {
public:
    StTpcFourPMaker(const char* name, StMuDstMaker *pevent);
    virtual Int_t Make();

protected:
    ClassDef(StTpcFourPMaker,1)
};
#endif
