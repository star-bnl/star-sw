/***************************************************************************
 *
 * $Id: StHWJetMaker.h,v 1.2 2003/09/10 19:47:20 perev Exp $
 * $Log: StHWJetMaker.h,v $
 * Revision 1.2  2003/09/10 19:47:20  perev
 * ansi corrs
 *
 * Revision 1.1  2003/04/24 14:15:16  thenry
 * These changes are really the first working version of the StFourPMakers
 * and teh StJetMakers.  This is all c++ stl implementation, and by virtue of
 * that fact, StEmcTpcFourPMaker bady needs to be speed optimized.
 *
 * Revision 1.0  2003/04/23 thenry
 * StHWJetMaker was created to do a simple jet analysis during fast offline
 *
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Finds jets for fast offline analysis
 *
 ***************************************************************************/
#ifndef StHWJetMaker_h
#define StHWJetMaker_h
#include "StJetMaker.h"
#include "StTpcFourPMaker.h"

class StHWJetMaker : public StJetMaker {
private:
    void* dontGoOutaScope;
public:

    StHWJetMaker(const Char_t *name);

    StTpcFourPMaker* fourPMaker;
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();     

    ClassDef(StHWJetMaker,0)
};
#endif


