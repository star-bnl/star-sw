/***************************************************************************
 *
 * $Id: StMiniDstMaker.h,v 1.1 2000/10/13 19:26:18 ullrich Exp $
 *
 * Author: Thomas Ullrich, Oct 2000
 ***************************************************************************
 *
 * Description:  Example program to write miniDSTs based on StEvent.
 *               The miniDST will contain primary K+/K- tracks only.
 *
 ***************************************************************************
 *
 * $Log: StMiniDstMaker.h,v $
 * Revision 1.1  2000/10/13 19:26:18  ullrich
 * Initial Revision.
 *
 * Revision 1.1  2000/10/13 19:26:18  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StMiniDstMaker_h
#define StMiniDstMaker_h
#include <vector>
#include <string>
#include "StMaker.h"
#include "StThreeVectorD.hh"

class StEvent;
class StTrack;

class StMiniDstMaker : public StMaker {
public:

    StMiniDstMaker(const Char_t *name="StMiniDstMaker");
    Int_t  Init();
    Int_t  Make();
    Int_t Finish();
	
protected:
    bool accept(StEvent*);
    bool accept(StTrack*);
    
private:
    ClassDef(StMiniDstMaker,1)
};

#endif
