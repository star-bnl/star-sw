/***************************************************************************
 *
 * $Id: StMiniDstMaker.h,v 1.2 2000/10/16 19:35:44 ullrich Exp $
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
 * Revision 1.2  2000/10/16 19:35:44  ullrich
 * Updated to run on Sun/CC5.
 *
 * Revision 1.1  2000/10/13 19:26:18  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StMiniDstMaker_h
#define StMiniDstMaker_h
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
