/***************************************************************************
 *
 * $Id: StRandyTopMapMaker.h,v 1.2 2000/04/03 20:28:10 rcwells Exp $
 *
 * Author: Randy Wells, Ohio State
 ***************************************************************************
 *
 * Description:  This is used to fix the topology map in StEvent
 *
 ***************************************************************************
 **************************************************************************/
#ifndef StRandyTopMapMaker_HH
#define StRandyTopMapMaker_HH
#include "StMaker.h"

class StEvent;
class StRun;

class StRandyTopMapMaker : public StMaker {
public:

    StRandyTopMapMaker(const Char_t *name="analysis");
    virtual ~StRandyTopMapMaker();
    
    virtual void Clear(Option_t *option="");
    virtual Int_t Init();
    virtual Int_t  Make();
    virtual Int_t  Finish();
    
    virtual const char *GetCVS() const
    {static const char cvs[]="$Id: StRandyTopMapMaker.h,v 1.2 2000/04/03 20:28:10 rcwells Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
private:
    Bool_t drawinit;
    Char_t collectionName[256];
        
    ClassDef(StRandyTopMapMaker,1)
};
#endif
