/***************************************************************************
 *
 * $Id: StAnalysisMaker.h,v 2.0 1999/11/04 16:10:05 ullrich Exp $
 *
 * Author: Torre Wenaus, BNL,
 *         Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:  This is an example of a maker to perform analysis
 *               using StEvent.
 *               Use this as a template and customize it for your
 *               studies.
 *
 ***************************************************************************
 *
 * $Log: StAnalysisMaker.h,v $
 * Revision 2.0  1999/11/04 16:10:05  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/
#ifndef StAnalysisMaker_HH
#define StAnalysisMaker_HH
#include "StMaker.h"
#include "HighPtTag.h"

class StEvent;
class StRun;

class StAnalysisMaker : public StMaker {
public:

    StAnalysisMaker(const Char_t *name="analysis");
    virtual ~StAnalysisMaker();
    
    virtual void Clear(Option_t *option="");
    virtual Int_t Init();
    virtual Int_t  Make();
    virtual Int_t  Finish();
    
    HighPtTag_st* tag() {return theTag;} // Tag accessor
    
    virtual const char *GetCVS() const
    {static const char cvs[]="$Id: StAnalysisMaker.h,v 2.0 1999/11/04 16:10:05 ullrich Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
private:
    Bool_t drawinit;
    Char_t collectionName[256];
    
    // Maker generates a tag
    HighPtTag_st* theTag; //!
    Int_t nevents;
    
    ClassDef(StAnalysisMaker,1)
};
#endif
