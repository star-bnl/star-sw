/***************************************************************************
 *
 * $Id: StMuDst2StEventMaker.h,v 1.1 2003/01/09 18:59:45 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StMuDst2StEventMaker_hh
#define StMuDst2StEventMaker_hh

#include "StMaker.h"
#include "StChain.h"

class StMuDstMaker;
class StEvent;

/**
   @class StMuDst2StEventMaker
   A small utility maker to create a StEvent and put in into the DataSet structure when running in the chain.
   Also, the time is read from the StEvent and put into the StEvtHddr structute for database usage.
*/
class StMuDst2StEventMaker : public StMaker {
 public:
    /// Default constructor; get pointer to StMuDstMaker
    StMuDst2StEventMaker(const char* self="muDst2StEventMaker", const char* muDstMakerName="muDstMaker");
    ~StMuDst2StEventMaker();
    
    int Init();   ///< do nothing
    void Clear(); ///< do nothing 
    int Make();   ///< create a StEvent from the muDst and put it into the .data tree structure. Also time stamp gets written and set in StEvtHddr for database usage
    int Finish(); ///< do nothing
    StEvent* event() { return  mStEvent; } ///< retunr pointer to StEvent, 0 if not created 
    virtual const char *GetCVS() const {
	static const char cvs[]="Tag $Name:  $ $Id: StMuDst2StEventMaker.h,v 1.1 2003/01/09 18:59:45 laue Exp $ built "__DATE__" "__TIME__ ; 
	return cvs;
    }
  

 private:
    StMuDstMaker* mMuDstMaker;
    StEvent* mStEvent;
    
    ClassDef(StMuDst2StEventMaker, 1)
}; 


#endif

/***************************************************************************
 *
 * $Log: StMuDst2StEventMaker.h,v $
 * Revision 1.1  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.15  2002/11/08 14:18:59  laue
 *
 **************************************************************************/
