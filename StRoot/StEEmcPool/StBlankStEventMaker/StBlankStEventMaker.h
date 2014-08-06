/***************************************************************************
 *
 * $Id: StBlankStEventMaker.h,v 1.2 2014/08/06 11:42:58 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StBlankStEventMaker_hh
#define StBlankStEventMaker_hh

#include "StMaker.h"
#include "StChain.h"

class StMuDstMaker;
class StEvent;

/**
   @class StBlankStEventMaker
   A small utility maker to create a StEvent and put in into the DataSet structure when running in the chain.
   Also, the time is read from the StEvent and put into the StEvtHddr structute for database usage.
*/
class StBlankStEventMaker : public StMaker {
 public:
    /// Default constructor; get pointer to StMuDstMaker
    StBlankStEventMaker(const char* self="blank");
    ~StBlankStEventMaker();
    
    void Clear(const char*);  
    int Make();   ///< create a StEvent from the muDst and put it into the .data tree structure. Also time stamp gets written and set in StEvtHddr for database usage
    StEvent* event() { return  mStEvent; } ///< return pointer to StEvent, 0 if not created 
    virtual const char *GetCVS() const {
	static const char cvs[]="Tag $Name:  $ $Id: StBlankStEventMaker.h,v 1.2 2014/08/06 11:42:58 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
	return cvs;
    }
  

 private:
    StEvent* mStEvent;
    
    ClassDef(StBlankStEventMaker, 1)
}; 


#endif

/***************************************************************************
 *
 * $Log: StBlankStEventMaker.h,v $
 * Revision 1.2  2014/08/06 11:42:58  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2004/06/06 18:28:13  balewski
 * trick to convert .fzd --> muDst
 *
 * Revision 1.5  2003/09/17 02:54:37  jeromel
 * Name clash. Added warning in case this happens in future
 *
 * Revision 1.4  2003/09/12 21:32:17  jeromel
 * No changes (misspelled)
 *
 * Revision 1.3  2003/09/07 03:49:03  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.2  2003/08/04 14:38:10  laue
 * Alex Suaide's updated for the EMC. Now EEMC is included.
 *
 * Revision 1.1  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.15  2002/11/08 14:18:59  laue
 *
 **************************************************************************/
