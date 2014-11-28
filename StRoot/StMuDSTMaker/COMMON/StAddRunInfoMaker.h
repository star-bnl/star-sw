/***************************************************************************
 *
 * $Id: StAddRunInfoMaker.h,v 1.4 2014/08/06 11:43:31 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StAddRunInfoMaker_hh
#define StAddRunInfoMaker_hh

#include "StMaker.h"
#include "StChain.h"

class StEvent;

/**
   @class StAddRunInfoMaker
   A small utility maker to add a StRunInfo for the year1 130GeV data.
   At that time we didn't have a StRunInfo
*/
class StAddRunInfoMaker : public StMaker {
 public:
    /// Default constructor; get pointer to StMuDstMaker
    StAddRunInfoMaker(const char* name="addRunInfo");
    ~StAddRunInfoMaker();
    
    int Make();   ///< add StRunInfo to StEvent
    virtual const char *GetCVS() const {
	static const char cvs[]="Tag $Name:  $ $Id: StAddRunInfoMaker.h,v 1.4 2014/08/06 11:43:31 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
	return cvs;
    }
  
 protected:
    ClassDef(StAddRunInfoMaker, 1)
}; 


#endif

/***************************************************************************
 *
 * $Log: StAddRunInfoMaker.h,v $
 * Revision 1.4  2014/08/06 11:43:31  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.3  2004/05/02 04:10:13  perev
 * private => protected
 *
 * Revision 1.2  2003/09/07 03:49:03  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.1  2003/03/06 01:34:18  laue
 * StAddRunInfoMaker is a make helper maker to add the StRunInfo for the
 * only year1 Au+Au 130GeV data
 *
 *
 **************************************************************************/
