/***************************************************************************
 *
 * $Id: StAddRunInfoMaker.h,v 1.1 2003/03/06 01:34:18 laue Exp $
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
    
    int Init();   ///< do nothing
    void Clear(); ///< do nothing 
    int Make();   ///< add StRunInfo to StEvent
    int Finish(); ///< do nothing
    virtual const char *GetCVS() const {
	static const char cvs[]="Tag $Name:  $ $Id: StAddRunInfoMaker.h,v 1.1 2003/03/06 01:34:18 laue Exp $ built "__DATE__" "__TIME__ ; 
	return cvs;
    }
  
 private:
    ClassDef(StAddRunInfoMaker, 1)
}; 


#endif

/***************************************************************************
 *
 * $Log: StAddRunInfoMaker.h,v $
 * Revision 1.1  2003/03/06 01:34:18  laue
 * StAddRunInfoMaker is a make helper maker to add the StRunInfo for the
 * only year1 Au+Au 130GeV data
 *
 *
 **************************************************************************/
