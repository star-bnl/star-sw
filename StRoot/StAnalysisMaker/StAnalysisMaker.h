// $Id: StAnalysisMaker.h,v 1.8 1999/08/09 19:38:32 kathy Exp $
//
// $Log: StAnalysisMaker.h,v $
// Revision 1.8  1999/08/09 19:38:32  kathy
// checkin Curtis' changes that print out the event # with each set of QAInfo stuff
//
// Revision 1.7  1999/08/06 20:21:52  kathy
// back to old version that didn't write out QA info file, but now added QAInfo tag in front of information that QA team wants in summarizeEvent.cc - will also add a few more lines of output to summarizeEvent.cc soon
//
// Revision 1.6  1999/07/30 22:56:02  kathy
// added new method and input param qaflag so that if turned on, a log file will be printed out with QA information
//
// Revision 1.5  1999/07/15 13:56:41  perev
// cleanup
//
// Revision 1.4  1999/06/25 19:20:40  fisyak
// Merge StRootEvent and StEvent
//
// Revision 1.3  1999/06/24 21:56:47  wenaus
// Version minimally changed from standard StAnalysisMaker
//
// Revision 1.3  1999/03/30 15:33:43  wenaus
// eliminate obsolete branch methods
//
// Revision 1.2  1999/02/12 02:00:27  wenaus
// Incorporate tag loading example
//
// Revision 1.1  1999/02/05 17:54:56  wenaus
// initial commit
//

 * Revision for new StEvent
 *

///////////////////////////////////////////////////////////////////////////////
//
// StAnalysisMaker
//
// Description: 
//  Sample maker to access and analyze StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Torre Wenaus, BNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
 **************************************************************************/
#include "tables/HighPtTag.h"
#define StAnalysisMaker_HH
#include "StMaker.h"
#include "HighPtTag.h"

class StEvent;

    virtual const char *GetCVS() const
  Bool_t drawinit;
  Char_t collectionName[256];

  // Maker generates a tag
  HighPtTag_st* theTag; //!
  Int_t nevents;

protected:

public:

  StAnalysisMaker(const Char_t *name="analysis");

  virtual ~StAnalysisMaker();
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();

  // Tag accessor
  HighPtTag_st* tag() {return theTag;};

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StAnalysisMaker.h,v 1.8 1999/08/09 19:38:32 kathy Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StAnalysisMaker, 1)
    Int_t nevents;

    
    ClassDef(StAnalysisMaker,1)
};
#endif
