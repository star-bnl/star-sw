// $Id: StAnalysisMaker.h,v 1.3 1999/03/30 15:33:43 wenaus Exp $
//
// $Log: StAnalysisMaker.h,v $
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

protected:

public:

  StAnalysisMaker(const Char_t *name="analysis", const Char_t *title="analysis");
  virtual ~StAnalysisMaker();
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   PrintInfo();
  virtual Int_t  Finish();

  // Tag accessor
  HighPtTag_st* tag() {return theTag;};

  ClassDef(StAnalysisMaker, 1)
    Int_t nevents;

    
    ClassDef(StAnalysisMaker,1)
};
#endif
