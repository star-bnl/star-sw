// $Id: StAnalysisMaker.h,v 1.1 1999/02/05 17:54:56 wenaus Exp $
//
// $Log: StAnalysisMaker.h,v $
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
// suppress 'bool' definition in ObjectSpace stuff
//#define OS_OMIT_BOOL
#define StAnalysisMaker_HH
#include "StMaker.h"
#include "StMaker.h"
#include "HighPtTag.h"

class StEvent;

    virtual const char *GetCVS() const
  Bool_t drawinit;
  Char_t collectionName[256];

protected:

public:

  StAnalysisMaker(const Char_t *name="analysis", const Char_t *title="analysis");
  virtual ~StAnalysisMaker();
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual void   MakeBranch();
  virtual void   PrintInfo();
  virtual void   SetBranch();
  virtual Int_t  Finish();

  ClassDef(StAnalysisMaker, 1)
    Int_t nevents;

    
    ClassDef(StAnalysisMaker,1)
};
#endif
