//! $Id: StHistMaker.h,v 1.1 2000/06/23 21:16:18 kathy Exp $
//! $Log: StHistMaker.h,v $
//! Revision 1.1  2000/06/23 21:16:18  kathy
//! code that will collect all histograms that were added together in StHistUtil::AddHists - has to be in a maker in order to write it out
//!
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHistMaker
#define STAR_StHistMaker

#include "StHistUtil.h"
#include "StMaker.h"
#include "TH1.h"

//////////////////////////////////////////////////////////////////////////

class StHistMaker : public StMaker {

 private:

  TH1** mHArray; //!


 public: 

//! static Char_t m_VersionCVS = "$Id: StHistMaker.h,v 1.1 2000/06/23 21:16:18 kathy Exp $";

  StHistMaker(const char *name="Summedhist", const char *title="QAhist");
  virtual       ~StHistMaker();
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();

  virtual void SetHArray(TH1**);

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHistMaker.h,v 1.1 2000/06/23 21:16:18 kathy Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StHistMaker, 1)   //StAF chain virtual base class for Makers
    };
    
inline void StHistMaker::SetHArray(TH1** val)
                           {mHArray = val; }


#endif




