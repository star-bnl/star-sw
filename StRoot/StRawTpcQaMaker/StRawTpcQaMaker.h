//! $Id: StRawTpcQaMaker.h,v 1.3 2000/06/19 19:01:22 kathy Exp $
//! $Log: StRawTpcQaMaker.h,v $
//! Revision 1.3  2000/06/19 19:01:22  kathy
//! put in Sergei's new versions of the code
//!

#ifndef STAR_StRawTpcQaMaker
#define STAR_StRawTpcQaMaker

//////////////////////////////////////////////////////////////////////////
// StRawTpcQaMaker.h                                                    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "TH1.h"
#include "TH2.h"
#include "hardWired.h"

class StRawTpcQaMaker : public StMaker {
private:


protected:

public:
  TH1F *mhist_1;
  TH2D* mSector[N__SECTORS]; //!

public: 
  StRawTpcQaMaker(const char *name="RawTpcQa");
  virtual       ~StRawTpcQaMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual Int_t  Clear();
  virtual void   PrintInfo();
 ClassDef(StRawTpcQaMaker, 1)   //StAF chain virtual base class for Makers
};

#endif




