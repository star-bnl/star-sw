//! $Id: StRawTpcQaMaker.h,v 1.4 2000/06/20 19:46:05 kathy Exp $
//! $Log: StRawTpcQaMaker.h,v $
//! Revision 1.4  2000/06/20 19:46:05  kathy
//! added GetCVS
//!
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

//! static Char_t m_VersionCVS = "$Id: StRawTpcQaMaker.h,v 1.4 2000/06/20 19:46:05 kathy Exp $";

protected:

public:
  TH1F *mhist_1;
  TH2D *mSector[N__SECTORS]; //!

public: 
  StRawTpcQaMaker(const char *name="RawTpcQa");
  virtual       ~StRawTpcQaMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual Int_t  Clear();
  virtual void   PrintInfo();

  virtual const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StRawTpcQaMaker.h,v 1.4 2000/06/20 19:46:05 kathy Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 ClassDef(StRawTpcQaMaker, 1)   //StAF chain virtual base class for Makers
};

#endif




