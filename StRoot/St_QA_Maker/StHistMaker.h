/*!
  \class StHistMaker
                                                                     
  Code that will collect all histograms that were added together in
  StHistUtil::AddHists - has to be in a maker in order to write it out
 
*/


#ifndef STAR_StHistMaker
#define STAR_StHistMaker

#include "StMaker.h"

class TH1;

class StHistMaker : public StMaker {

 private:
  TH1** mHArray; //!
  int   mHArraySize;

 public: 
  StHistMaker(const char *name="QA", const char *title="SummedQAHist");
  virtual       ~StHistMaker() {}
  virtual Int_t  Init();
  virtual Int_t  Finish();
  virtual Int_t  Make();

  void SetHArray(TH1** arrayPtr) {mHArray = arrayPtr;}
  void SetHArraySize(int size) {mHArraySize = size;}

// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHistMaker.h,v 2.4 2014/08/06 11:43:53 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StHistMaker,0)
};

#endif

///////////////////////////////////////////////////////////////////////////
//! $Id: StHistMaker.h,v 2.4 2014/08/06 11:43:53 jeromel Exp $
//! $Log: StHistMaker.h,v $
//! Revision 2.4  2014/08/06 11:43:53  jeromel
//! Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//!
//! Revision 2.3  2003/09/19 22:58:11  genevb
//! Initialize pointers to zero, some doxygenization
//!
//! Revision 2.2  2003/09/10 19:47:43  perev
//! ansi corrs
//!
//! Revision 2.1  2002/09/06 02:51:34  genevb
//! Remove limit on maximum number of histograms that can be copied
//!
//! Revision 2.0  2000/08/25 16:02:40  genevb
//! New revision: new structure, multiplicity classes
//!
//! Revision 1.2  2000/06/29 04:46:01  lansdell
//! removed virtual from inline methods
//!
//! Revision 1.1  2000/06/23 21:16:18  kathy
//! code that will collect all histograms that were added together in
//! StHistUtil::AddHists - has to be in a maker in order to write it out


