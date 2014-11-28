#ifndef STAR_St_sfs_Maker
#define STAR_St_sfs_Maker

//////////////////////////////////////////////////////////////////////////
// $Id: St_sfs_Maker.h,v 1.2 2014/08/06 11:43:56 jeromel Exp $
// $Log: St_sfs_Maker.h,v $
// Revision 1.2  2014/08/06 11:43:56  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2009/11/10 21:14:18  fisyak
// pams clean up
//
//                                                                      //
// St_sfs_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class St_sfs_Maker : public StMaker {
 private:
  Double_t mResXSvt, mResZSvt;
 public: 
  St_sfs_Maker(const char *name="svt_hits") : StMaker(name), mResXSvt(-1),  mResZSvt(-1){}
  virtual       ~St_sfs_Maker() {}
  virtual Int_t  InitRun(Int_t RunNo);
  virtual Int_t  Make();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_sfs_Maker.h,v 1.2 2014/08/06 11:43:56 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
   ClassDef(St_sfs_Maker,0)   // chain virtual base class for Makers
};

#endif


