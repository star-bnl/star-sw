//
#ifndef STAR_StSvtdEdx
#define STAR_StSvtdEdx
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtSeqAdj base class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#define MAXPOINTS 20
#include <Stiostream.h>

class TH2F;
class StSvtGeometry;

class StSvtdEdxMaker : public StMaker
{
 public: 
  StSvtdEdxMaker(const char *name = "SvtdEdx");
  StSvtdEdxMaker(StSvtdEdxMaker& analmaker);
  ~StSvtdEdxMaker();

  virtual Int_t Init();
  virtual Int_t InitRun(Int_t RunNumber);
  virtual Int_t Make();
 
  void FillHistograms(double dEdx, double p);
  Int_t GetSvtGeometry();

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StSvtdEdxMaker.h,v 1.6 2004/11/11 20:21:32 jeromel Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

 private:

  StSvtGeometry* mGeom; //!
  TH2F* mSvtdEdx; //!

  ClassDef(StSvtdEdxMaker,0)   //virtual base class for Makers

};


#endif
