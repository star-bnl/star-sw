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
#include <iostream.h>

class TH2F;
class St_svg_geom;

class StSvtdEdxMaker : public StMaker
{
 public: 
  StSvtdEdxMaker(const char *name = "SvtdEdx");
  StSvtdEdxMaker(StSvtdEdxMaker& analmaker);
  ~StSvtdEdxMaker();

  virtual Int_t Init();
  virtual Int_t Make();
  
  void FillHistograms(double dEdx, double p);

 private:

  St_svg_geom* mGeom; //!
  TH2F* mSvtdEdx; //!


  ClassDef(StSvtdEdxMaker,1)   //virtual base class for Makers

};


#endif
