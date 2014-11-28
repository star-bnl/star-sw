// $Id: StLaserAvClusterMaker.h,v 1.2 2013/09/28 13:01:53 fisyak Exp $

#ifndef STAR_StLaserAvClusterMaker
#define STAR_StLaserAvClusterMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TFile.h"
class StTpcHit;
class StTpcPadCoordinate;
class StLaserAvClusterMaker : public StMaker {
 public: 
  StLaserAvClusterMaker(const char *name="LaserAvEvent") : StMaker(name), fFileName(), fId(0), fToken(0) {}
  virtual       ~StLaserAvClusterMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  void SetInputFile(const Char_t *fileName="./laserHitsPad2003FF.root") {fFileName = fileName;}
  void SetToken(Int_t token) {fToken = token;}
 private:
  const Char_t *fFileName;
  Int_t fId;			 
  Int_t fToken;
  StTpcHit *CreateTpcHit(StTpcPadCoordinate *padcoord=0, Double_t sigmaY = 0, Double_t sigmaZ = 0);
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StLaserAvClusterMaker.h,v 1.2 2013/09/28 13:01:53 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StLaserAvClusterMaker,0)   //StAF chain virtual base class for Makers
};
#endif
// $Log: StLaserAvClusterMaker.h,v $
// Revision 1.2  2013/09/28 13:01:53  fisyak
// Freeze
//
// Revision 1.1.1.1  2012/08/27 22:59:19  fisyak
// Rename StLaserAvEvent => StLaserAvClusterMaker
//
// Revision 1.1  2012/06/11 14:24:24  fisyak
// The first version based on TSpectrum2
//
// Revision 1.1.1.1  2011/10/13 15:02:39  hejdar
// Hejdar makers
//
