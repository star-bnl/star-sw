// $Id: StFemtoDstMaker.h,v 1.16 2014/08/06 11:43:53 jeromel Exp $

#ifndef STAR_StFemtoDstMaker
#define STAR_StFemtoDstMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TTree.h"
#include "TFile.h"
class StPicoDst;
class StKFParticleInterface;
class StFemtoDstMaker : public StMaker {
 private:
  StPicoDst *fFemtoDst;
  TTree     *fFemtoTree;
  StKFParticleInterface *fStKFParticleInterface;
  TFile     *fOutFile;
 public: 
  StFemtoDstMaker(const char *name="FemtoDst"): StMaker(name), fFemtoDst(0), fFemtoTree(0), fStKFParticleInterface(0), fOutFile(0) {}
    virtual       ~StFemtoDstMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id:  $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  
  ClassDef(StFemtoDstMaker,0)
};
#endif
// $Log: StFemtoDstMaker.h,v $

