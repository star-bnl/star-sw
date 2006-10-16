// $Id: StSceComp.hh,v 1.3 2006/10/16 19:54:45 fisyak Exp $
//
// $Log: StSceComp.hh,v $
// Revision 1.3  2006/10/16 19:54:45  fisyak
// St_DataSet => TDataSet
//
// Revision 1.2  2005/05/12 08:22:10  lmartin
// cvs tags added and histograms in the .hist branch
//

#ifndef STSCECOMP_HH
#define STSCECOMP_HH
#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "Rtypes.h"
class StSceComp
{
 public:
  StSceComp(int rComp, int rProb, int rGhostOrTrue, int rKindPackage, int rIdMatch, int rIdWaf, Double_t *rD2e , Double_t *rDxg, Double_t *rDxl);
  ~StSceComp();
  void       setNComp(int rComp);
  void       setProb(int rProb);
  void       setGhostOrTrue(int rGhostOrTrue);
  void       setKindPackage(int rKindPackage);
  void       setIdMatch(int rIdMatch);
  void       setIdWaf(int rIdWaf);
  void       setD2e(Double_t rD2e, int iR);
  void       setDxg(Double_t rDxg, int iR);
  void       setDxl(Double_t rDxl, int iR);

  void       setPrevComp(StSceComp *rPrevComp);
  void       setNextComp(StSceComp *rNextComp);

  int        getNComp();
  int        getProb();
  int        getGhostOrTrue();
  int        getKindPackage();
  int        getIdMatch();
  int        getIdWaf();
  Double_t      getD2e(int iR);
  Double_t      getDxg(int iR);
  Double_t      getDxl(int iR);

  StSceComp* getPrevComp();
  StSceComp* getNextComp();

  StSceComp* giveCopy();
  
 private:
  int        mComp;
  int        mProb;
  int        mGhostOrTrue;
  int        mKindPackage;
  int        mIdMatch;
  int        mIdWaf;
  Double_t     *mD2e;
  Double_t     *mDxg;
  Double_t     *mDxl;

  StSceComp *mPrevComp;
  StSceComp *mNextComp;
};
#endif
