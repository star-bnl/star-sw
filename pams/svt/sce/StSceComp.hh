#ifndef STSCECOMP_HH
#define STSCECOMP_HH
#include <stdiostream.h>
#include <stdlib.h>
#include <math.h>

class StSceComp
{
 public:
  StSceComp(int rComp, int rProb, int rGhostOrTrue, int rKindPackage, int rIdMatch, int rIdWaf, float *rD2e , float *rDxg, float *rDxl);
  ~StSceComp();
  void       setNComp(int rComp);
  void       setProb(int rProb);
  void       setGhostOrTrue(int rGhostOrTrue);
  void       setKindPackage(int rKindPackage);
  void       setIdMatch(int rIdMatch);
  void       setIdWaf(int rIdWaf);
  void       setD2e(float rD2e, int iR);
  void       setDxg(float rDxg, int iR);
  void       setDxl(float rDxl, int iR);

  void       setPrevComp(StSceComp *rPrevComp);
  void       setNextComp(StSceComp *rNextComp);

  int        getNComp();
  int        getProb();
  int        getGhostOrTrue();
  int        getKindPackage();
  int        getIdMatch();
  int        getIdWaf();
  float      getD2e(int iR);
  float      getDxg(int iR);
  float      getDxl(int iR);

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
  float     *mD2e;
  float     *mDxg;
  float     *mDxl;

  StSceComp *mPrevComp;
  StSceComp *mNextComp;
};
#endif
