#ifndef STSSDSTRIP_HH
#define STSSDSTRIP_HH
#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>

class StSsdStrip
{
 public:
  StSsdStrip(int rNStrip, int rDigitSig, int rSigma);
  StSsdStrip(int rNStrip, int rDigitSig, int rSigma, int *rMcHit);
  ~StSsdStrip();
  void        setPrevStrip(StSsdStrip *rPrevStrip);
  void        setNextStrip(StSsdStrip *rNextStrip);
  void        setSigma(int rSigma);
  void        setNStrip(int rNStrip);
  void        setDigitSig(int rDigitSig);
  void        setIdMcHit(int rIdMcHit, int iR);
  int         getNStrip();
  int         getDigitSig();
  int         getSigma();
  int         getIdMcHit(int iR);
  StSsdStrip* getPrevStrip();
  StSsdStrip* getNextStrip();
  void        copyTo(StSsdStrip *ptrClone);
  
private:
  int        mNStrip;
  int        mDigitSig;
  int        mSigma;
  int        *mIdMcHit;
  StSsdStrip *mPrevStrip;
  StSsdStrip *mNextStrip;
};

#endif
