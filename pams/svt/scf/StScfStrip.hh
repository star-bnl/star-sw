#ifndef STSCFSTRIP_HH
#define STSCFSTRIP_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>

class StScfStrip
{
 public:
  StScfStrip(int rNStrip, int rDigitSig, int rSigma);
  StScfStrip(int rNStrip, int rDigitSig, int rSigma, int *rMcHit);
  ~StScfStrip();
  void        setPrevStrip(StScfStrip *rPrevStrip);
  void        setNextStrip(StScfStrip *rNextStrip);
  void        setSigma(int rSigma);
  void        setNStrip(int rNStrip);
  void        setDigitSig(int rDigitSig);
  void        setIdMcHit(int rIdMcHit, int iR);
  int         getNStrip();
  int         getDigitSig();
  int         getSigma();
  int         getIdMcHit(int iR);
  StScfStrip* getPrevStrip();
  StScfStrip* getNextStrip();
  void        copyTo(StScfStrip *ptrClone);
  
private:
  int        mNStrip;
  int        mDigitSig;
  int        mSigma;
  int        *mIdMcHit;
  StScfStrip *mPrevStrip;
  StScfStrip *mNextStrip;
};

#endif
