#ifndef STSPASTRIP_HH
#define STSPASTRIP_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>

class StSpaStrip
{
 public:
  StSpaStrip(int rNStrip, int rMcStrip, int rDigitSig, float rAnalogSig, int *rIdMcHit);
  StSpaStrip(int rNStrip, int rMcStrip, float rAnalogSig);
  StSpaStrip(int rNStrip, int rDigitSig);
  ~StSpaStrip();
  void        setNStrip(int rNStrip);
  void        setMcStrip(int rMcStrip);
  void        setDigitSig(int rDigitSig);
  void        setAnalogSig(float rAnalogSig);
  void        setIdMcHit(int rIdMcHit, int iR);
  void        setPrevStrip(StSpaStrip *rPrevStrip);
  void        setNextStrip(StSpaStrip *rNextStrip);
  int         getNStrip();
  int         getMcStrip();
  int         getDigitSig();
  float       getAnalogSig();
  int         getIdMcHit(int iR);
  StSpaStrip* getPrevStrip();
  StSpaStrip* getNextStrip();
  StSpaStrip* giveCopy();
  
private:
  int         mNStrip;
  int         mMcStrip;
  int         mDigitSig;
  float       mAnalogSig;
  int        *mIdMcHit;
  StSpaStrip *mPrevStrip;
  StSpaStrip *mNextStrip;
};

#endif
