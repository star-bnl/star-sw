#ifndef STSSDSTRIP_HH
#define STSSDSTRIP_HH

#define SSD_MAXIDMCHIT 5

class StSsdStrip
{
 public:
  StSsdStrip(int rNStrip, int rDigitSig, float rSigma);
  StSsdStrip(int rNStrip, int rDigitSig, float rSigma, int *rMcHit);
  ~StSsdStrip();
  void        setPrevStrip(StSsdStrip *rPrevStrip);
  void        setNextStrip(StSsdStrip *rNextStrip);
  void        setSigma(float rSigma);
  void        setNStrip(int rNStrip);
  void        setDigitSig(int rDigitSig);
  void        setIdMcHit(int rIdMcHit, int iR);
  int         getNStrip();
  int         getDigitSig();
  float         getSigma();
  int         getIdMcHit(int iR);
  StSsdStrip* getPrevStrip();
  StSsdStrip* getNextStrip();
  void        copyTo(StSsdStrip *ptrClone);
  
private:
  int        mNStrip;
  int        mDigitSig;
  float        mSigma;
  int        *mIdMcHit;
  StSsdStrip *mPrevStrip;
  StSsdStrip *mNextStrip;
};

#endif
