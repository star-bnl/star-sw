#ifndef STSLSSTRIP_HH
#define STSLSSTRIP_HH

class StSlsStrip
{
 public:
  StSlsStrip(int rNStrip, int rIdHit, int rMcHit, int rMcTrack, float rAnalogSig);
  ~StSlsStrip();

  void        setNStrip(int rNStrip);
  void        setIdHit(int rIdHit, int iR);
  void        setIdMcHit(int rIdMcHit, int iR);
  void        setIdMcTrack(int rIdMcTrack, int iR);
  void        setDigitSig(int rDigitSig);
  void        setNHits(int rNHits);
  void        setAnalogSig(float rAnalogSig);
  void        setPrevStrip(StSlsStrip *rPrevStrip);
  void        setNextStrip(StSlsStrip *rNextStrip);
  int         getNStrip();
  int         getIdHit(int iR);
  int         getIdMcHit(int iR);
  int         getIdMcTrack(int iR);
  int         getDigitSig();
  int         getNHits();
  float       getAnalogSig();
  StSlsStrip* getPrevStrip();
  StSlsStrip* getNextStrip();
  void        copyTo(StSlsStrip *ptrClone);

 private:
  int         mNStrip;
  int        *mIdHit;
  int        *mIdMcHit;
  int        *mIdMcTrack;
  int         mDigitSig;
  int         mNHits;
  float       mAnalogSig;
  StSlsStrip *mPrevStrip;
  StSlsStrip *mNextStrip;

};
#endif
