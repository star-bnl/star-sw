// $Id: StSsdStrip.hh,v 1.6 2005/03/18 14:15:51 lmartin Exp $
//
// $Log: StSsdStrip.hh,v $
// Revision 1.6  2005/03/18 14:15:51  lmartin
// missing CVS header added
//

#ifndef STSSDSTRIP_HH
#define STSSDSTRIP_HH

#define SSD_MAXIDMCHIT 5

class StSsdStrip
{
 public:
  StSsdStrip(int rNStrip, int rDigitSig, float rSigma, int rPedestal);
  StSsdStrip(int rNStrip, int rDigitSig, float rSigma, int rPedestal, int *rMcHit);
  ~StSsdStrip();
  void        setPrevStrip(StSsdStrip *rPrevStrip);
  void        setNextStrip(StSsdStrip *rNextStrip);
  void        setSigma(float rSigma);
  void        setPedestal(int iPedestal);
  void        setNStrip(int rNStrip);
  void        setDigitSig(int rDigitSig);
  void        setIdMcHit(int rIdMcHit, int iR);
  int         getNStrip();
  int         getDigitSig();
  float       getSigma();
  int         getPedestal(); 
  int         getIdMcHit(int iR);
  StSsdStrip* getPrevStrip();
  StSsdStrip* getNextStrip();
  void        copyTo(StSsdStrip *ptrClone);
  
private:
  int        mNStrip;
  int        mDigitSig;
  float      mSigma;
  int        mPedestal;
  int        *mIdMcHit;
  StSsdStrip *mPrevStrip;
  StSsdStrip *mNextStrip;
};

#endif
