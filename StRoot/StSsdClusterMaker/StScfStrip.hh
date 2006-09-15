// $Id: StScfStrip.hh,v 1.3 2006/09/15 21:04:49 bouchet Exp $
//
// $Log: StScfStrip.hh,v $
// Revision 1.3  2006/09/15 21:04:49  bouchet
// noise of the strips and clusters coded as a float ; read the noise from ssdStripCalib
//
// Revision 1.2  2005/05/17 14:16:34  lmartin
// CVS tags added
//
#ifndef STSCFSTRIP_HH
#define STSCFSTRIP_HH
#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>

class StScfStrip
{
 public:
  StScfStrip(int rNStrip, int rDigitSig, float rSigma);
  StScfStrip(int rNStrip, int rDigitSig, float rSigma, int *rMcHit);
  ~StScfStrip();
  void        setPrevStrip(StScfStrip *rPrevStrip);
  void        setNextStrip(StScfStrip *rNextStrip);
  void        setSigma(float rSigma);
  void        setNStrip(int rNStrip);
  void        setDigitSig(int rDigitSig);
  void        setIdMcHit(int rIdMcHit, int iR);
  int         getNStrip();
  int         getDigitSig();
  float       getSigma();
  int         getIdMcHit(int iR);
  StScfStrip* getPrevStrip();
  StScfStrip* getNextStrip();
  void        copyTo(StScfStrip *ptrClone);
  
private:
  int        mNStrip;
  int        mDigitSig;
  float      mSigma;
  int        *mIdMcHit;
  StScfStrip *mPrevStrip;
  StScfStrip *mNextStrip;
};

#endif
