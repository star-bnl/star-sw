// $Id: StSpaNoise.hh,v 1.2 2005/05/13 08:39:33 lmartin Exp $
//
// $Log: StSpaNoise.hh,v $
// Revision 1.2  2005/05/13 08:39:33  lmartin
// CVS tags added
//

#ifndef STSPANOISE_HH
#define STSPANOISE_HH
# include <Stiostream.h>
# include <stdlib.h>
# include <math.h>

class StSpaNoise
{
 public:
  StSpaNoise(int rNStrip, int rPedestal, int rSigma);
  ~StSpaNoise();
  void        setNStrip(int rNStrip);
  void        setPedestal(int rPedestal);
  void        setSigma(int rSigma);
  void        setNoiseValue(int rNoiseValue);
  void        setIsActive(int rIsActive);
  void        setPrevNoise(StSpaNoise *rPrevNoise);
  void        setNextNoise(StSpaNoise *rNextNoise);
  int         getNStrip();
  int         getPedestal();
  int         getSigma();
  int         getNoiseValue();
  int         getIsActive();
  StSpaNoise* getPrevNoise();
  StSpaNoise* getNextNoise();
  StSpaNoise* giveCopy();

private:
  int         mNStrip;
  int         mPedestal;
  int         mSigma;
  int         mNoiseValue;
  int         mIsActive;
  StSpaNoise *mPrevNoise;
  StSpaNoise *mNextNoise;

};
#endif
