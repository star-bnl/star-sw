#ifndef STSPAWAFER_HH
#define STSPAWAFER_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StSpaListNoise.hh"
# include "StSpaListStrip.hh"
# include "StSpaNoise.hh"
# include "StSpaStrip.hh"
class StSpaWafer
{
 public:
                  StSpaWafer(int id);
                  ~StSpaWafer();

  StSpaListStrip* getStripP();
  StSpaListStrip* getStripN();
  void            addStrip(StSpaStrip *ptr, int iSide);
  void            addNoise(StSpaNoise *ptr, int iSide);
  void            setIsActive(int rIsActive, int iSide, int rNStrip);
  void            sortNoise();
  void            sortStrip();
  void            addNoiseToStripSignal(long NElectronInAMip,long A128Dynamic);
  void            pedestalSubstraction();
  void            zeroSubstraction();
  void            convertAnalogToDigit(long NElectronInAMip,long ADCDynamic,
				       long NBitEncoding, float DAQCutValue);
  void            updateListStrip();


private:
  int                      mId;
  StSpaListStrip          *mStripP;
  StSpaListStrip          *mStripN;
  StSpaListNoise          *mNoiseP;
  StSpaListNoise          *mNoiseN;


};  
#endif
