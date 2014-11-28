///////////////////////////////////////////////////////////////////////////////
//
// $Id: StPidAmpMaker.h,v 1.11 2014/08/06 11:43:32 jeromel Exp $
//
// Authors: Aihong Tang
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: produce hist. for PIDFitter, which builds PID tables.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StPidAmpMaker_H
#define StPidAmpMaker_H
#include <Stiostream.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StPidProbabilityConst.hh"
#include "TVector2.h"
#include "TString.h"
class StFlowEvent;
class TH1F;
class TH1D;
class TH2F;
class TH2D;
class TH3F;
class StFlowSelection;

class StPidAmpMaker : public StMaker {

public:

           StPidAmpMaker(const Char_t* name="PidAmp");
           StPidAmpMaker(const StPidAmpMaker &from){};
  virtual  ~StPidAmpMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  void     SetMultBinNumber(int val);
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StPidAmpMaker.h,v 1.11 2014/08/06 11:43:32 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;}

private:

  void     FillParticleHistograms();
  Int_t    GetPositionInArray(Int_t theMultBin, Int_t theDcaBin, Int_t theChargeBin, Int_t thePBin, Int_t theEtaBin, Int_t theNHitsBin);

  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object
  TH1F**           pidHisto;    //!
  int          mSingleMultiplicityBin; //always be 1. 
  int          theMultBin;
  
  TString      MakerName;

  ClassDef(StPidAmpMaker,0)              // macro for rootcint
};


inline void StPidAmpMaker::SetMultBinNumber(int val) {
  theMultBin=val;
}

#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Log: StPidAmpMaker.h,v $
// Revision 1.11  2014/08/06 11:43:32  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.10  2003/09/10 19:47:26  perev
// ansi corrs
//
// Revision 1.9  2003/09/02 17:58:47  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.8  2002/02/14 21:25:56  aihong
// re-install the new version
//

//
/////////////////////////////////////////////////////////////////////////////////
