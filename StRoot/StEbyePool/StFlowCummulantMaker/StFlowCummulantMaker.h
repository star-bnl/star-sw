///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCummulantMaker.h,v 1.2 2003/09/02 17:57:58 perev Exp $
//
// Authors: Art Poskanzer and Raimond Snellings, LBNL, Aug 1999
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the FlowTags and/or StFlowEvent
//
///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCummulantMaker.h,v $
// Revision 1.2  2003/09/02 17:57:58  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.1  2001/01/31 19:48:22  snelling
// Cummulant calculation for the q-vector based on Ollitraults paper
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowCummulantMaker_H
#define StFlowCummulantMaker_H
#include <Stiostream.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TVector2.h"
#include "TString.h"
class StFlowEvent;
class StFlowSelection;
class TH1F;
class TH1D;
class TH2F;
class TH2D;
class TH3F;
class TProfile;
class TProfile2D;

class StFlowCummulantMaker : public StMaker {

public:

           StFlowCummulantMaker(const Char_t* name="FlowAnalysis");
           StFlowCummulantMaker(const Char_t* name,
			       const StFlowSelection& pFlowSelect);
  virtual  ~StFlowCummulantMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();
  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowCummulantMaker.h,v 1.2 2003/09/02 17:57:58 perev Exp $ built "__DATE__" "__TIME__ ;
    return cvs;}

private:

  void     FillFromFlowEvent();

  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object


  TString      MakerName;

  enum { count = 3 };

  Float_t xz[count][2*count]; //!
  Float_t yz[count][2*count]; //!
  Float_t rz[count]; //!
  Float_t g0[count][2*count]; //!
  Float_t cumul[count]; //!
  Int_t   nevents; //!
  Int_t   mTotalMultInt; //!
  Float_t zmax;
  Float_t r0;



  ClassDef(StFlowCummulantMaker, 1)              // macro for rootcint
};

#endif
