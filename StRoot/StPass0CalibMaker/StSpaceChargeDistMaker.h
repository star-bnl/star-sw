/*!
  \class StSpaceChargeDistMaker
  
  StSpaceChargeDistMaker looks at the distribution of charge
  in the TPC

*/

#ifndef STAR_StSpaceChargeDistMaker
#define STAR_StSpaceChargeDistMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TArrayI.h"

class StEvent;
class TH1D;
class TH2D;
class TH3D;

class StSpaceChargeDistMaker : public StMaker {
 
public: 
  StSpaceChargeDistMaker(const char *name="SCDist");
  virtual       ~StSpaceChargeDistMaker();
  virtual Int_t  InitRun(Int_t run);
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  
  void SetThrows(Int_t N) { throws = N; }

  void AcceptTrigger(Int_t trig); // negative value accepts all triggers

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpaceChargeDistMaker.h,v 1.1 2012/07/06 17:23:00 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  

protected:

  void   Pads();
  StEvent* event;
  TH3D* Space3ChargePRZ;
  TH3D* Space3ChargeU;
  TH2D* Rhist;
  TH2D* Phist;
  TH3D* RPhist;
  TH3D* RPPhist;
  TH2D* PHhist;
  TH2D* PPhist;
  TH1D* ZdcC;
  Float_t gainCorr[4096];
  TArrayI trigs;
  Int_t throws;
  Int_t run;

  ClassDef(StSpaceChargeDistMaker, 0)
};
    
#endif

//_____________________________________________________________________________
// $Id: StSpaceChargeDistMaker.h,v 1.1 2012/07/06 17:23:00 genevb Exp $
// $Log: StSpaceChargeDistMaker.h,v $
// Revision 1.1  2012/07/06 17:23:00  genevb
// Introduce StSpaceChargeDistMaker
//
//
