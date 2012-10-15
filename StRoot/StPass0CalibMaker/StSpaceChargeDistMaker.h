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
class StTpcHit;

class StSpaceChargeDistMaker : public StMaker {
 
public: 
  StSpaceChargeDistMaker(const char *name="SCDist");
  virtual       ~StSpaceChargeDistMaker();
  virtual Int_t  InitRun(Int_t run);
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  
  void SetThrows(Float_t N) { throws = N; }

  void AcceptTrigger(Int_t trig); // negative value accepts all triggers

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpaceChargeDistMaker.h,v 1.2 2012/10/15 17:51:12 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  

protected:

  void   GeomInit();
  void   GeomFill(StTpcHit* hit);
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
  Float_t throws;
  Int_t run;

  Float_t Xpads[128];
  UShort_t Npads[128];
  Int_t NP;
  Int_t NR;
  Int_t NS;
  Float_t XMIN[128];
  Float_t YMIN[32768]; // 128*256
  Bool_t PLIVE[1048576]; // 128*256*32

  ClassDef(StSpaceChargeDistMaker, 0)
};
    
#endif

//_____________________________________________________________________________
// $Id: StSpaceChargeDistMaker.h,v 1.2 2012/10/15 17:51:12 genevb Exp $
// $Log: StSpaceChargeDistMaker.h,v $
// Revision 1.2  2012/10/15 17:51:12  genevb
// Include distortion corrections, which must be evenly sampled per event (per hit)
//
// Revision 1.1  2012/07/06 17:23:00  genevb
// Introduce StSpaceChargeDistMaker
//
//
