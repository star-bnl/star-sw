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
  
  void SetThrows(Float_t N) { throws = N; }

  void AcceptTrigger(Int_t trig); // negative value accepts all triggers

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpaceChargeDistMaker.h,v 1.5 2014/08/06 11:43:32 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  

protected:

  void   GeomInit();
  void   GeomFill(Float_t z);
  StEvent* event;
  TH3D* Space3ChargePRZ;
  TH3D* Space3ChargeU;
  TH2D* thrownR;
  TH2D* acceptedR;
  TH3D* thrownRP;
  TH3D* acceptedRP;
  TH2D* thrownP;
  TH2D* acceptedP;
  TH1D* ZdcC;
  TArrayI trigs;
  Float_t throws;
  Int_t run;

  Float_t GGZ;
  Float_t Xpads[128];
  UShort_t Npads[128];
  Int_t NP;
  Int_t NR;
  Int_t NS;
  Float_t XMIN[128];
  Float_t XWID[128];
  Float_t YMIN[32768]; // 128*256
  Bool_t LiveRow[4096]; // 128*32
  Bool_t LivePad[1048576]; // 128*256*32

  ClassDef(StSpaceChargeDistMaker, 0)
};
    
#endif

//_____________________________________________________________________________
// $Id: StSpaceChargeDistMaker.h,v 1.5 2014/08/06 11:43:32 jeromel Exp $
// $Log: StSpaceChargeDistMaker.h,v $
// Revision 1.5  2014/08/06 11:43:32  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.4  2012/11/28 02:08:52  genevb
// Remove de-smearing bias in z and treat z more differentially
//
// Revision 1.3  2012/11/13 22:05:19  genevb
// Use TPC dE/dx correction code, and introduce de-smearing
//
// Revision 1.2  2012/10/15 17:51:12  genevb
// Include distortion corrections, which must be evenly sampled per event (per hit)
//
// Revision 1.1  2012/07/06 17:23:00  genevb
// Introduce StSpaceChargeDistMaker
//
//
