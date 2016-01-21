#ifndef __D0Event__
#define __D0Event__

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Event                                                                //
//                                                                      //
// Description of the event and track parameters                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject.h"
#include "TClonesArray.h"
#include "TRefArray.h"
#include "TRef.h"
#include "TH1.h"
#include "TBits.h"
#include "TMath.h"
#include "KFParticle/KFParticle.h"

class KpiPair : public TObject {
 public: 
  KFParticle pair;
  Float_t    M, dM, S, dS, T, dT;
  ClassDef(KpiPair,1)
    
};
class D0Event : public TObject {

private:
   Int_t                fEvtNum;
   Int_t                fRun;
   Int_t                fNPairs;  //Number of pairs
   TClonesArray        *fPairs;   //->array with all pairs
   static TClonesArray *fgPairs;

public:
   D0Event();
   virtual      ~D0Event() {Clear();}
   void          Set(Int_t i, Int_t r) { Clear(); fEvtNum = i; fRun = r;}
   Int_t         GetEvtNum() const { return fEvtNum; }
   Int_t         GetRun() const { return fRun; }
   void          Clear(Option_t *option ="") {fgPairs->Clear("C"); fNPairs = 0;}
   static void   Reset(Option_t *option ="") {delete fgPairs; fgPairs = 0;}
   KpiPair      *AddPair();
   Int_t         GetNPairs() const { return fNPairs; }
   TClonesArray *GetPairs() const {return fPairs;}
   ClassDef(D0Event,1)  //Event structure
};

#endif
