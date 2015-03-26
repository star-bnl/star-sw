#ifndef StPicoEmcPidTraits_hh
#define StPicoEmcPidTraits_hh

#include "TObject.h"

class StPicoEmcPidTraits: public TObject {
 public:
  StPicoEmcPidTraits();
  StPicoEmcPidTraits(Int_t index, Int_t id, Int_t adc0, Float_t* e, Float_t* dist, Int_t* nhit, Int_t* ntow);
  ~StPicoEmcPidTraits();
  virtual void Print(const Char_t *option = "") const;

  Int_t   bemcId() const         { return (Int_t)mBEMCId;                 }
  Int_t   adc0() const           { return (Int_t)mBTOWADC0;               }
  Float_t e0() const             { return (Float_t)mBTOWE0/1000.;         }
  Float_t e() const              { return (Float_t)mBTOWE/1000.;          }
  Float_t zDist() const          { return (Float_t)mBEMCDistZ/100.;       }
  Float_t phiDist() const        { return (Float_t)mBEMCDistPhi/10000.;   }
  Int_t   nEta() const           { return (Int_t)mBSMDNEta;               }
  Int_t   nPhi() const           { return (Int_t)mBSMDNPhi;               }  

  Int_t   btowId() const         { return (Int_t)mBTOWId;                 }
  Int_t   btowId2() const        { return (Int_t)mBTOWId23/10;            }
  Int_t   btowId3() const        { return (Int_t)mBTOWId23%10;            }
  Float_t e1() const             { return (Float_t)mBTOWE1/1000.;         }
  Float_t e2() const             { return (Float_t)mBTOWE2/1000.;         }
  Float_t e3() const             { return (Float_t)mBTOWE3/1000.;         }
  Float_t etaTowDist() const     { return (Float_t)mBTOWDistEta/10000.;   }
  Float_t phiTowDist() const     { return (Float_t)mBTOWDistPhi/10000.;   }


 private:
  Short_t  mTrackIndex;       // Index to the associated track in the event

  // these variables are extracted from the standard BEMC cluster algorithm
  Short_t  mBEMCId;           // index in bemcPoint array
  Short_t  mBTOWADC0;         // adc0 higest adc in the cluster
  Short_t  mBTOWE0;           // E0*1000 highest tower in the cluster
  Short_t  mBTOWE;            // EMC point E*1000 
  Short_t  mBEMCDistZ;        // z*100
  Short_t  mBEMCDistPhi;      // phi*10000
  UChar_t  mBSMDNEta;         // # of hits in eta
  UChar_t  mBSMDNPhi;         // # of hits in phi

  // these variables are purely from single tower or nearby towers  
  Short_t  mBTOWId;           // projected tower Id 1-4800
  Char_t   mBTOWId23;         // emc 2nd and 3rd closest tower local id  ( 2nd X 10 + 3rd), each id 0-8
  Short_t  mBTOWE1;           // E1*1000 matched (closest) tower E
  Short_t  mBTOWE2;           // E2*1000 2nd closest tower E
  Short_t  mBTOWE3;           // E3*1000 3rd closest tower E
  Short_t  mBTOWDistEta;      // eta*10000 distance between track and matched tower center
  Short_t  mBTOWDistPhi;      // phi*10000 distance between track and matched tower center

  ClassDef(StPicoEmcPidTraits,1);
};

#endif
