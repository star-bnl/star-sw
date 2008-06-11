#ifndef __StGammaTower_h__
#define __StGammaTower_h__

#include <TObject.h>
#include <vector>
#include <TRefArray.h>
#include <TMath.h>

enum { kEEmcTower=0, kEEmcPre1, kEEmcPre2, kEEmcPost, kBEmcTower=10, kBEmcPres=11 };

class StGammaTower : public TObject
{
 public:
  StGammaTower();
  ~StGammaTower(){ /* nada */ }
  Int_t   id;	   // ID of the tower
  Float_t energy;  // scalar energy
  Float_t eta;     // eta (from event vertex to smd depth, or pre/post depth)
  Float_t phi;     // phi (from event vertex to smd depth, or pre/post depth)
  Int_t   stat;    // status bits (non fatal HW problems)
  Int_t   fail;    // fail bits (fatal HW problems)
  Int_t   layer;   // see below
  Float_t pt(){ return energy / TMath::CosH(eta); }

  Int_t sector();    // returns eemc sector [0,11]
  Int_t subsector(); // returns eemc subsector [0,4]
  Int_t etabin();    // returns eemc etabin [0,11] 
  Int_t phibin();

  TRefArray candidates; // referencing candidates
  ClassDef(StGammaTower,2);
};

typedef std::vector<StGammaTower> StGammaTowerVec_t;

// Definition of layers
//
//   0 - endcap tower
//   1 - endcap preshower 1
//   2 - endcap preshower 2
//   3 - endcap postshwoer

//  10 - barrel tower
//  11 - barrel preshower

#endif
