#include "StEmcMath.h"
#include "StEvent/StMeasuredPoint.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StEmcUtil/emcInternalDef.h"

ClassImp(StEmcMath)

Bool_t 
StEmcMath::etaPhi(StMeasuredPoint* point,StMeasuredPoint* vertex, Double_t &eta, Double_t &phi)
{ 
  //
  // point must be defined; vertex is zero on default 
  //
  const StThreeVectorF *p1, *p2;
  StThreeVectorF vt;

  if(point)  {
    if(vertex) p1 = &vertex->position();
    p2 = &point->position();

    if(p1) vt = (*p2) - (*p1); // shift
    else   vt = (*p2);

    eta = vt.pseudoRapidity();
    phi = vt.phi();
    return kTRUE;
  }
  return kFALSE; // point indefined
}

Double_t 
StEmcMath::pseudoRapidity(StMeasuredPoint* point,StMeasuredPoint* vertex)
{ 
  Double_t eta, phi;
  if(etaPhi(point,vertex, eta,phi)) return eta;
  else return -9999.;
}

Double_t 
StEmcMath::phi(StMeasuredPoint* point,StMeasuredPoint* vertex)
{ 
  Double_t eta, phi;
  if(etaPhi(point,vertex, eta,phi)) return phi;
  else return -9999.;
}

UInt_t 
StEmcMath::detectorId(const StDetectorId stId)
{
  // Transition from STAR numeration to internal EMC numeration
  Int_t id = stId - kBarrelEmcTowerIdentifier + 1;
  if(id<BEMC || id> ESMDP) return 0; // Wrong value of stId
  else                     return UInt_t(id);
}

StDetectorId
StEmcMath::detectorId(const UInt_t id)
{
  // Transition from internal EMC numeration numeration to STAR
  StDetectorId stId = StDetectorId(id + kBarrelEmcTowerIdentifier - 1);
  if(stId<kBarrelEmcTowerIdentifier || stId >kEndcapSmdVStripIdentifier) 
  return kUnknownId;
  else return stId; 
}
