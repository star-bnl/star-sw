#include "StEmcMath.h"
#include "StEvent/StMeasuredPoint.h"
#include "StarClassLibrary/StThreeVectorF.hh"

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


