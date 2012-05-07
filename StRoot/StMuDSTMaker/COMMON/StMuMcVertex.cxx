// $Id: StMuMcVertex.cxx,v 1.2 2012/05/07 14:47:06 fisyak Exp $
#include "StMuMcVertex.h"
#include "Stiostream.h"
#include "TString.h"
#include "TMath.h"
ClassImp(StMuMcVertex);
//________________________________________________________________________________
ostream&              operator<<(ostream& os,  const StMuMcVertex& v) {
  os << Form("Mc:%4i NoTk:%4i T:%4i",v.Id(),v.NoDaughters(),TMath::Nint(1e7*v.Time()));
  os << Form(" xyz:%8.3f %8.3f %8.3f",v.XyzV().x(),v.XyzV().y(),v.XyzV().z());
  return os;
}
//________________________________________________________________________________
void StMuMcVertex::Print(Option_t *option) const {cout << *this << endl;}
//________________________________________________________________________________
// $Log: StMuMcVertex.cxx,v $
// Revision 1.2  2012/05/07 14:47:06  fisyak
// Add handles for track to fast detector matching
//
// Revision 1.1  2011/10/17 00:19:14  fisyak
// Active handing of IdTruth
//
