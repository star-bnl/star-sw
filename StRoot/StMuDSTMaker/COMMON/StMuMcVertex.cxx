// $Id: StMuMcVertex.cxx,v 1.1 2011/10/17 00:19:14 fisyak Exp $
#include "StMuMcVertex.h"
#include "Stiostream.h"
#include "TString.h"
ClassImp(StMuMcVertex);
//________________________________________________________________________________
ostream&              operator<<(ostream& os,  const StMuMcVertex& v) {
  os << Form("Vx:%4i NoTk:%4i Time:%4i parent:%6i",v.Id(),v.NoDaughters(),v.Time(),v.IdParTrk());
  os << Form(" xyz:%8.3f %8.3f %8.3f",v.XyzV().x(),v.XyzV().y(),v.XyzV().z());
  return os;
}
//________________________________________________________________________________
void StMuMcVertex::Print(Option_t *option) const {cout << *this << endl;}
//________________________________________________________________________________
// $Log: StMuMcVertex.cxx,v $
// Revision 1.1  2011/10/17 00:19:14  fisyak
// Active handing of IdTruth
//
