// $Id: StMuMcTrack.cxx,v 1.1 2011/10/17 00:19:13 fisyak Exp $
#include "StMuMcTrack.h"
#include "Stiostream.h"
#include "TString.h"
ClassImp(StMuMcTrack);
//________________________________________________________________________________
ostream&              operator<<(ostream& os,  const StMuMcTrack& v) {
  os << Form("Tk:%4i Vx:%4i Ge:%4i NoHits:%3i",v.Id(),v.IdVx(),v.GePid(),v.NoHits());
  os << Form(" q:%2i pT:%7.3f eta:%6.3f phi:%6.3f, p:%8.3f %8.3f %8.3f",v.Charge(),v.Pxyz().x(),v.Pxyz().y(),v.Pxyz().z());
  return os;
}
//________________________________________________________________________________
void StMuMcTrack::Print(Option_t *option) const {cout << *this << endl;}
//________________________________________________________________________________
// $Log: StMuMcTrack.cxx,v $
// Revision 1.1  2011/10/17 00:19:13  fisyak
// Active handing of IdTruth
//
