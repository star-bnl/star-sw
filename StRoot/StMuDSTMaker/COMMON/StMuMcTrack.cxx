// $Id: StMuMcTrack.cxx,v 1.3 2012/11/26 23:08:23 fisyak Exp $
#include "StMuMcTrack.h"
#include "Stiostream.h"
#include "TString.h"
#include "TMath.h"
ClassImp(StMuMcTrack);
//________________________________________________________________________________
ostream&              operator<<(ostream& os,  const StMuMcTrack& v) {
  os << Form("Tk:%4i Vx:%4i Ge:%4i NoHits:%3i",v.Id(),v.IdVx(),v.GePid(),v.NoHits());
  os << Form(" q:%2i pT:%7.3f eta:%6.3f phi:%6.3f p:%8.3f px:%8.3f py:%8.3f pz:%8.3f",v.Charge(),v.pT(), v.Eta(), 
	     TMath::ATan2(v.Pxyz().y(),v.Pxyz().x()), v.Ptot(), v.Pxyz().x(),v.Pxyz().y(),v.Pxyz().z());
  return os;
}
//________________________________________________________________________________
Int_t StMuMcTrack::CorrectGePid(Int_t gePid) {
  // By pass embedding particle redefinition
  if (gePid ==           99) gePid =         11151;
  if (gePid ==          207) gePid =            41;
  if (gePid ==        40001) gePid =            24;
  if (gePid ==           98) gePid =            18;
  if (gePid ==        40002) gePid =            32;
  if (gePid ==           97) gePid =            26;
  if (gePid ==        40003) gePid =            23;
  if (gePid ==        40004) gePid =            31;
  if (gePid ==        40005) gePid =            22;
  if (gePid ==        40006) gePid =            30;
  if (gePid ==        10150) gePid =           150;
  if (gePid ==        10151) gePid =           151;
  if (gePid ==        11151) gePid =         10151;
  if (gePid ==        10018) gePid =            98;
  if (gePid ==        10026) gePid =            97;
  if (gePid ==        10017) gePid =            17;
  if (gePid ==        10039) gePid =            39;
  if (gePid ==        10040) gePid =            40;
  if (gePid ==           98) gePid =            18;
  if (gePid ==           97) gePid =            26;
  if (gePid < 0 || gePid > 50) {
    //    cout << "Illegal gePid " << gePid << endl;
    gePid = 51;
  }
  return gePid;
}
//________________________________________________________________________________
const Char_t *StMuMcTrack::GeName() {
  static const Char_t *GeNames[52] = {
    //   1       2       3      4         5           6       7        8         9          10
    "",
    "gamma"   ,"e+"   ,"e-"  ,"nu"   ,"mu+"   ,"mu-"   ,"pi0"  ,"pi+"   ,"pi-"      ,"K0L",
    "K+"      ,"K-"   ,"N"   ,"P"    ,"Pbar"  ,"K0S"   ,"eta"  ,"Lambda","Sigma+"   ,"Sigma0",
    "S-"      ,"Xi0"  ,"Xi-" ,"Omega","Nbar"  ,"LamBar","SBar-","SBar0" ,"SBar+"    ,"XiBar0",
    "XiBar+"  ,"OmBar","tau+","tau-" ,"D+"    ,"D-"    ,"D0"   ,"Dbar0" ,"Ds+"      ,"Ds-"   ,
    "LambC+"  ,"W+"   ,"W-"  ,"Z0"   ,"H2"    ,"H3"    ,"alpha","geanti","He3"      ,"Cerenk",
    "??????"};
  static TString Name;
  Int_t iGe = CorrectGePid(GePid());
  Name = GeNames[iGe];
  return Name.Data();
}
//________________________________________________________________________________
void StMuMcTrack::Print(Option_t *option) const {cout << *this << endl;}
//________________________________________________________________________________
// $Log: StMuMcTrack.cxx,v $
// Revision 1.3  2012/11/26 23:08:23  fisyak
// Fix print out, thanks to Jonathan (bug #2442)
//
// Revision 1.2  2012/05/07 14:47:06  fisyak
// Add handles for track to fast detector matching
//
// Revision 1.1  2011/10/17 00:19:13  fisyak
// Active handing of IdTruth
//
