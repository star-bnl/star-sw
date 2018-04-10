// $Id: StKFTrack.cxx,v 2.3 2018/04/10 11:32:09 smirnovd Exp $
#include "StKFTrack.h"
//________________________________________________________________________________
StKFTrack::StKFTrack(Int_t k, KFParticle *particle, Double_t chi2, Int_t iWE) : 
  fK(k), fWeight(-1), fW(-1), fOrigKFParticle(particle), fWestOrEast(iWE) {
  if (particle) {
    fParticle = KFParticle(*particle);
    SetChi2(chi2);
  }
}
//________________________________________________________________________________
void StKFTrack::SetChi2(Double_t chi2) {
  fChi2 = chi2;
  if (fChi2 >= 0) {
    fWeight = TMath::Exp(-fChi2/(2.*StAnneling::Temperature()));
  } else {
    fWeight = -1;
  }
}
//________________________________________________________________________________
Int_t StKFTrack::CorrectGePid(Int_t gePid) {
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
    std::cout << "Illegal gePid " << gePid << std::endl;
  }
  if (gePid < 0 || gePid > 50) gePid = 51;
  return gePid;
}
//________________________________________________________________________________
std::ostream&  operator<<(std::ostream& os,  const StKFTrack& p) {
  os << Form("%5i %9.3f %9.3f %9.3f %9.3f",
	     p.K(),p.Weight(),p.W(),p.OrigParticle()->GetZ(),p.Chi2());
  return os;
}
// $Log: StKFTrack.cxx,v $
// Revision 2.3  2018/04/10 11:32:09  smirnovd
// Minor corrections across multiple files
//
// - Remove ClassImp macro
// - Change white space
// - Correct windows newlines to unix
// - Remove unused debugging
// - Correct StTpcRTSHitMaker header guard
// - Remove unused preprocessor directives in StiCA
// - Minor changes in status and debug print out
// - Remove using std namespace from StiKalmanTrackFinder
// - Remove includes for unused headers
//
// Revision 2.2  2012/06/11 15:33:41  fisyak
// std namespace
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
