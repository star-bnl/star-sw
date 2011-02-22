//_____________________________________________________________________
// @(#)StRoot/StBFChainOpt:$Name:  $:$Id: StBFChainOpt.cxx,v 1.3 2011/02/22 19:16:44 perev Exp $
//_____________________________________________________________________
#include "TROOT.h"
#include "TString.h"
#include "StBFChainOpt.h"
#include "StBFChain.h"
ClassImp(StBFChainOpt);
//_____________________________________________________________________
StBFChainOpt::StBFChainOpt(StBFChain *bfc):StChainOpt("StBFChainOpt")
{
  fBFChain = bfc;
}
// //_____________________________________________________________________
// Int_t StBFChainOpt::kOpt(const char *Tag) const
// {
//   return fBFChain->kOpt(Tag,0);
// }
//_____________________________________________________________________
const TString &StBFChainOpt::GetFileIn() const
{
  return fBFChain->GetFileIn();
}
//_____________________________________________________________________
const TString &StBFChainOpt::StBFChainOpt::GetFileOut() const
{
  return fBFChain->GetFileOut();
}
//_____________________________________________________________________
TFile *StBFChainOpt::GetTFile() const
{
  return fBFChain->GetTFile();
}
//_____________________________________________________________________
TString StBFChainOpt::GetGeometry() const
{
  return fBFChain->GetGeometry();
}
