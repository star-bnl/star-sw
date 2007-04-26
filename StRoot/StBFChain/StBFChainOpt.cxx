//_____________________________________________________________________
// @(#)StRoot/StBFChainOpt:$Name:  $:$Id: StBFChainOpt.cxx,v 1.2 2007/04/26 20:38:00 perev Exp $
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
