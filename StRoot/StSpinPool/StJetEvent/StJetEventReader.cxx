//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 15 September 2009
//

// C++ STL
#include <functional>
using namespace std;

// ROOT
#include "TChain.h"
#include "TStopwatch.h"

// STAR
#include "StSpinPool/StJetEvent/StJetEvent.h"
#include "StSpinPool/StJetEvent/StJetEventAnalyzer.h"
#include "StSpinPool/StJetSkimEvent/StJetSkimEvent.h"

// Local
#include "StJetEventReader.h"

ClassImp(StJetEventReader);

StJetEventReader::StJetEventReader(const char* jetfile, const char* skimfile)
  : mJetChain(new TChain("jet"))
  , mSkimChain(new TChain("jetSkimTree"))
  , mEntry(0)
{
  mJetChain->Add(jetfile);
  mSkimChain->Add(skimfile);

  for (int i = 0; i < mJetChain->GetNbranches(); ++i) {
    TBranch* branch = (TBranch*)mJetChain->GetListOfBranches()->At(i);
    const char* branchname = branch->GetName();
    if (strstr(branchname,"ConeJets12")) addJetBranch(branchname);
  }

  addJetBranch("PythiaConeJets");
  addSkimBranch("skimEventBranch");
}

StJetEventReader::~StJetEventReader()
{
  delete mJetChain; mJetChain = 0;
  delete mSkimChain; mSkimChain = 0;
}

StJetEvent* StJetEventReader::getJetEvent(const char* branchname) const
{
  TBranch* branch = mJetChain->GetBranch(branchname);
  if (branch) return *(StJetEvent**)branch->GetAddress();
  return 0;
}

StJetSkimEvent* StJetEventReader::getSkimEvent(const char* branchname) const
{
  TBranch* branch = mSkimChain->GetBranch(branchname);
  if (branch) return *(StJetSkimEvent**)branch->GetAddress();
  return 0;
}

bool StJetEventReader::next() const
{
  return mJetChain->GetEntry(mEntry) && mSkimChain->GetEntry(mEntry++);
}

void StJetEventReader::addJetBranch(const char* branchname)
{
  TBranch* branch = mJetChain->GetBranch(branchname);
  if (branch) branch->SetAddress(new StJetEvent*(0));
}

void StJetEventReader::addSkimBranch(const char* branchname)
{
  TBranch* branch = mSkimChain->GetBranch(branchname);
  if (branch) branch->SetAddress(new StJetSkimEvent*(0));
}

void StJetEventReader::eventLoop(int nentries)
{
  TStopwatch stopwatch;
  while (nentries-- && next()) {
    if (nentries % 1000 == 0) printf("nentries = %d\n",nentries);
    for_each(mAnalyzers.begin(),mAnalyzers.end(),bind2nd(mem_fun(&StJetEventAnalyzer::analyze),this));
  }
  stopwatch.Print();
}
