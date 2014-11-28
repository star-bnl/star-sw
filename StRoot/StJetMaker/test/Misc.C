#include "Misc.hh"

#include "UniqueStringGenerator.hh"

#include <TDirectory.h>
#include  <TTree.h>

#include <string>

using namespace std;

TDirectory *setupTestTDirecotry()
{
  string testdir(UniqueStringGenerator::generate());
  gROOT->cd();
  gDirectory->mkdir(testdir.c_str());
  gDirectory->cd(testdir.c_str());
  new TTree("bemcTowers", "bemcTowers");
  new TTree("tpcTracks", "tpcTracks");
}
