#include <vector>

void 
loadlibs()
{
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  // load more libraries :)
  gSystem->Load("libmysqlclient");
  gSystem->Load("StEvent");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  // load even more libraries (EEMC stuff) 
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcPoolTTM");
}


void readnanodst()
{
  loadlibs();
  TFile *ttm = new TFile("ttm093.ndst.root");
  TTree *t   = (TTree *)ttm->Get("ttm");
  int nev = t->GetEntries();

  TList          *matchlist       = new TList;                  
  StEventInfo    *evinfo          = new StEventInfo();
  StEventSummary *evsumm          = new StEventSummary();
  StMuTriggerIdCollection *evtrig = new StMuTriggerIdCollection();

  t->GetBranch("matches")->SetAddress(&matchlist);
  t->GetBranch("info")->SetAddress(&evinfo);
  t->GetBranch("summary")->SetAddress(&evsumm);
  t->GetBranch("trigger")->SetAddress(&evtrig);

  for(int i=0; i<nev; i++) {
    EEmcTTMatch *tmatch;
    t->GetEntry(i);
    if(matchlist->IsEmpty()) continue;
    StTriggerId &trig = evtrig->nominal();
    cerr << "<Event id=\""        << evinfo->id() << "\" run=\" " << evinfo->runId() << "\" >\n";
    cerr << "<MagneticField b=\"" << evsumm->magneticField() << "\" >\n";
    TIter nextMatch(matchlist);
    //while ((tmatch = (EEmcTTMatch *) nextMatch())) 
    //  tmatch->Out(cerr);
    cerr << "</Event>\n" << endl;
  }
} 
