
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


void 
readndst(const char *fName="/star/current/ndst/run*.ndst.root")
{
  loadlibs();
  TChain *chain = new TChain("ttm");
  chain->Add(fName);
  Double_t nev = chain->GetEntries();

  cerr << nev << endl;

  TList          *matchlist       = new TList;                  
  StEventInfo    *evinfo          = new StEventInfo();
  StEventSummary *evsumm          = new StEventSummary();
  StMuTriggerIdCollection *evtrig = new StMuTriggerIdCollection();

  chain->SetBranchAddress("matches",&matchlist);
  chain->SetBranchAddress("info"   ,&evinfo);
  chain->SetBranchAddress("summary",&evsumm);
  chain->SetBranchAddress("trigger",&evtrig);

  for(int i=0; i<nev; i++) {
    EEmcTTMatch *tmatch;
    chain->GetEntry(i);
    if(matchlist->IsEmpty()) continue;
    StTriggerId &trig = evtrig->nominal();
    cerr << "<Event id=\""        << evinfo->id() << "\" run=\" " << evinfo->runId() << "\" >\n";
    cerr << "<MagneticField b=\"" << evsumm->magneticField() << "\" >\n";
    TIter nextMatch(matchlist);
    while ((tmatch = (EEmcTTMatch *) nextMatch())) 
      tmatch->Out(cerr);
    cerr << "</Event>\n" << endl;
  }
} 
