void runGmtTree(const Char_t *input, const Char_t *output=0) {
  gROOT->LoadMacro("bfc.C");
  Load();
  TString Input(input);
  TString Chain("in,StEvent,mysql,libPhysics,db,StarMagField,MagF,svtDb,ssdDb,GmtAligner,nodefault");
  if (Input.Contains("rcf") || Input.Contains("gstar")) Chain += ",y2005e,simu";
  cout << "Chain used:\t" << Chain << endl;
  bfc(-1,Chain.Data(),input,0,output);
  StMaker *db = chain->Maker("db");
  if (db) db->SetDebug(1);
  StGmtAligner *mtree  = (StGmtAligner *) chain->Maker("GmtAligner");
  if (! mtree) *mtree  = new StGmtAligner();
  if (! mtree) return;
#if 0
  mtree->SetMinNoHits(2);
  mtree->SetpCut(0.2);
#else
  mtree->SetMinNoHits(0);
  mtree->SetpCut(0);
#endif
  chain->Init();
  chain->EventLoop(9999);
}
