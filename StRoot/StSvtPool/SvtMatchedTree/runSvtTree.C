void runSvtTree(const Char_t *input, const Char_t *output=0) {
  gROOT->LoadMacro("bfc.C");
  Load();
  TString Input(input);
  TString Chain("in,StEvent,mysql,libPhysics,db,StarMagField,MagF,svtDb,ssdDb,SvtMatTree,nodefault");
  if (Input.Contains("rcf") || Input.Contains("gstar")) Chain += ",y2005e,simu";
  cout << "Chain used:\t" << Chain << endl;
  bfc(-1,Chain.Data(),input,0,output);
  StMaker *db = chain->Maker("db");
  if (db) db->SetDebug(1);
  SvtMatchedTree *mtree  = (SvtMatchedTree *) chain->Maker("SvtMatTree");
  if (! mtree) *mtree  = new SvtMatchedTree();
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
