void runHftTree(Int_t N = 999999, const Char_t *input="./*event.root", const Char_t *output=0) {
  gROOT->LoadMacro("bfc.C");
  //  Load();
  TString Input(input);
  //TString Chain("AgML,y2013,simu,in,StEvent,mysql,libPhysics,db,StarMagField,MagF,pxlDb,HftMatTree,nodefault");
  TString Chain("ry2014a,in,StEvent,mysql,libPhysics,db,StarMagField,MagF,sstDb,pxlDb,istDb,HftMatTreeB,nodefault");
  if (Input.Contains("rcf") || Input.Contains("gstar")) Chain += ",AgML,y2013,simu";
  cout << "Chain used:\t" << Chain << endl;
  bfc(-1,Chain.Data(),input,0,output);
  StMaker *db = chain->Maker("db");
  if (db) db->SetDebug(1);
  HftMatchedTree *mtree  = (HftMatchedTree *) chain->Maker("HftMatTree");
  if (! mtree) *mtree  = new HftMatchedTree();
  if (! mtree) return;
  //#if 0
  //mtree->SetMinNoHits(2);
  //mtree->SetpCut(0.2);
  //#else
  mtree->SetMinNoHits(0);
  mtree->SetpCut(0);
  //#endif
  if (N < 0) return;
  chain->Init();
  if (N == 0) return;
  chain->EventLoop(N);
}
