void na27(Char_t *dstname="mppnobc") {
  TString Dstname(Form("/star/data07/calib/fisyak/na27/%s.root",dstname));
  TFile *dst = new TFile(Dstname.Data());
  if (! dst->IsOpen()) {cout << "Can't open " << Dstname << " and die\n"; return;}
  TTree *tree = (TTree*) dst->Get("h100");
  if (! tree) {cout << "Can't find h100 TTree and die\n"; return;} 
  //  gInterpreter->ProcessLine(".L NA27MuDst.C+");
  gSystem->Load("libPhysics");
  gInterpreter->ProcessLine(".L NA27MuDst.C+");
  NA27MuDst t(tree);
  TFile *f = new TFile(Form("%sKpAna.root",dstname),"recreate");
  t.Loop();
  f->Write();
  delete f;
}
