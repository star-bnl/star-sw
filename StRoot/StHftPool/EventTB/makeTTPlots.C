void makeTTPlots(const Char_t *tag = "Plots") {
// test data file:
///data09/calib/fisyak/Pass112/TpcSsd/065/Event_6065045_raw_1010001.root
  int err2 = gROOT->LoadMacro("Xdcor.C++");
  cout << ".L Xdcor.C++  is done: " << err2   << endl;
  if (err2) return;
  int err1 = gROOT->LoadMacro("TT.C++");
  cout << ".L TT.C++  is done: " << err1   << endl;
  if (err1) return;

  TString FileN(gDirectory->GetName());
  gInterpreter->ProcessLine(".L Chain.C");
  TChain *theChain = Chain();
  cout << "Chain pointer: " << theChain << endl;
  TreeClass *t = new TreeClass((TTree*)theChain);
  cout << "TreeClass object t is constructed" << endl;
  TString Out = gSystem->DirName(FileN); cout << Out << endl;
  TString Dir = gSystem->DirName(Out);
  if (Out.BeginsWith("/")) Out = Out.Data()+1;
  Out.ReplaceAll("/","_");
  Out += tag; 
  TString Tag(tag);
  t->SetMinNoFitPoints(25);
  Out += "TBNFP25";
  Out += Form("rCut%imm",(int)(t->GetRCut()*10.));
  Out += ".root";
  cout << " ===> " << Out << endl;
  t->SetOutFileName(Out);
  t->Loop();
  delete t; t=0;
}
