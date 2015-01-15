void makeAPlots(const Char_t *tag = "Anodes") {
  TString FileN(gDirectory->GetName());
  gInterpreter->ProcessLine(".L Chain.C");
  TChain *theChain = Chain();
  TString macro(".L ");
  const Char_t *T = gSystem->Which(gROOT->GetMacroPath(),"T.C");
  macro += T; delete [] T;
  macro += "+";
  gInterpreter->ProcessLine(macro);
  //  gInterpreter->ProcessLine(".L T.C+");
  TT t(theChain);
  TString Out = gSystem->DirName(FileN); cout << Out;
  TString Dir = gSystem->DirName(Out);
  //  Out.ReplaceAll(Dir,"");
  if (Out.BeginsWith("/")) Out = Out.Data()+1;
  Out.ReplaceAll("/","_");
  //   if (FileN.Contains("6073006_raw") Out += "R06";
  //   if (FileN.Contains("6073023_raw") Out += "R23";
  Out += tag; 
  TString Tag(tag);
  Out += ".root";
  cout << " ===> " << Out << endl;
  t.SetOutFileName(Out);
  t.Loop4BadAnodes();
}
