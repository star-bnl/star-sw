void makeTPlots(const Char_t *tag = ""){//"dEdx") {
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
#if 1
  TString Out = gSystem->DirName(FileN); cout << Out;
  TString Dir = gSystem->DirName(Out);
  //  Out.ReplaceAll(Dir,"");
  if (Out.BeginsWith("/")) Out = Out.Data()+1;
#else
  TString Out = gSystem->BaseName(FileN);
  Out.ReplaceAll(".root","");
#endif
  Out.ReplaceAll("/","_");
  //   if (FileN.Contains("6073006_raw") Out += "R06";
  //   if (FileN.Contains("6073023_raw") Out += "R23";
  Out += "Plots"; Out += tag; 
  TString Tag(tag);
  if (Tag.Contains("G14G",TString::kIgnoreCase) || 
      Tag.Contains("G24G",TString::kIgnoreCase) || 
      Tag.Contains("G34G",TString::kIgnoreCase) ||
      Tag.Contains("G44G",TString::kIgnoreCase)) {
    t.SetuMinMax(2.5, 2.9);
    Out += "_u_2.5-2.9";
    //    Out += "_u_2.5-2.9_v_0-2.8";
    //    t.SetvMinMax(0.0, 2.8);
  }
  if (Tag.Contains("G15G",TString::kIgnoreCase)) {
    t.SetuMinMax(2.0, 2.5);
    Out += "_u_2.0-2.5";
    //    Out += "_u_2.5-2.9_v_0-2.8";
    //    t.SetvMinMax(0.0, 2.8);
  }
  if (Tag.Contains("G21G",TString::kIgnoreCase)) {
    t.SetVertexZCut(2.0); // cm from z = 0
  }
  if (Tag.Contains("G22G",TString::kIgnoreCase)) {
    t.SetDipCut(0.9); // pT < DipCut*p 
  }
  if (Tag.Contains("NoW",TString::kIgnoreCase)) {
    t.SetNoWafers(); 
  }
  if (Tag.Contains("BL",TString::kIgnoreCase)) {
    t.SetLaddersInGlobal(kTRUE); 
  }
  if (Tag.Contains("ssd",TString::kIgnoreCase)) {
    t.SetSsd(kTRUE); 
  }
  if (Tag.Contains("svt",TString::kIgnoreCase)) {
    t.SetSvt(kTRUE); 
  }
  if (Tag.Contains("east",TString::kIgnoreCase)) {
    if (Tag.Contains("fareast",TString::kIgnoreCase))    t.SetEastWest(3);
    else                                                 t.SetEastWest(1);
  } else {
    if (Tag.Contains("west",TString::kIgnoreCase)) {
      if (Tag.Contains("farwest",TString::kIgnoreCase)) t.SetEastWest(4);
      else                                              t.SetEastWest(2);
    }
  }
  if (Tag.Contains("global",TString::kIgnoreCase)) {
    t.UseGlobal();
  }
  if (Tag.Contains("local",TString::kIgnoreCase)) {
    t.UseLocal();
  }  
  if (Tag.Contains("dEdx",TString::kIgnoreCase)) {
    t.SetdEdxCut(4.e-6,40.);
  }  
  //  t.SetRCut(1.0);
  t.SetRCut(0.5);
  t.SetMinNoFitPoints(25);
  Out += "NFP25";
  Out += Form("rCut%3.1fcm",t.GetRCut());
  Out += ".root";
  cout << " ===> " << Out << endl;
  t.SetOutFileName(Out);
  t.Loop(0);
}
