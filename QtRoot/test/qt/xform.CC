{
  TString rootEnv    = gSystem->Getenv("ROOTSYS");
  TString xformPath  = rootEnv + "/test/qt";
  TString dynPath = gSystem->GetDynamicPath();

  TString xformDLL   = "libxform";
  TString xformMacro = TROOT::GetMacroPath();
  TString sysName = gSystem->GetName();
  if (sysName.Contains("Unix") ) {
     dynPath    += ":";
     xformMacro += ":";
  } else {
     dynPath    += ";";
     xformMacro += ";";
  }
  dynPath    += xformPath;
  xformMacro += xformPath;
  gSystem->SetDynamicPath(dynPath.Data() );
  TROOT::SetMacroPath(xformMacro.Data());
  TString rootTutorials = rootEnv + "/tutorials";

  //  Go to ROOT tutorials directory
  // gSystem->ChangeDirectory(rootTutorials.Data());

  //  Load XForm using the GUI thread
   gSystem->Load(xformDLL);
   gROOT  ->Macro("demos.C");
}
