// void RootShower()
{
   gSystem->Load("libPhysics");
   gSystem->Load("libEG");
       
   TString rootEnv         = gSystem->Getenv("ROOTSYS");
   TString rootShowerPath  = rootEnv        + "/test/qtRootShower";
   TString dynPath = gSystem->GetDynamicPath();
   TString rootShowerDLL   = "libRootShower";
   TString rootShowerMacro = TROOT::GetMacroPath();
   TString sysName = gSystem->GetName();
   if (sysName.Contains("Unix") ) {
      dynPath         += ":";
      rootShowerMacro += ":";
   } else {
      dynPath         += ";";
      rootShowerMacro += ";";
   }
   dynPath += rootShowerPath;
   rootShowerMacro + = rootShowerPath;
   gSystem->SetDynamicPath(dynPath.Data() );
   TROOT::SetMacroPath(rootShowerMacro.Data());

   TString rootTutorials = rootEnv + "/tutorials";
	// printf(" %s\n", rootShowerDLL.Data());
   gSystem->Load(rootShowerDLL.Data());
	new ShowerMain();     
}
