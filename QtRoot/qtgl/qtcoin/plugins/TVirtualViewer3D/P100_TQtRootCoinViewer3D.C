void P100_TQtRootCoinViewer3D()
{
  if (TString(gSystem->GetName()) != "WinNT" ) {
     TString ivrootDir = gSystem->Getenv("IVROOT");
     if (ivrootDir.IsNull() ) {
         ivrootDir = "$ROOT/5.99.99/Coin2Qt4/$STAR_HOST_SYS/coin3d";
     }
     if (!ivrootDir.IsNull()) {
         ivrootDir += "/lib/";
         gSystem->ExpandPathName(ivrootDir);
         if (!gSystem->AccessPathName(ivrootDir.Data()) ) {
             if ( ! (gSystem->Load(ivrootDir+"libSoQt") + 
                gSystem->Load(ivrootDir+"libCoin") 
             // + gSystem->Load(ivrootDir+"libSmallChange") 
             )  ) {}
         }
     } 
  }
  gPluginMgr->AddHandler("TVirtualViewer3D", "oiv", "TQtRootCoinViewer3D",
      "RQIVTGL", "TQtRootCoinViewer3D(TVirtualPad*)");
}
