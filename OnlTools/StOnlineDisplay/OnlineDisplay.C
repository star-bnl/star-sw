{
gSystem->Load("StarRoot");
gSystem->Load("StarClassLibrary");
gROOT->Macro("Load.C");
gSystem->Load("RTS");
gSystem->Load("StTpcDb");
gSystem->Load("St_db_Maker");
gSystem->Load("StDetectorDbMaker");
gSystem->Load("StDbUtilities");
gSystem->Load("StDaqLib");
gSystem->Load("StEmcUtil"); 
gSystem->Load("StOnlineDisplay");
#if 0
 TString ivrootDir = "$IVROOT/lib/";
 gSystem->ExpandPathName(ivrootDir);
 
 fprintf(stderr," Loading ... Coin3D from  %s     \n",(const char*)ivrootDir);
 bool CheckCoin = true;
   if (!gSystem->AccessPathName(ivrootDir.Data())) {
      ivrootDir="";
      fprintf(stderr," Loading ... libSoQt.so %d     \n",gSystem->Load(ivrootDir+"libSoQt"));
      fprintf(stderr," Loading ... libCoin.so %d     \n",gSystem->Load(ivrootDir+"libCoin"));
      fprintf(stderr," Loading ... libSmallChange %d \n",gSystem->Load(ivrootDir+"libSmallChange"));
      CheckCoin = false;
   }
gSystem->Load("libQtOpenGL.so");
gSystem->Load("ONLINE.so");
  StStartDispay *a = new StStartDispay;
  a->SetDaqFileName();
#endif  
//  a->Show();
//  a->ShowStarEvent();
}
