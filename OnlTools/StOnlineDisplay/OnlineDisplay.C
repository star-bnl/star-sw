class StSteeringModule;
StSteeringModule *displayMaker=0;
StSteeringModule *OnlineDisplay (const char *daqFile=
      "/star/data03/daq/2007/346/8346052/st_physics_8346052_raw_1010011.daq"
      ) {
gROOT->Macro("Load.C");
gSystem->Load("RTS");
gSystem->Load("StTpcDb");
gSystem->Load("St_db_Maker");
gSystem->Load("StDetectorDbMaker");
gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
gSystem->Load("StDaqLib");
gSystem->Load("StEmcUtil"); 
gSystem->Load("StMagF");
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
 StSteeringModule *ds = new StSteeringModule; 
 ds->SetNumber(21);
 ds->SetDaqFileName(daqFile);
 ds->SetL3TracksOn(0);
 ds->SetL3HitsOn(1);
 ds->SetEmcHitsOn(0);
 ds->SetMagneticField(0.5);
 ds->Init();
 ds->Make();
 displayMaker=ds;
 return ds;
}
