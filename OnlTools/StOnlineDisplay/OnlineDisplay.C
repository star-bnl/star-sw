class StSteeringModule;
StSteeringModule *displayMaker=0;

StSteeringModule *OnlineDisplay (const char *daqFile=
      "st_physics_11029020_raw_1030002.daq"
//      "st_physics_adc_10118050_raw_4320001.daq"     
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
 StSteeringModule *ds = new StSteeringModule; 
 ds->SetNumber(21);
 ds->SetDaqFileName(daqFile);
 ds->SetL3TracksOn(0);
 ds->SetL3HitsOn(1);
 ds->SetEmcHitsOn(0);
 ds->SetMagneticField(0.5); 
//  ds->SetDebug(4);
 ds->Init();
 ds->Make();
 displayMaker=ds;
 return ds;
}

StSteeringModule *OnlineDisplay (int runNumber) {
  StSteeringModule *ds = OnlineDisplay("");
  ds->SetRunNumber(runNumber); 
  ds->Clear();
  ds->Make();
  return ds;
}
