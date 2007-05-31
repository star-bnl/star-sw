void loadGammaLibs()
{
  //-- Load muDst shared libraries --                                                                          
  gROOT -> LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  //gSystem->Load("StDetectorDbMaker");

  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("libgeometry_Tables");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcUtil");

  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcSimulatorMaker");

  gSystem->Load("StEEmcA2EMaker");
  gSystem->Load("StEEmcClusterMaker");
  gSystem->Load("StEEmcPointMaker");
  gSystem->Load("StEEmcPi0Mixer");
  gSystem->Load("StGammaMaker");

}
