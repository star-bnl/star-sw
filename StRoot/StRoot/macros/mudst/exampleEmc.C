// This is an example of how to read the MuDst and do the clusterfinding 
// for the BEMC

void exampleEmc(const Char_t *infile="/star/data24/reco/production_dAu2008/ReversedFullField/P08ic/2008/026/9026032/st_physics_adc_9026032_raw_1070010.MuDst.root",const Int_t n_event=100) {
  gROOT->Macro("loadMuDst.C");

  // Load St_db_Maker and co
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbBroker.so");
  gSystem->Load("St_db_Maker");

  // Load Emc libraries
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");

  StChain *chain=new StChain;
  StMuDstMaker *mudst_mk=new StMuDstMaker(0,0,"",infile,"",999);

  // Need St_db_Maker for Emc calibration
  St_db_Maker *db1 = new St_db_Maker("db","$HOME/StarDb","MySQL:StarDb","$STAR/StarDb");

  // Maker to apply calibration
  StEmcADCtoEMaker *adc_to_e=new StEmcADCtoEMaker();
  adc_to_e->setPrint(kFALSE);
  // Makers for clusterfinding
  StPreEclMaker *pre_ecl=new StPreEclMaker();
  pre_ecl->setPrint(kFALSE);
  StEpcMaker *epc=new StEpcMaker();
  epc->setPrint(kFALSE);

  chain->Init();
   
  // This is how you can set alternative clustering parameters
  /*
  pre_ecl->SetClusterConditions("bemc",  4, 0.2, 0.1, 0.05, kFALSE);
  
  Int_t sizeMax = 5;
  Float_t energySeed = 0.2;
  Float_t energyAdd  = 0.1;
  Float_t minTotE  = 0.05;
  
  pre_ecl->SetClusterConditions("bsmde", sizeMax,energySeed, energyAdd, 
		                  minTotE, kFALSE);
  pre_ecl->SetClusterConditions("bsmdp", sizeMax,energySeed, energyAdd, 
		                  minTotE, kFALSE);
  */	  

  Int_t i_event=0;
  while (i_event < n_event && chain->Make() == kStOk) {

    StEmcCollection *emcCollection = mudst_mk->muDst()->emcCollection();

    if (emcCollection) {
      cout << emcCollection->barrelPoints().size() << " points in barrel" << endl;

      StEmcDetector *barrel = emcCollection->detector(kBarrelEmcTowerId);
      if (barrel->cluster()) 
        cout << barrel->cluster()->clusters().size() << " barrel tower clusters" << endl;
      StEmcDetector *smde = emcCollection->detector(kBarrelSmdEtaStripId);
      if (barrel->cluster()) 
        cout << smde->cluster()->clusters().size() << " smd eta clusters" << endl;
      StEmcDetector *smdp = emcCollection->detector(kBarrelSmdPhiStripId);
      if (barrel->cluster()) 
        cout << smdp->cluster()->clusters().size() << " smd phi clusters" << endl;
    }
    else {
      cout << "No emc collection!" << endl;
    }
    
    i_event++;
    chain->Clear();
  }
}
