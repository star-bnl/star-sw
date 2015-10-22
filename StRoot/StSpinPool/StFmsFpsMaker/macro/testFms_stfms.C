/** Macro to test FMS code on MuDST input. */
void testFms_stfms(Int_t ibegin = 1, Int_t iend = 100,
                   const char* file = "file.lis",
                   const char* qafile = "stfmsQAhisto.root",
                   Bool_t qa = true,
                   Bool_t trigger = true) {
  gROOT->Macro("load.C");  // Load all required libraries
  StChain* chain = new StChain("StChain");
  chain->SetDEBUG(0);
  StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file,
                                              ".", 1000, "MuDst");
  if (trigger) {
    // Trigger filter
    StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;
    filterMaker->addTrigger(320220);  // FMSJP1
    filterMaker->addTrigger(320231);  // FMSJP2
    filterMaker->addTrigger(320227);  // FMSLgBS2
    filterMaker->addTrigger(320226);  // FMSLgBS1
    filterMaker->addTrigger(320222);  // FMSSmBS1
    filterMaker->addTrigger(320223);  // FMSSmBS2
    // Add 2013 FMS triggers, 430601 to 430612 inc.
    for (Int_t id(1); id < 13; ++id) {
      filterMaker->addTrigger(430600 + id);
    }  // for
  }  // if
  // Database makers: basic, FMS and endcap
  St_db_Maker* dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb");
  dbMk->SetDEBUG(0);
  dbMk->SetDateTime(20130222, 0);
//  dbMk->SetDateTime(20110601, 0);
  StFmsDbMaker* fmsdb = new StFmsDbMaker("fmsDb");
  fmsdb->setDebug(1);
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;
  // Barrel ADC to energy maker
  StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;
  adc->saveAllStEvent(true);
  // FMS makers
  StFmsHitMaker* fmshitMk = new StFmsHitMaker();
  StFmsPointMaker* fmsptMk = new StFmsPointMaker("StFmsPointMaker");
  if (qa) {  // Activate QA figures
    StFmsQAHistoMaker* fmsQa = new StFmsQAHistoMaker();
    fmsQa->SetOutputFile(qafile);
  }  // if
  chain->Init();
  chain->EventLoop(ibegin,iend);
  chain->Finish();
  delete chain;
}
