write_fgtSimuParams()
{
  gSystem->Setenv("DB_ACCESS_MODE", "write"); //jb add
    // Load all required libraries
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("St_base.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");

    // Initialize db manager
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fgt");
    StDbTable* dbtable = node->addDbTable("fgtSimuParams");
    TString storeTime = "2011-11-11 11:11:12"; // beginTime timestamp in MySQL format: "YYYY-MM-DD HH:mm:ss"
    mgr->setStoreTime(storeTime.Data());

    // Create your c-struct
    fgtSimuParams_st table;
    memset(table.param,0, sizeof(table.param));
    
    // Fill structure with data 

    /* description of the parameters */
    strncpy(table.comment," reasonable fgt-slow-simu parametrization, ver=102, by Jan",192);  

    table.param[0] =102;// fgt-simu-setup version # 

    // analog signal propagation
    table.param[1] = 9992; // meLossTab[9993-10000]=5.0E+03 to 6.0E+06 (too high) are cut off. They are replaced by meLossTab[9992]=4.78E+03 in simulation. 
    table.param[2] = 40. ; //( ions/cm) # of primary pairs produced per cm of path
    table.param[3] = 80.e-9; //  (seconds) track ignored by FGT slow sim
    table.param[4] = 0.035 ; // cm, for 2D gauss smearing, default=0.035 for FNAL
    table.param[5] = 0.005; // (GeV) track ignored by FGT slow sim
    table.param[6] = 0.017; //  (cm per 1 cm of path)
    table.param[7] = 6; // # of bins in 2D distribution to store 2D gauss
    table.param[8] = -1008; // not used 
    table.param[9] = -1009; // not used
 
    // digitalization
    table.param[10] = 5.0;  //  drop strips below it
    table.param[11] = 1000; // in a.u. used in simu
    table.param[12] = 2.0;  // a factor making simulated ADCs comparable to 2012 pp510 data 
    table.param[13] = 0.45; // divide charge between P/R plane

    table.param[14] =  -1014; // not used
    table.param[15] =  -1015; // not used


    // Store data to the StDbTable
    dbtable->SetTable((char*)&table, 1);

    // uncomment next line to set "sim" flavor. "ofl" flavor is set by default, no need to set it.
    // dbtable->setFlavor("sim");

    // Store table to database
    mgr->storeDbTable(dbtable);
}
