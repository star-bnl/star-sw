Int_t migrate_run(Int_t runNumber = 0, Int_t startRunTime, TSQLServer* mysql_online, TSQLServer* mysql_offline)
{
  if (!runNumber) {
	std::cout << "please provide correct run number!" << std::endl;
	return 1; 
  }

  TString query;
  TMySQLResult* result = 0;
  StTriggerThreshold* trgdefs = 0;

  query = Form("SELECT object,idx,reg,label,value,defaultvalue FROM `Conditions_rts`.`dict` WHERE hash=(SELECT dicthash FROM `Conditions_rts`.`run` WHERE idx_rn = %d)", runNumber);

  std::cout << "ready to run query: " << query << std::endl;

  result = (TMySQLResult*)mysql_online->Query(query);

  TObjArray thresholds;

  if (result) {
    TMySQLRow* row;
    while (row = (TMySQLRow*)result->Next()) {
	  std::cout << "processing row" << std::endl;
	  trgtrss = new StTriggerThreshold();

      trgtrss->object = atoi(row->GetField(0));                                                                                                           
      trgtrss->index = atoi(row->GetField(1));                                                                                                            
      trgtrss->reg = atoi(row->GetField(2));                                                                                                              
      trgtrss->label = row->GetField(3);                                                                                                                  
      trgtrss->value = atoi(row->GetField(4));                                                                                                            
      trgtrss->defaultvalue = atoi(row->GetField(5));

	  thresholds.Add(&*trgtrss);
    }
    result->Close();
  }
  std::cout << "done with results results: " << thresholds.GetEntriesFast() << std::endl;

  for (Int_t i = 0; i < thresholds.GetEntriesFast(); i++) {
	StTriggerThreshold* trs = (StTriggerThreshold*)thresholds[i];
	trs->print();
  }

  TBufferFile buf(TBuffer::kWrite);
  buf << &thresholds;

  cout << "Serialized array of size: " << buf.BufferSize() << endl;
 
  StDbManager* mgr = StDbManager::Instance();                                                                                                      
  StDbConfigNode* node = mgr->initConfig("Calibrations_trg");                                                                                            
  StDbTable* tdT = node->addDbTable("triggerThreshold");                                                                                               
                                                                                                                                                     
  triggerThreshold_st* trgtrs = new triggerThreshold_st;
  memcpy(trgtrs->trigthr, buf.Buffer(), buf.BufferSize());
  trgtrs->size = buf.BufferSize();

  tdT->SetTable((char*)trgtrs,1);

  mgr->setStoreTime(startRunTime);

  int value = mgr->storeDbTable(tdT);                                                                                                               
  std::cout << "Migrated trigger definitions at " << mgr->getDateStoreTime() << " & run=" << runNumber << std::endl;

  return 0;
}

Int_t migrate_trigger_thresholds() {

  // Load libraries
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");

  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StTriggerUtilities");

  const char* database_online = "mysql://heston.star.bnl.gov:3501?timeout=60";
  const char* user_online = "";
  const char* pass_online = "";

  const char* database_offline = "mysql://robinson.star.bnl.gov:3306?timeout=60";
  const char* user_offline = "";
  const char* pass_offline = "";

  TSQLServer* mysql_offline = 0;
  TSQLServer* mysql_online  = 0;
  TMySQLResult* result = 0;

  mysql_offline = TMySQLServer::Connect(database_offline, user_offline, pass_offline);

  if (!mysql_offline) {
    std::cerr << "Connection to " << database_offline << " failed" << std::endl;
    return 1;
  }
  std::cout << "connected to " << database_offline << std::endl;

  // offline db: get latest beginTime - start of the run
  TString sql = "SELECT MAX(UNIX_TIMESTAMP(beginTime)) FROM `Calibrations_trg`.`triggerThreshold`";
  result = (TMySQLResult*)mysql_offline->Query(sql);
  if (!result) { std::cout << "CANNOT query offline db: " << sql << std::endl; 
	return 1; 
  }
  std::cout << "successfully queried " << database_offline << " for latest run" << std::endl;

  TMySQLRow* row = 0;
  row = (TMySQLRow*)result->Next();
  Int_t unixtime = 0;
  if (row && row->GetField(0)) { 
 	unixtime = atoi( row->GetField(0) ); 
  }
  std::cout << "got timestamp: " << unixtime << std::endl;

  mysql_online = TMySQLServer::Connect(database_online, user_online, pass_online);
  if (!mysql_online) {
    std::cerr << "Connection to " << database_online << " failed" << std::endl;
    return 1;
  }
  std::cout << "successfully connected to " << database_online << std::endl;

  // online db: get run number to migrate, based on beginTime from offline db
  sql = Form("SELECT runNumber, startRunTime FROM `RunLog`.`runDescriptor` WHERE startRunTime > %d ORDER BY startRunTime ASC LIMIT 1;", unixtime);
  result = (TMySQLResult*)mysql_online->Query(sql);
  if (!result) { std::cout << "CANNOT query offline db: " << sql << std::endl; 
	return 1; 
  }
  row = 0;
  row = (TMySQLRow*)result->Next();
  if (!row) { std::cout << "ERROR fetching row from offline: " << sql << std::endl; 
	return 1; 
  }
  Int_t run_number = atoi(row->GetField(0));  
  Int_t startRunTime = atoi(row->GetField(1));

  std::cout << "starting migration for run " << run_number << ", with beginTime: " << startRunTime << std::endl;

  Int_t error = 0;
  error = migrate_run(run_number, startRunTime, mysql_online, mysql_offline);
  if (error) {
	std::cout << "There was an error during migration..." << std::endl;
	return 1;
  }
  std::cout << "iteration successful" << std::endl;
  return 0;
}

