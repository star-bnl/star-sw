Int_t migrate_run(Int_t runNumber = 0, Int_t startRunTime, TSQLServer* mysql_online, TSQLServer* mysql_offline)
{
  if (!runNumber) {
	std::cout << "please provide correct run number!" << std::endl;
	return 1; 
  }

  TString query;
  TMySQLResult* result = 0;
  StTriggerDefinition* trgdefs = 0;

  query = Form("SELECT ct.idx_trigger, ct.name, ct.offlineBit, cp.onbits, cp.offbits, cp.onbits1, cp.onbits2, cp.onbits3, 
	cp.offbits1, cp.offbits2, cp.offbits3 
	FROM `Conditions_rts`.`triggers` ct, `Conditions_rts`.`pwc` cp WHERE ct.idx_rn = %d AND ct.idx_rn = cp.idx_rn AND ct.idx_trigger = cp.idx_idx", runNumber);

  std::cout << "ready to run query: " << query << std::endl;

  result = (TMySQLResult*)mysql_online->Query(query);

  TObjArray definitions;

  if (result) {
    TMySQLRow* row;
    while (row = (TMySQLRow*)result->Next()) {
	  std::cout << "processing row" << std::endl;
	  trgdefs = new StTriggerDefinition();
      Int_t triggerIndex = atoi(row->GetField(0));
      assert(0 <= triggerIndex && triggerIndex < 32);
      trgdefs->triggerIndex = triggerIndex;
      trgdefs->name = row->GetField(1);
      trgdefs->triggerId = atoi(row->GetField(2));
      trgdefs->onbits   = atoi(row->GetField(3));
      trgdefs->offbits  = atoi(row->GetField(4));
      trgdefs->onbits1  = atoi(row->GetField(5));
      trgdefs->onbits2  = atoi(row->GetField(6));
      trgdefs->onbits3  = atoi(row->GetField(7));
      trgdefs->offbits1 = atoi(row->GetField(8));
      trgdefs->offbits2 = atoi(row->GetField(9));
      trgdefs->offbits3 = atoi(row->GetField(10));
	  definitions.Add(&*trgdefs);
    }
    result->Close();
  }
  std::cout << "done with results results: " << definitions.GetEntriesFast() << std::endl;

  for (Int_t i = 0; i < definitions.GetEntriesFast(); i++) {
	StTriggerDefinition* def = (StTriggerDefinition*)definitions[i];
	def->print();
  }


  std::cout << "COLLECTION PRINT: " << std::endl;
  definitions.Print();

  TBufferFile buf(TBuffer::kWrite);
  buf << &definitions;

  cout << "Serialized array of size: " << buf.BufferSize() << endl;
 
  StDbManager* mgr = StDbManager::Instance();                                                                                                      
  StDbConfigNode* node = mgr->initConfig("Calibrations_trg");                                                                                            
  StDbTable* tdT = node->addDbTable("triggerDefinition");                                                                                               
                                                                                                                                                     
  triggerDefinition_st* trgdef = new triggerDefinition_st;
  
  ofstream out("sizes.txt", std::ios::app);                                                                                 
  out << buf.BufferSize() << std::endl;                                                                            
  out.close();
  return 0; // FIXME
  
  memset(trgdef->trigdef, 0, 128*1024*1024);
  memcpy(trgdef->trigdef, buf.Buffer(), buf.BufferSize());
  trgdef->size = buf.BufferSize();


//  ofstream out2("test-comp.out", std::ios::binary || std::ios::out);                                                                                 
//  out2.write(reinterpret_cast<char*>(trgdef->trigdef), trgdef->size);                                                                            
//  out2.close();

  tdT->SetTable((char*)trgdef,1);

  mgr->setStoreTime(startRunTime);

  int value = mgr->storeDbTable(tdT);                                                                                                              
  std::cout << "Migrated trigger definitions at " << mgr->getDateStoreTime() << " & run=";                                                                      
  std::cout << runNumber << std::endl;

  return 0;
}

Int_t migrate_trigger_definitions() {

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
  TString sql = "SELECT MAX(UNIX_TIMESTAMP(beginTime)) FROM `Calibrations_trg`.`triggerDefinition`";
  result = (TMySQLResult*)mysql_offline->Query(sql);
  if (!result) { 
	std::cout << "CANNOT query offline db: " << sql << std::endl; 
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
  if (!row || !row->GetField(0)) { 
	std::cout << "ERROR fetching row from " << mysql_online << ", all runs migrated? " << sql << std::endl; 
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

