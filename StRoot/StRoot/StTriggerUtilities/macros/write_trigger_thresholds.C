int write_trigger_thresholds(int runNumber = 13078009)
{

  // Load all required libraries
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StTriggerUtilities");
  //******//
  gSystem->Setenv("DB_ACCESS_MODE","write");
  //******//  
  // Initialize db manager
  ///*
  StDbManager* mgr = StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig("Calibrations_trg");
  StDbTable* dbtable = node->addDbTable("triggerThreshold");
  // beginTime timestamp in MySQL format: "YYYY-MM-DD HH:mm:ss"
  ifstream intime(Form("beginTimes/%d.beginTimes.txt", runNumber));
  if(!intime){
    cout<<"can't open beginTime file"<<endl;
    return 0;
  }
  char date[10];
  char time[8];
  intime >> date >> time;
  TString storeTime(Form("%s %s", date, time));
  //******//
  //time stamp 2012-07-30 00:00:0X for test purposes
  //TString storeTime("2012-07-30 00:00:02");
  //******//
  mgr->setStoreTime(storeTime.Data());
 // */
  // Create your c-struct
  triggerThreshold_st table;
  
  // Fill structure with data 
  // sample setup for a single channel, please add more channels!
  strcpy(table.comments, Form("run%d triggerThreshold uploaded by zchang", runNumber)); 
  cout<<"comments set to "<<table.comments<<endl;

  TObjArray objarr = readOnline(runNumber);
  TBufferFile buf(TBuffer::kWrite);
  buf << &objarr;
  objarr.Delete();

  cout<<"Buffer size: "<<buf.BufferSize()<<endl;

  memset(table.trigthr, 0, buf.BufferSize());
  memcpy(table.trigthr, buf.Buffer(), buf.BufferSize());
  table.size = buf.BufferSize();

  //******//
  // Store data to the StDbTable
  dbtable->SetTable((char*)&table, 1);
  
  // uncomment next line to set "sim" flavor. "ofl" flavor is set by default, no need to set it.
  // dbtable->setFlavor("sim");
  
  // Store table to database
  cout<<"Storing Db table: "<< mgr->storeDbTable(dbtable) << endl;
  //******//

  ofstream out(Form("buffer/%d.trigthr.buffer.out", runNumber));
  assert(out);
  out.write(buf.Buffer(),buf.BufferSize());
  out.close();

  return 1;
}
TObjArray readOnline(int runNumber)
{
  // Open connection to online database
  const char* database = "mysql://db04.star.bnl.gov:3411/Conditions_rts?timeout=60";
  const char* user = "zchang";
  const char* pass = "";
  TMySQLServer* mysql = TMySQLServer::Connect(database,user,pass);

  if (!mysql) {
    cerr << "Connection to " << database << " failed" << endl;
    return;
  }

  TObjArray arr;
  TString query;
  TMySQLResult* result;
  //  TDatime beginTime;


  query = Form("select object,idx,reg,label,value,defaultvalue from `Conditions_rts`.`dict` where hash=(select dicthash from run where idx_rn = %d)",runNumber);

  result = (TMySQLResult*)mysql->Query(query);
  if (result) {
    TMySQLRow* row;
    while (row = (TMySQLRow*)result->Next()) {
      StTriggerThreshold* th = new StTriggerThreshold;
      th->object = atoi(row->GetField(0));
      th->index = atoi(row->GetField(1));
      th->reg = atoi(row->GetField(2));
      th->label = row->GetField(3);
      th->value = atoi(row->GetField(4));
      th->defaultvalue = atoi(row->GetField(5));
      delete row;
      arr.Add(th);
    }
    result->Close();
  }

  mysql->Close();

  for (int i = 0; i < arr.GetEntriesFast(); ++i) {
    StTriggerThreshold* th = (StTriggerThreshold*)arr.At(i);
    th->print();
  }
  return arr;
}
