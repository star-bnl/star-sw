int write_trigger_definitions(int runNumber = 13078009)
{
  // Load all required libraries
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
  //gSystem->Load("St_base.so");
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
  StDbManager* mgr = StDbManager::Instance();
  StDbConfigNode* node = mgr->initConfig("Calibrations_trg");
  StDbTable* dbtable = node->addDbTable("triggerDefinition");
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
  mgr->setStoreTime(storeTime.Data());
  
  // Create your c-struct
  triggerDefinition_st table;
  
  // Fill structure with data 
  // sample setup for a single channel, please add more channels!
  strcpy(table.comments, Form("run%d triggerDefinition uploaded by zchang", runNumber)); 
  cout<<"comments set to "<<table.comments<<endl;

  TObjArray objarr = readOnline(runNumber);
  TBufferFile buf(TBuffer::kWrite);
  buf << &objarr;
  objarr.Delete();

  cout<<"Buffer size: "<<buf.BufferSize()<<endl;

  memset(table.trigdef, 0, buf.BufferSize());
  memcpy(table.trigdef, buf.Buffer(), buf.BufferSize());
  table.size = buf.BufferSize();

  //******//
  // Store data to the StDbTable
  dbtable->SetTable((char*)&table, 1);
  
  // uncomment next line to set "sim" flavor. "ofl" flavor is set by default, no need to set it.
  // dbtable->setFlavor("sim");
  
  // Store table to database
  cout<<"Storing Db table: "<< mgr->storeDbTable(dbtable) << endl;
  //******//

  ofstream out(Form("buffer/%d.trigdef.buffer.out", runNumber));
  assert(out);
  out.write(buf.Buffer(),buf.BufferSize());
  out.close();

  return 1;
}
TObjArray readOnline(int runNumber)
{
  TObjArray array;

  // Open connection to online database
  const char* database = "mysql://db04.star.bnl.gov:3411/Conditions_rts?timeout=60";
  const char* user = "zchang";
  const char* pass = "";
  TMySQLServer* mysql = TMySQLServer::Connect(database,user,pass);

  if (!mysql) {
    cerr << "Connection to " << database << " failed" << endl;
    return;
  }

  TString query;
  TMySQLResult* result;

  query = Form("select idx_trigger,name,offlineBit from `Conditions_rts`.`triggers` where idx_rn = %d",runNumber);

  result = (TMySQLResult*)mysql->Query(query);
  if (result) {
    TMySQLRow* row;
    while (row = (TMySQLRow*)result->Next()) {
      int triggerIndex = atoi(row->GetField(0));
      StTriggerDefinition* def = new StTriggerDefinition;
      def->triggerIndex = triggerIndex;
      def->name = row->GetField(1);
      def->triggerId = atoi(row->GetField(2));
      array.AddAtAndExpand(def,triggerIndex);
      delete row;
    }
    result->Close();
  }

  query = Form("select idx_idx,onbits,offbits,onbits1,onbits2,onbits3,offbits1,offbits2,offbits3 from `Conditions_rts`.`pwc` where idx_rn = %d",runNumber);

  result = (TMySQLResult*)mysql->Query(query);
  if (result) {
    TMySQLRow* row;
    while (row = (TMySQLRow*)result->Next()) {
      int triggerIndex = atoi(row->GetField(0));
      StTriggerDefinition* def = (StTriggerDefinition*)array.At(triggerIndex);
      if (def) {
        sscanf(row->GetField(1), "%ud", &def->onbits);
        sscanf(row->GetField(2), "%ud", &def->offbits);
        sscanf(row->GetField(3), "%ud", &def->onbits1);
        sscanf(row->GetField(4), "%ud", &def->onbits2);
        sscanf(row->GetField(5), "%ud", &def->onbits3);
        sscanf(row->GetField(6), "%ud", &def->offbits1);
        sscanf(row->GetField(7), "%ud", &def->offbits2);
        sscanf(row->GetField(8), "%ud", &def->offbits3);
        //cout<<hex<<def->onbits<<" "<<def->offbits<<endl;
      }
      delete row;
    }
    result->Close();
  }

  mysql->Close();

  array.Compress();

  for (int triggerIndex = 0; triggerIndex < array.GetEntriesFast(); ++triggerIndex) {
    StTriggerDefinition* def = (StTriggerDefinition*)array.At(triggerIndex);
    if (def) def->print();
  }
  return array;
  /*
  TBufferFile buf(TBuffer::kWrite);
  buf << &a;
  array.Delete();

  cout << "Serialized array of " << buf.BufferSize() << " : " << buf.Buffer() << endl;

  ofstream out("test.out");
  assert(out);
  out.write(buf.Buffer(),buf.BufferSize());
  out.close();
  */
}
