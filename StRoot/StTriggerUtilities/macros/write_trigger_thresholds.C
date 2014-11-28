void write_trigger_thresholds(int runNumber = 10180030)
{
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

  // Open connection to Run 9 online database
  const char* database = "mysql://dbbak.starp.bnl.gov:3408/Conditions_rts?timeout=60";
  const char* user = "";
  const char* pass = "";
  TMySQLServer* mysql = TMySQLServer::Connect(database,user,pass);

  if (!mysql) {
    cerr << "Connection to " << database << " failed" << endl;
    return;
  }

  TObjArray a;
  TString query;
  TMySQLResult* result;
  TDatime beginTime;

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
      a.Add(th);
    }
    result->Close();
  }

  mysql->Close();

  for (int i = 0; i < a.GetEntriesFast(); ++i) {
    StTriggerThreshold* th = (StTriggerThreshold*)a.At(i);
    th->print();
  }

  TBufferFile buf(TBuffer::kWrite);
  buf << &a;
  a.Delete();

  cout << "Serialized array of " << buf.BufferSize() << " : " << buf.Buffer() << endl;

  ofstream out("test.out");
  assert(out);
  out.write(buf.Buffer(),buf.BufferSize());
  out.close();
}
