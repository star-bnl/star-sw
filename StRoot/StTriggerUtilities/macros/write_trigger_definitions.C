void write_trigger_definitions(int runNumber = 10180030)
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

  TString query;
  TMySQLResult* result;
  StTriggerDefinition trgdefs[32];

  query = Form("select idx_trigger,name,offlineBit from `Conditions_rts`.`triggers` where idx_rn = %d",runNumber);

  result = (TMySQLResult*)mysql->Query(query);
  if (result) {
    TMySQLRow* row;
    while (row = (TMySQLRow*)result->Next()) {
      int triggerIndex = atoi(row->GetField(0));
      assert(0 <= triggerIndex && triggerIndex < 32);
      trgdefs[triggerIndex].triggerIndex = triggerIndex;
      trgdefs[triggerIndex].name = row->GetField(1);
      trgdefs[triggerIndex].triggerId = atoi(row->GetField(2));
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
      assert(0 <= triggerIndex && triggerIndex < 32);
      trgdefs[triggerIndex].onbits   = atoi(row->GetField(1));
      trgdefs[triggerIndex].offbits  = atoi(row->GetField(2));
      trgdefs[triggerIndex].onbits1  = atoi(row->GetField(3));
      trgdefs[triggerIndex].onbits2  = atoi(row->GetField(4));
      trgdefs[triggerIndex].onbits3  = atoi(row->GetField(5));
      trgdefs[triggerIndex].offbits1 = atoi(row->GetField(6));
      trgdefs[triggerIndex].offbits2 = atoi(row->GetField(7));
      trgdefs[triggerIndex].offbits3 = atoi(row->GetField(8));
      delete row;
    }
    result->Close();
  }

  mysql->Close();

  TObjArray a;

  for (int triggerIndex = 0; triggerIndex < 32; ++triggerIndex) {
    if (!trgdefs[triggerIndex].name.IsNull()) {
      trgdefs[triggerIndex].print();
      a.Add(&trgdefs[triggerIndex]);
    }
  }

  TBufferFile buf(TBuffer::kWrite);
  buf << &a;

  cout << "Serialized array of " << buf.BufferSize() << " : " << buf.Buffer() << endl;

  ofstream out("test.out");
  assert(out);
  out.write(buf.Buffer(),buf.BufferSize());
  out.close();
}
