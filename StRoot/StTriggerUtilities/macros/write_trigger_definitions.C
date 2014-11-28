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
  TObjArray a;

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
      a.AddAtAndExpand(def,triggerIndex);
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
      StTriggerDefinition* def = (StTriggerDefinition*)a.At(triggerIndex);
      if (def) {
	def->onbits   = atoi(row->GetField(1));
	def->offbits  = atoi(row->GetField(2));
	def->onbits1  = atoi(row->GetField(3));
	def->onbits2  = atoi(row->GetField(4));
	def->onbits3  = atoi(row->GetField(5));
	def->offbits1 = atoi(row->GetField(6));
	def->offbits2 = atoi(row->GetField(7));
	def->offbits3 = atoi(row->GetField(8));
      }
      delete row;
    }
    result->Close();
  }

  mysql->Close();

  a.Compress();

  for (int triggerIndex = 0; triggerIndex < a.GetEntriesFast(); ++triggerIndex) {
    StTriggerDefinition* def = (StTriggerDefinition*)a.At(triggerIndex);
    if (def) def->print();
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
