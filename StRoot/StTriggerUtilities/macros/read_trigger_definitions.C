void read_trigger_definitions()
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

  ifstream in("test.out");
  assert(in);
  in.seekg(0,ios::end);
  streampos length = in.tellg();
  in.seekg(0,ios::beg);
  char* buffer = new char[length];
  in.read(buffer,length);
  in.close();
  TBufferFile buf(TBuffer::kRead);
  buf.SetBuffer(buffer,length,kFALSE);

  TObjArray* a = 0;
  buf >> a;
  a->Print();

  cout << "Total entries: " << a->GetEntriesFast() << endl;

  for (int i = 0; i < a->GetEntriesFast(); ++i) {
    StTriggerDefinition* trgdef = (StTriggerDefinition*)a->At(i);
    trgdef->print();
  }
}
