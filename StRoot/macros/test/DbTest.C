void DbTest(){
  gSystem->Load("St_base"); 
  gSystem->Load("StUtilities");  
  gSystem->Load("StChain");  
  StChain* mchain = new StChain("StDbTest");
  gSystem->Load("StDbLib.so"); 
  gSystem->Load("StDbMaker.so");  
  gSystem->Load("StTpcDb.so"); 
//
// Construct with configuration="dbtest1"
// that is a key in the StarDb::StarDbKeys table
//
  StDbMaker* mk = new StDbMaker("TrsES99","StarDb");
  mk->SetTime(1);
  mk->Init();
  mk->Make();
//
// could do this with an St_DataSetIter....
//
  St_DataSet* set2 = mk->GetData("StarDb");
  St_DataSet* set3 = set2->Find("Calibrations"); 
  St_DataSet* set4 = set3->Find("tpc");
  St_DataSet* set5 = set4->Find("tpcSlowControlSim");
//
// for purpose of handling with St_DataSet,
// StDb_tpcDriftVelocity inherits from TObject
// & can be put into an St_ObjectSet..
//
  StDbTableI* table=((StDbDataSet*)set5)->GetDbTable();
//
// any user code must compile with the header file
// tpcSlowControlSim* tpv = (tpcSlowControlSim*)table->GetTable();
//
  //
  // Or write to an ascii file
  // 
ofstream of("mytpcSlowControlSim.xml");
StDbXmlWriter* writer = new StDbXmlWriter(of);
writer->streamTableName(table->getTableName());

 // optional ... provides the db-address

writer->streamAccessor();
table->StreamAccessor(writer); 
writer->endAccessor();

table->dbStreamer(writer);
writer->streamEndTableName();

}







