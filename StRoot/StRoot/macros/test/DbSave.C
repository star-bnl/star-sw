void DbSave(){

  // Baseline shared libraries
  gSystem->Load("libTable");
  gSystem->Load("StUtilities.so");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");

  // DB-specific libs

  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker"); 
  gSystem->Load("St_db_Maker");

  // create makers connecting to databases RunParams & Geometry

  St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
//  dbMk->SetFlavor("sim+ofl");
  dbMk->SetFlavor("sim");
//  dbMk->SetFlavor("ofl");
 
  dbMk->Init();

  // Make reaquests for data

  // choose timestamp 
  TDataSet *db = dbMk->Find(".data/StarDb");
//  db->ls(2);

  dbMk->SetDateTime(19990101,10000); 	//Request Time
  TDatime newTime(20020101,0); 		//Replace Time


  TDataSetIter next(db,1);
  TDataSet *ds=0,*p=0;
  while ((ds=next())) 
  { 
    printf("NAME: %s\n",ds->GetName());
    dbMk->Save(ds->GetName(),&newTime);
  }

}







