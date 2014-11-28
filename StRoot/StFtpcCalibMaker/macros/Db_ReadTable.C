void Db_ReadTable(char* domainName, char* tableName, char* timestamp){

//  Use this macro to read the FTPC table "tableName" from the "domainName" database, 
//  Calibrations_ftpc or Geometry_ftpc, and write it out into your working directory
//
//   Don't forget to
//   
//     setenv DB_ACCESS_MODE read
//
//   For example:
//   
//         root4star -b -q 'Db_ReadTable.C("Calibrations_ftpc","ftpcGas","2008-05-01 00:00:00")'

  gSystem->Load("St_base"); // needed for StDbModifier
  gSystem->Load("StChain");
 
  // DB-specific libs
  gSystem->Load("libStDb_Tables");
  gSystem->Load("StUtilities"); 
  gSystem->Load("StDbLib");                                                     
  StDbManager* dbManager=StDbManager::Instance();
  StDbModifier* modify=new StDbModifier();
    modify->SetDbName(domainName);
    modify->SetDateTime(timestamp);
    modify->SetFlavor("ofl");
                                                                                
  TString fname(tableName);
    fname+=".C";
    
    modify->SetTableName(tableName);
    modify->SetOutputFileName(fname.Data());
    modify->ReadDataFromDB();

    cout<<"Wrote out "<<domainName<<" database table "<<fname.Data()<<endl;
 
}

