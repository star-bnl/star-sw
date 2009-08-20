void Db_LoadTable(char* domainName, char* tableName, char* timestamp){

//  Use this macro to load the FTPC table "tableName" from your working directory 
//  into the "domainName" database, Calibrations_ftpc or Geometry_ftpc 
//
//   Don't forget to
//   
//     setenv DB_ACCESS_MODE write
//
//   For example:
//   
//         root4star -b -q 'Db_LoadTable.C("Calibrations_ftpc","ftpcGas","2008-05-01 00:00:00")'

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
  
    modify->SetInputFileName(fname.Data());
    modify->WriteDataToDB();

    cout<<"Loaded "<<fname.Data()<<" into the "<<domainName<<" database"<<endl;
 
}

