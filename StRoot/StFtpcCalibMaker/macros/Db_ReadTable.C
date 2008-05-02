void Db_ReadTable(char* tableName, char* timestamp){

//  Use this macro to read an FTPC table from the Calibrations_ftpc database 
//  and write it out into your working directory
//
//   Don't forget to
//   
//     setenv DB_ACCESS_MODE read
//
//   For example:
//   
//         root4star -b -q 'Db_ReadTable.C("ftpcGas","2008-05-01 00:00:00")'

  gSystem->Load("St_base"); // needed for StDbModifier
  gSystem->Load("StChain");
 
  // DB-specific libs
  gSystem->Load("libStDb_Tables");
  gSystem->Load("StUtilities"); 
  gSystem->Load("StDbLib");                                                     
  StDbManager* dbManager=StDbManager::Instance();
  StDbModifier* modify=new StDbModifier();
    modify->SetDbName("Calibrations_ftpc");
    modify->SetDateTime(timestamp);
    modify->SetFlavor("ofl");
                                                                                
  TString fname(tableName);
    fname+=".C";
    
    modify->SetTableName(tableName);
    modify->SetOutputFileName(fname.Data());
    modify->ReadDataFromDB();

    cout<<"Wrote out Calibrations_ftpc database table "<<fname.Data()<<endl;
 
}

