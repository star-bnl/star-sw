void Db_LoadTable(char* tableName, char* timestamp){

//  Use this macro to load an FTPC table from your working directory 
//  into the Calibrations_ftpc database 
//
//   Don't forget to
//   
//     setenv DB_ACCESS_MODE write
//
//   For example:
//   
//         root4star -b -q 'Db_LoadTable.C("ftpcGas","2008-05-01 00:00:00")'

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
  
    modify->SetInputFileName(fname.Data());
    modify->WriteDataToDB();

    cout<<"Loaded "<<fname.Data()<<" into the Calibrations_ftpc database"<<endl;
 
}

