void Db_ReadDriftMaps(char* dirname, char* timestamp, char* flavor){

//  Use this macro to read the FTPC drift map tables (ftpcDeflection,ftpcVDrift,ftpcdDeflectiondP,ftpcdVDriftdP)
//  from the Calibrations_ftpc database and to write them out into the directory dirname
//
//  The FTPC drift map tables are "flavored" tables
//  Before running this macro, create a sub-directory in your working directory for each of the 5 flavors:
//        flavor                 sub-directory 
//        ------                 ------------- 
//        ffp10kv               FullFieldPositive
//        hfp10kv               HalfFieldPositive
//        zf10kv                ZeroField
//        hfn10kv               HalfFieldNegative
//        ffn10kv               FullFieldNegative
//
//   Don't forget to
//   
//     setenv DB_ACCESS_MODE read
//
//   To dump the complete set of drift maps for a  specific timestamp run the following 5 jobs.
//   The timestamp in this example is for the 50% Ar,50% CO2 drift maps
//   The timestamp for the 50.3% Ar, 49.7% CO2 drift maps is "2005-11-01 00:00:00":
//   
//         root4star -b -q 'Db_ReadDriftMaps.C("FullFieldPositive","2001-11-04 00:00:00","ffp10kv")'
//         root4star -b -q 'Db_ReadDriftMaps.C("HalfFieldPositive","2001-11-04 00:00:00","hfp10kv")'
//         root4star -b -q 'Db_ReadDriftMaps.C("ZeroFieldPositive","2001-11-04 00:00:00","zf10kv")'
//         root4star -b -q 'Db_ReadDriftMaps.C("HalfFieldNegative","2001-11-04 00:00:00","hfn10kv")'
//         root4star -b -q 'Db_ReadDriftMaps.C("FullFieldNegative","2001-11-04 00:00:00","ffn10kv")'


  gSystem->Load("St_base"); // needed for StDbModifier
  gSystem->Load("StChain");
 
  // DB-specific libs
  gSystem->Load("libStDb_Tables");
  gSystem->Load("StUtilities"); // probably don't need this
  gSystem->Load("StDbLib");                                                     
  StDbManager* dbManager=StDbManager::Instance();
  StDbModifier* modify=new StDbModifier();
    modify->SetDbName("Calibrations_ftpc");
    modify->SetDateTime(timestamp);
    modify->SetFlavor(flavor);
                                                                                
  char* tableNames[4]= { "ftpcdVDriftdP","ftpcDeflection","ftpcVDrift","ftpcdDeflectiondP" };

  for(int i=0; i<4; i++){
    TString fname(dirname);
    fname+="/";
    fname+=tableNames[i];
    fname+=".C";
 
    modify->SetTableName(tableNames[i]);
                                                                               
    modify->SetOutputFileName(fname.Data());
//    mod->WriteDataToDB();
    modify->ReadDataFromDB();

    cout<<"Wrote out Calibrations_ftpc database table "<<fname.Data()<<endl;
  }
 
}

