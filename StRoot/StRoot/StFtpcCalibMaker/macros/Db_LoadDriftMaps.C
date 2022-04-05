void Db_LoadDriftMaps(char* dirname, char* timestamp, char* flavor){

//  Use this macro to load the 5 sets of the 4  FTPC "flavored" drift map tables 
//  (ftpcDeflection,ftpcVDrift,ftpcdDeflectiondP,ftpcdVDriftdP)
//  to the Calibrations_ftpc database
//
//  There should be 5 sub-directories in your working directory, one for each of the 5 flavors.
//  The drift map tables are produced by the StFtpcDriftMapMaker
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
//     setenv DB_ACCESS_MODE write
//
//   To load the complete set of drift maps for a  specific timestamp run the following 5 jobs.
//   The timestamp in this example is for the 50% Ar,50% CO2 drift maps which should be
//   used again starting with the 2007/2008 dAu run
//   The timestamp used here is "2007-11-01 00:00:00"
//
//         root4star -b -q 'Db_LoadDriftMaps.C("FullFieldPositive","2007-11-01 00:00:00","ffp10kv")'
//         root4star -b -q 'Db_LoadDriftMaps.C("HalfFieldPositive","2007-11-01 00:00:00","hfp10kv")'
//         root4star -b -q 'Db_LoadDriftMaps.C("ZeroFieldPositive","2007-11-01 00:00:00","zf10kv")'
//         root4star -b -q 'Db_LoadDriftMaps.C("HalfFieldNegative","2007-11-01 00:00:00","hfn10kv")'
//         root4star -b -q 'Db_LoadDriftMaps.C("FullFieldNegative","2007-11-01 00:00:00","ffn10kv")'


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
                                                                               
    modify->SetInputFileName(fname.Data());
    modify->WriteDataToDB();

    cout<<"Loaded "<<fname.Data()<<" to database"<<endl;
  }
 
}

