void t0_single(TString filename, char* t0, char* gas,float mbfield)
{


  cout<<"Starting t0_single.C:"<<endl;
  cout<<"               filename = "<<filename<<".root"<<endl;
  cout<<"               t0       = "<<t0<<endl;
  cout<<"               gas      = "<<gas<<endl;
  cout<<"               mbfield  = "<<mbfield<<endl;
  cout<<endl;

  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StUtilities");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StFtpcCalibMaker");
  gSystem->Load("libftpc_Tables");
  gSystem->Load("StFtpcClusterMaker");
  gSystem->Load("StMagF");

  if (atof(t0)!=0 || atof(gas)!=0)
    {
      //  Create the makers to be called by the current chain
      const char *mysqlDB =  "MySQL:StarDb";
      const char *paramsDB = "$STAR/StarDb";
      //const char *paramsDB = "$PWD/StarDb";
      
      StChain *chain =  new StChain();

      StFtpcCalibMaker *laser=new StFtpcCalibMaker();
      laser->GetRunInfo(filename);
      
      St_db_Maker *dbMk = new St_db_Maker("db",mysqlDB,paramsDB);
      dbMk->SetDateTime(laser->Date(),laser->Time());
      
      dbMk->Init();
      dbMk->Make();
      
      cout<<"After Database init !!!"<<endl;
    }
  
  cout<<endl;
  cout<<"Starting StFtpcCalibMaker ..."<<endl;
  cout<<endl;

  if (atof(t0)!=0 || atof(gas)!=0)
    laser->DbInit(mbfield);
  
  laser->DoT0Calib(filename,t0,gas,mbfield);

  delete laser;
}
