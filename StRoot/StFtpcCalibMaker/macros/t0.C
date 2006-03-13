void t0(TString filename, char* t0, char* gas,float mbfield)
{

  
  cout<<"Starting t0.C:"<<endl;
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
      cout<<" date = "<<laser->Date()<<" time = "<<laser->Time()<<endl;

      St_db_Maker *dbMk = new St_db_Maker("db",mysqlDB,paramsDB);
      dbMk->SetDateTime(laser->Date(),laser->Time());
      
      dbMk->Init();
      dbMk->Make();
      
      cout<<"dbDate = "<<dbMk->GetDateTime().GetDate()<<endl;
      cout<<"After Database init !!!"<<endl;
      cout<<endl;
    }
  
  cout<<"Starting StFtpcCalibMaker ..."<<endl;
  cout<<endl;


  if (atof(t0)!=0 || atof(gas)!=0)
    laser->DbInit(mbfield);
  
  for (int i=-10;i<11;i++)
    {

      float step=i/10.0;
      char t[3];

      sprintf(t,"%.2f",step);

      if (i!=0)
	laser->DoT0Calib(filename,t,"0",mbfield);

    }

  // for i == 0
  laser->DoT0Calib(filename,"0.000001","0",mbfield);

  delete laser;
}
