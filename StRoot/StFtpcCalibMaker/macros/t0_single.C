// $Id: t0_single.C,v 1.5 2009/12/09 14:41:49 jcs Exp $
//
// $Log: t0_single.C,v $
// Revision 1.5  2009/12/09 14:41:49  jcs
// delta_t0 and delta_gas can now both = 0
//
// Revision 1.4  2008/05/15 21:05:55  jcs
// load StDetectorDbMaker.so
//
// Revision 1.3  2006/04/05 08:50:36  jcs
// set t0 = ".000001" if t0 = "0" and gas = "0" to avoid seg fault
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

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
  gSystem->Load("StDetectorDbMaker.so");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StFtpcCalibMaker");
  gSystem->Load("libftpc_Tables");
  gSystem->Load("StFtpcClusterMaker");
  gSystem->Load("StMagF");

  //  Create the makers to be called by the current chain
  const char *mysqlDB =  "MySQL:StarDb";
  const char *paramsDB = "$STAR/StarDb";
  //const char *paramsDB = "$PWD/StarDb";
      
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

    if (laser->DbInit(mbfield) == kStWarn) {
     delete laser;
     break;
  }
  
  laser->DoT0Calib(filename,t0,gas,mbfield);

  delete laser;
}
