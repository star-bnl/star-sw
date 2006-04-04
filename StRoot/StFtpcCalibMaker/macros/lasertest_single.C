// $Id: lasertest_single.C,v 1.3 2006/04/04 11:17:15 jcs Exp $
//
// $Log: lasertest_single.C,v $
// Revision 1.3  2006/04/04 11:17:15  jcs
// simplify macro
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

void lasertest_single(TString filename,int ftpc, int lsec, int straight, int gfit, int minz, int maxz, int minrad, int maxrad,char* t0, char* gas, float mbfield)
{

  cout<<"Starting lasertest_single.C:"<<endl;
  cout<<"                            filename = "<<filename<<".root"<<endl;
  cout<<"                            ftpc     = "<<ftpc<<endl;
  cout<<"                            lsec     = "<<lsec<<endl;
  cout<<"                            straight = "<<straight<<endl;
  cout<<"                            gfit     = "<<gfit<<endl;
  cout<<"                            minz     = "<<minz<<endl;
  cout<<"                            maxz     = "<<maxz<<endl;
  cout<<"                            minrad   = "<<minrad<<endl;
  cout<<"                            maxrad   = "<<maxrad<<endl;
  cout<<"                            t0       = "<<t0<<endl;
  cout<<"                            gas      = "<<gas<<endl;
  cout<<"                            mbfield  = "<<mbfield<<endl;
  cout<<endl;

  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");

  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libtpc_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");

  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbBroker.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("St_db_Maker.so");
  gSystem->Load("StTpcDb");

  gSystem->Load("StFtpcCalibMaker");
  gSystem->Load("libftpc_Tables");
  gSystem->Load("StFtpcClusterMaker");
  gSystem->Load("StFtpcTrackMaker");

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

      laser->DbInit(mbfield);
  
      laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,t0,gas,mbfield);

      delete laser;
  }
  else
  {
    cout<<"Macro will not work if both t0 and gas exactly = 0"<<endl;
  }
}
