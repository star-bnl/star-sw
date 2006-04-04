// $Id: lasertest.C,v 1.3 2006/04/04 12:02:30 jcs Exp $
//
// $Log: lasertest.C,v $
// Revision 1.3  2006/04/04 12:02:30  jcs
// simplify macro
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

void lasertest(TString filename,int ftpc, int lsec, int straight, int gfit, int minz, int maxz, int minrad, int maxrad,char* t0, char* gas,float mbfield)
{

  cout<<"Starting lasertest.C:"<<endl;
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
    
      laser->DbInit( mbfield);
  for (int i=-5;i<7;i++)
  //for (int i=0;i<10;i++)
    {
      //float step=i/10.0;
      //float step=0;int i=0;
      float step=i/10.0;
      for (int k=-5;k<8;k++)
	{
	  float step2=k/10.0;
	  //float step2=-0.0;int k=0.0;
	  char t[3];char g[3];
	  
	  sprintf(g,"%.2f",step2);
	  sprintf(t,"%.2f",step);
	  cout<<endl;
          cout<<"laser->DoLaserCalib: deltat0 = "<<t<<" und deltaGas = "<<g<<endl;
	  if (k==0 && i==0)
	    cout<<"Kommt zum Schluss !!!"<<endl;
	  else
	    laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,t,g,mbfield);
	}
    }

    //cout<<"deltat0 = "<<"0"<<" und deltaGas = "<<"0"<<endl;
  laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,"0.000001","0",mbfield);
  delete laser;
    } 
    else {
      cout<<"Macro will not work if both t0 and gas exactly = 0"<<endl;     
    }
}
