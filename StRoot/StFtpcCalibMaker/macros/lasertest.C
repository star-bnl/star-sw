// $Id: lasertest.C,v 1.8 2009/08/04 08:42:23 jcs Exp $
//
// $Log: lasertest.C,v $
// Revision 1.8  2009/08/04 08:42:23  jcs
// The 'perfect' gain table and adjustAverageWest = adjustAverageEast = 0.0
// are used for laser run calibration
//
// Revision 1.7  2008/05/14 21:46:01  jcs
// remove minz,maxz,minrad,maxrad from argument list and set values in macro
//
// Revision 1.6  2008/04/23 19:42:18  jcs
// load the libStDb_Tables.so and StDetectorDbMaker.so which are needed as of STAR version SL07d
//
// Revision 1.5  2006/08/02 11:02:34  jcs
// add comments
// remove commented out old code
//
// Revision 1.4  2006/04/05 08:50:36  jcs
// set t0 = ".000001" if t0 = "0" and gas = "0" to avoid seg fault
//
// Revision 1.3  2006/04/04 12:02:30  jcs
// simplify macro
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

void lasertest(TString filename,int ftpc, int lsec, int straight, int gfit,char* t0, char* gas,float mbfield)
{

  Int_t minz, maxz;
  Int_t minrad = 0;
  Int_t maxrad = 30;
  cout<<"Starting lasertest.C:"<<endl;
  cout<<"                            filename = "<<filename<<".root"<<endl;
  cout<<"                            ftpc     = "<<ftpc;
  if ( ftpc == 1 ) cout<<" FTPC West"<<endl;
  if ( ftpc == 2 ) cout<<" FTPC East"<<endl;
  cout<<"                            lsec     = "<<lsec<<endl;
  cout<<"                            straight = "<<straight<<endl;
  cout<<"                            gfit     = "<<gfit<<endl;
  // for FTPC West
  if ( ftpc == 1 ) {
     minz = 0;
     maxz = 300;
  }
  // for FTPC East
  if ( ftpc == 2 ) {
     minz = -300;
     maxz = 0;
  }
  cout<<"                            minz     = "<<minz<<endl;
  cout<<"                            maxz     = "<<maxz<<endl;
  cout<<"                            minrad   = "<<minrad<<endl;
  cout<<"                            maxrad   = "<<maxrad<<endl;
// if both t0 and gas = "0", set t0=".000001" otherwise program will seg fault
  if (atof(t0)==0 && atof(gas)==0) {
     t0 = ".000001";
     cout<<"     changed t0=0 to        t0       = "<<t0<<" to avoid seg fault"<<endl;
  } else
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

  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDetectorDbMaker.so");
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

      Bool_t laserRun = kTRUE;
      dbMk->setLaserRun(laserRun);
      
      dbMk->Init();
      dbMk->Make();
      
      cout<<"dbDate = "<<dbMk->GetDateTime().GetDate()<<endl;
      cout<<"After Database init !!!"<<endl;
      cout<<endl;
    
      if (laser->DbInit(mbfield) == kStWarn) {
         delete laser;
         break;
      }

  // Interation over tzero
  for (int i=-5;i<7;i++)
    {
      float step=i/10.0;
      
      // Interation over gas composition
      for (int k=-5;k<8;k++)
	{
	  float step2=k/10.0;
	  char t[3];char g[3];
	  
	  sprintf(t,"%.2f",step);
	  sprintf(g,"%.2f",step2);
	  cout<<endl;
          cout<<"laser->DoLaserCalib: deltat0 = "<<t<<" und deltaGas = "<<g<<endl;
	  if (k==0 && i==0)
	    cout<<"Kommt zum Schluss !!!"<<endl;
	  else
	    laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,t,g,mbfield);
	}
    }

  laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,"0.000001","0",mbfield);

  delete laser;
}
