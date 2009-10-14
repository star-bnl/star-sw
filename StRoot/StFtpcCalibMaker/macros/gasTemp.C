// $Id: gasTemp.C,v 1.1 2009/10/14 15:58:43 jcs Exp $
//
// $Log: gasTemp.C,v $
// Revision 1.1  2009/10/14 15:58:43  jcs
// change and add macros so that in addition to varying t0 and the gas compostion,
// the gas temperature can be varied
//
//

void gasTemp(TString filename,int ftpc, int lsec, int straight, int gfit,char* t0, char* gas,float gastemp,float mbfield)
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
  cout<<"                            gastemp  = "<<gastemp<<endl;
  cout<<"                            mbfield  = "<<mbfield<<endl;
  cout<<endl;

  if (gClassTable->GetID("TTable") < 0) gSystem->Load("libStar");

  gSystem->Load("libMinuit.so");
  gSystem->Load("libSpectrum.so");

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
      
      dbMk->Init();
      dbMk->Make();
      
      cout<<"dbDate = "<<dbMk->GetDateTime().GetDate()<<endl;
      cout<<"After Database init !!!"<<endl;
      cout<<endl;
    
      if (laser->DbInit(mbfield) == kStWarn) {
         delete laser;
         break;
      }

  // Interation over temperature
  for (gastemp = -2; gastemp<4.5; gastemp+=0.5)
    {
      // Interation over gas composition
      for (int k=-5;k<8;k++)
	{
	  float step2=k/10.0;
	  //char t[3];char g[3];
          char g[3]; 
	  
	  //sprintf(T,"%.2f",step);
	  sprintf(g,"%.2f",step2);
	  cout<<endl;
          cout<<"laser->DoLaserCalib: deltaT = "<<gastemp<<" und deltaGas = "<<g<<endl;
	  if (k==0 && gastemp==0)
	    cout<<"Comes at the end !!!"<<endl;
	  else
	    laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,t0,g,gastemp,mbfield);
	}
    }

  laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,"0.000001","0",0,mbfield);

  delete laser;
}
