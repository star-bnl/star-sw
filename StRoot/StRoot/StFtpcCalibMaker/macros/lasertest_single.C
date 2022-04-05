// $Id: lasertest_single.C,v 1.11 2011/05/11 11:55:46 jcs Exp $
//
// $Log: lasertest_single.C,v $
// Revision 1.11  2011/05/11 11:55:46  jcs
// had to change order of library loading to avoid undefined symbols
//
// Revision 1.10  2009/12/09 14:41:49  jcs
// delta_t0 and delta_gas can now both = 0
//
// Revision 1.9  2009/10/14 15:58:43  jcs
// change and add macros so that in addition to varying t0 and the gas compostion,
// the gas temperature can be varied
//
// Revision 1.8  2009/09/12 14:45:48  jcs
// For ROOT 5.22.00 must load libMinuit.so and libSpectrum.so
//
// Revision 1.7  2009/08/04 08:42:23  jcs
// The 'perfect' gain table and adjustAverageWest = adjustAverageEast = 0.0
// are used for laser run calibration
//
// Revision 1.6  2008/05/14 21:46:01  jcs
// remove minz,maxz,minrad,maxrad from argument list and set values in macro
//
// Revision 1.5  2008/04/23 19:42:18  jcs
// load the libStDb_Tables.so and StDetectorDbMaker.so which are needed as of STAR version SL07d
//
// Revision 1.4  2006/04/05 08:50:36  jcs
// set t0 = ".000001" if t0 = "0" and gas = "0" to avoid seg fault
//
// Revision 1.3  2006/04/04 11:17:15  jcs
// simplify macro
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

void lasertest_single(TString filename,int ftpc, int lsec, int straight, int gfit, char* t0, char* gas, float gastemp, float mbfield)
{

  Int_t minz, maxz;
  Int_t minrad = 0;
  Int_t maxrad = 30;

  cout<<"Starting lasertest_single.C:"<<endl;
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

  gSystem->Load("StStarLogger");
  gSystem->Load("StUtilities");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StarMagField");
  gSystem->Load("StMagF");

  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDetectorDbMaker.so");  
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbBroker.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("St_db_Maker.so");

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
     cout<<" run = "<<laser->RunNum()<<" date = "<<laser->Date()<<" time = "<<laser->Time()<<endl;
      
      St_db_Maker *dbMk = new St_db_Maker("db",mysqlDB,paramsDB);
      dbMk->SetDateTime(laser->Date(),laser->Time());

      Bool_t laserRun = kTRUE; 

      dbMk->InitRun(laser->RunNum());
      dbMk->Init();
      dbMk->Make();
      
      cout<<"dbDate = "<<dbMk->GetDateTime().GetDate()<<endl;
      cout<<"After Database init !!!"<<endl;
      cout<<endl;

      if (laser->DbInit(mbfield) == kStWarn) {
         delete laser;
         break;
      }
  
      laser->DoLaserCalib(filename,ftpc,lsec,straight,gfit,minz,maxz,minrad,maxrad,t0,gas,gastemp,mbfield);

      delete laser;
}
