//
// $Id: StMixer.C,v 1.1 2000/02/16 22:13:46 pfachini Exp $
//
// Description:
// Read in a .trs and a data file for the Mixer
//
///////////////////////////////////////////////////////
//
// $Log: StMixer.C,v $
// Revision 1.1  2000/02/16 22:13:46  pfachini
// maro to run StMixerMaker
//
//
////////////////////////////////////////////////////////
TBrowser *b = 0;
class StChain;
StChain  *chain=0;
class St_DataSet;
St_DataSet *Event;
void Load(){
  gSystem->Load("St_base");
  gSystem->Load("libm");
  gSystem->Load("StUtilities");
  gSystem->Load("StChain");
  gSystem->Load("xdf2root");
  gSystem->Load("St_Tables");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StDaqLib");
  gSystem->Load("StDAQMaker");
  //gSystem->Load("StTpcDb");
  gSystem->Load("StTrsMaker");
  gSystem->Load("StMixerMaker");
  gSystem->Load("libmsg");
  gSystem->Load("libtls");
  gSystem->Load("StIOMaker");
  //gSystem->Load("geometry");
}
void StMixer(const Int_t Nevents=1, 
	     const Char_t *kind1="daq",
	     const Char_t *file1 ="/disk1/star/daq/st_physics_0003373_raw_0001.daq",
             //const Char_t *file1 ="trs_muon_10cmdrift_good.trs",
	     //const Char_t *file1 ="hij_1evt1.trs",
	     const Char_t *kind2="trs",
	     const Char_t *file2 ="/star/u2c/pfachini/workdir/Mixer/Pion1Evt.trs")
	     //const Char_t *file2 ="hij_1evt1.trs")
             //const Char_t *file2 ="/disk1/star/daq/st_physics_0003373_raw_0001.daq")
             //const Char_t *file2 ="trs_muon_10cmdrift_good.trs")
{
  if (gClassTable->GetID("StChain") < 0) Load();
  chain = new StChain("mixer");
  chain->SetDebug();

  //set I/O for crs
  //StIOMaker *ioMk = new StIOMaker();
  //ioMk->SetFile(daqfile);
  //ioMk->SetDebug(2);
  
  //  Create the makers to be called by the current chain

  //  ROOT Db
  const char* mainDB = "$STAR/StDb";
  St_db_Maker *dbMk = new St_db_Maker("db",mainDB);
  //dbMk->SetDebug();
  chain->SetInput("params","db:StDb/params");

  const char* calibDB = "$STAR_ROOT/calib";
  St_db_Maker *calibMk = new St_db_Maker("calib",calibDB);
  chain->SetInput("calib","calib:calib");
  //calibMk->SetDebug();

  //  MySQL DB
  //const char *mainDB = "MySQL:Geometry";
  //St_db_Maker *dbMk = new St_db_Maker("Geometry",mainDB);
  //dbMk->SetDebug();
  //dbMk->Init();
  //dbMk->GetDataBase("Geometry/tpc");
  
  //const char *calibDB = "MySQL:Calib";
  //St_db_Maker *calibMk = new St_db_Maker("Calib",calibDB);
  //calibMk->SetDebug();
  //calibMk->Init();
  //calibMk->GetDataBase("Calibrations/tpc");
  //StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");
  //tpcDbMk->Init();
  //tpcDbMk->Make();
  //cout << "The Db: " << gStTpcDb << endl;

  // Trs
  if (!strcmp(kind1,"trs")) {
    StTrsMaker    *trs_first = new StTrsMaker("TrsFirst");
    trs_first->SetDebug();
    trs_first->readFile(file1);
  }
  if (!strcmp(kind2,"trs")) {
    StTrsMaker    *trs_second = new StTrsMaker("TrsSecond");
    trs_second->SetDebug();
    trs_second->readFile(file2);
  }

  // Data
  if (!strcmp(kind1,"daq")) {
    TString InFile(file1);
    gSystem->ExpandPathName(InFile);
    StDAQMaker    *tpc_first = new StDAQMaker("DaqFirst",InFile);
  }
  if (!strcmp(kind2,"daq")) {
    TString InFile(file2);
    gSystem->ExpandPathName(InFile);
    StDAQMaker    *tpc_second = new StDAQMaker("DaqSecond",InFile);
  }

  // Mixer
  StMixerMaker  *mixer = new StMixerMaker("Mixer",kind1,kind2);
  mixer->writeFile("mixer.trs",Nevents);

  // Init the main chain and all its makers
  int iInit = chain->Init();
  if (iInit) chain->Fatal(iInit,"on init");
  gBenchmark->Start("mixer");
  
  for (Int_t i =1; i <= Nevents; i++){
    if (chain->Make(i)) break;
    
    if (i != Nevents) chain->Clear();
    printf ("===========================================\n");
    printf ("=========================================== Done with Event no. %d\n",i);
    printf ("===========================================\n");
  }
  if (Nevents > 1) {
    chain->Finish();
    gBenchmark->Print("mixer");
  }
  else b = new TBrowser;

}
