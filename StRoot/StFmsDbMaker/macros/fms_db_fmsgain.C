#include <iostream.h> 
#include <fstream.h>

//void fms_db_fmsgain(char* opt = "readdb writetext", char* dataspec="fms_gain.txt") {
void fms_db_fmsgain(char* opt = "readtext writedb", char* dataspec="FmsPtGainR16.txt") {
  // that's begin time for validity range for WRITING TO DB 
  // your data will be available from storeTime till 2037
  TString data(dataspec);
  if(data.Contains("run8") && data.Contains("dAu200"))
    TString storeTime = "2007-11-09 12:00:00";
  else
    if(data.Contains("run8") && data.Contains("pp200"))
      TString storeTime = "2008-01-28 12:00:00";
    else
      if(data.Contains("run9") && data.Contains("pp200"))  
	TString storeTime = "2009-01-16 00:00:00";
      else std::cout<<"Invalid year range"<<std::endl;

  //TString storeTime = "2014-12-20 00:00:00";
  TString storeTime = "2015-01-01 00:00:00";
  // this is data and time for READING FROM DB
  //int readDate = 20141220;
  int readDate = 20150101;
//  int readDate = 20130222;
  int readTime = 0;

  TString option(opt);
  std::cout << "Opt =" << opt << "\n";
  std::cout << "testinput = " << option.Contains("testinput")  << "\n";
  std::cout << "readtext  = " << option.Contains("readtext")  << "\n";
  std::cout << "readdb    = " << option.Contains("readdb")    << "\n";
  std::cout << "writedb   = " << option.Contains("writedb")   << "\n";
  std::cout << "writetext = " << option.Contains("writetext") << "\n";
  
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // max index dimensions
   const Int_t MAX = 2500;

  // structure to fill up
  fmsGain_st gain[MAX];
  
  if(option.Contains("testinput")){
    // fill only first entry out of 548 for testing purposes:
    gain[0].detectorId = 2; // 1-2
    gain[0].ch = 7; // 1-20
    gain[0].gain = .15; // 1-16
    
    gain[1].detectorId = 1; // 1-2
    gain[1].ch = 3; // 1-20
    gain[1].gain = .7; // 1-16
    
    gain[2].detectorId = 2; // 1-2
    gain[2].ch = 20; // 1-20
    gain[2].gain = .15; // 1-16
    
    gain[3].detectorId = 1; // 1-2
    gain[3].ch = 13; // 1-20
    gain[3].gain = .9; // 1-16
  }

  int ew[2] = {0,1};
  int detId[14] =   { 0,  1, 2, 3,  4,  5,  6,  7,   8,   9,   10,  11,  12,  13};
  int detType[14] = { 0,  0, 1, 1,  2,  2,  3,  3,   4,   4,    4,   4,   5,   5};
  int detNx[14] =   { 7,  7, 7, 7, 48, 48,  1,  1,  17,  17,   12,  12,   9,   9};
  int detNy[14] =   { 7,  7, 1, 1,  1,  1, 48, 48,  34,  34,   24,  24,  12,  12};
  //int module[14] =  { 1,  2, 3, 4,  5,  6,  7,  8,   1,   2,    3,   4,   5,   6};
  //int maxch[14]  =  {49, 49, 7, 7, 48, 48, 48, 48, 578, 578,  288, 288, 108, 108};
  int module[8] =  { 1,  2, 5,  6,  1,   2,    3,   4};
  int maxch[8]  =  {49, 49, 7, 7, 578, 578,  288, 288};
  int ngain = 0;

  if(option.Contains("readtext")){
    //char* file="fmsgain.20080526.txt";
    FILE* fp;
    int rew,rmod,rch;
    float rgain;
    ngain = 0;
    cout << "Reading "<<dataspec<<"\n";
    if(fp=fopen(dataspec,"r")){
      for(int iew=0; iew<2; iew++){
	for(int mod=0; mod<4; mod++){
	  for(int ch0=0; ch0<maxch[mod+iew*4]; ch0++){
	    if(!feof(fp)) fscanf(fp,"%d %d %d %f",&rew,&rmod,&rch,&rgain);
	    //printf("%5d %5d %5d %6.4f\n",rew,rmod,rch,rgain);
	    if(rmod!=module[mod+iew*4]) cout << "Error mod# ="<<rmod<<" "<<module[mod+iew*4]<<"\n";
	    if(rch !=ch0 +1) cout << "Error ch#  ="<<rch <<" "<<ch0+1<<"\n";
	    gain[ngain].detectorId=(rew-1)*8+rmod-1;
	    gain[ngain].ch=rch;
	    if(data.Contains("run8"))
	      gain[ngain].gain=rgain; 
	    else
	      if(data.Contains("run9"))  
		gain[ngain].gain=1.;  //temporary for run9
	    ngain++;
	  }
	}
      }
    }
    fclose(fp);
  }

  if(option.Contains("readdb")){
    gSystem->Load("StChain");
    gSystem->Load("StBFChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_Tables");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");

    St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
    dbMk->SetDebug();
    dbMk->SetDateTime(readDate,readTime);
    dbMk->SetFlavor("ofl"); // for offline calibrations/mapping
    // dbMk->SetFlavor("simu"); // for simulations
    dbMk->Init();
    dbMk->Make();

    // this is done inside ::Make method
    TDataSet *DB = 0;
    // "dbMk->" will NOT be needed. 
    //  if done inside your FmsDbMaker. Simply use DB = GetInputDb("Calibrations/fms/mapping")
    DB = dbMk->GetInputDB("Calibrations/fms");
    if (!DB) { std::cout << "ERROR: no db maker?" << std::endl; }

    // fetch ROOT descriptor of db table
    St_fmsGain *dbppmap = 0;
    dbppmap = (St_fmsGain*) DB->Find("fmsGain");
    // fetch data and place it to appropriate structure
    ngain = 0;
    if (dbppmap) {
      std::cout << "Reading fmsGain table\n";
      fmsGain_st *pptable = dbppmap->GetTable();
      for(int iew=0; iew<2; iew++){
	for(int mod=0; mod<4; mod++){
	  for(int ch0=0; ch0<maxch[mod+iew*4]; ch0++){
	    printf("%3d%8d%8d%8.4f\n",int(pptable[ngain].detectorId/8+1),int(pptable[ngain].detectorId%8+1),
		   int(pptable[ngain].ch),float(pptable[ngain].gain) );
	    ngain++;
	  }
	}
      }
      memcpy(gain,pptable,sizeof(gain));
    }
    else {
      std::cout << "WARNING: No data in fmsGain table (wrong timestamp?). Nothing to return, then.\n";
    }
  }


  if(option.Contains("writetext")){
    int ii = data.Length();
    data.Insert(ii-4,"_out");
    char* file=data.Data();
    FILE* fp;
    cout << "Writing "<<file<<"\n";
    ngain = 0;
    if(fp=fopen(file,"w")){
      for(int iew=0; iew<2; iew++){
	for(int mod=0; mod<4; mod++){
	  for(int ch0=0; ch0<maxch[mod+iew*4]; ch0++){
	    if(gain[ngain].detectorId/8==0)
	      fprintf(fp,"%2d%2d%3d%6.1f\n",gain[ngain].detectorId/8+1,gain[ngain].detectorId%8+1,
		      gain[ngain].ch,gain[ngain].gain);
	    else
	      if(gain[ngain].detectorId/8==1)
		fprintf(fp,"%3d%8d%8d%8.4f\n",gain[ngain].detectorId/8+1,gain[ngain].detectorId%8+1,
			gain[ngain].ch,gain[ngain].gain);
	      else printf("ERROR: detectorId %d not recognized.\n",gain[ngain].detectorId);
	    ngain++;
	  }
	}
      }
    }
    fclose(fp);
  }

  if(option.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    //putenv("DB_ACCESS_MODE=write");
    //char* env = getenv("DB_ACCESS_MODE");
    //cout <<  "Setting DB_ACCESS_MODE " << env << endl;
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
    StDbTable* table = node->addDbTable("fmsGain");
    mgr->setStoreTime(storeTime.Data());
    // store data in the table
    table->SetTable((char*)&gain,ngain);
    // set store time
    // store table in dBase
    mgr->storeDbTable(table);
    //StDbTable* table = node->findTable("fmsGain");
    //node->removeTable(table);
    gSystem->Unsetenv("DB_ACCESS_MODE");
    //unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
  }
}

