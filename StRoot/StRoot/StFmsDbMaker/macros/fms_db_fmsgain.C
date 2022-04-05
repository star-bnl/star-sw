#include <iostream.h> 
#include <fstream.h>

// max index dimensions
static const Int_t MAX = 2500;
static const Int_t MAXD = 14;
static const Int_t MAXCH= 578;

int getDetectorId(int ew, int nstb) {return (ew-1)*8+nstb-1;}
int getEW(int detid) {return detid/8 + 1;}
int getNSTB(int detid) {return detid%8 + 1;}

void fms_db_fmsgain(char* option = "readtext writetext 15sim", char* dataspec="FmsPtGainR16.txt") {
  // that's begin time for validity range for WRITING TO DB 
  // your data will be available from storeTime till 2037    
  TString opt(option);
  TString data(dataspec);
  int readDatte=20071210, readTime=0, isSim=0;
  if(opt.Contains("run8") && opt.Contains("dAu200")){
      TString storeTime = "2007-11-09 12:00:00";
  }else if(opt.Contains("run8") && opt.Contains("pp200")){
      TString storeTime = "2008-01-28 12:00:00";
  }else if(opt.Contains("run9") && opt.Contains("pp200")){
      TString storeTime = "2009-01-16 00:00:00";
  }else if(opt.Contains("15sim")){
      TString storeTime = "2014-12-10 00:00:01"; readDate = 20141215; readTime = 0; isSim=1;
  }else if(opt.Contains("15ofl")){
      TString storeTime = "2014-12-20 00:00:00"; readDate = 20141225; readTime = 0;
  }      
  else std::cout<<"Invalid year range"<<std::endl;
  
  std::cout << "Opt =" << option << "\n";
  std::cout << "testinput = " << opt.Contains("testinput")  << "\n";
  std::cout << "readtext  = " << opt.Contains("readtext")  << "\n";
  std::cout << "readdb    = " << opt.Contains("readdb")    << "\n";
  std::cout << "writedb   = " << opt.Contains("writedb")   << "\n";
  std::cout << "writetext = " << opt.Contains("writetext") << "\n";
  
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // structure to fill up
  fmsGain_st gain[MAX];
  int idx[MAXD][MAXCH];
  memset(idx,-1,sizeof(idx));
  for(int i=0; i<MAX; i++){
      gain[i].detectorId=-1;
      gain[i].ch        =-1;
      gain[i].gain      =-1.0;
  }
  
  int detId[MAXD]  = { 0,  1, 2, 3,  4,  5,  6,  7,   8,   9,   10,  11,  12,  13};
  int detType[MAXD]= { 0,  0, 1, 1,  2,  2,  3,  3,   4,   4,    4,   4,   5,   5};
  int detNx[MAXD]  = { 7,  7, 7, 7, 48, 48,  1,  1,  17,  17,   12,  12,   9,   9};
  int detNy[MAXD]  = { 7,  7, 1, 1,  1,  1, 48, 48,  34,  34,   24,  24,  12,  12};
  int ew[MAXD]     = { 0,  0, 0, 0,  0,  0,  0,  0,   1,   1,    1,   1,   1,   1};
  int nstb[MAXD]   = { 1,  2, 5, 6,  3,  4,  7,  8,   1,   2,    3,   4,   1,   2};
  int maxch[MAXD]  = {49, 49, 7, 7, 48, 48, 48, 48, 578, 578,  288, 288, 108, 108};
  
  int index=0;
  for(int det=0; det<MAXD; det++){
      for(int ch=0; ch<maxch[det]; ch++){
	  gain[index].detectorId=det;
	  gain[index].ch        =ch+1;
	  idx[det][ch]          =index;	  
	  index++;	  
      }
  }
  
  if(opt.Contains("readtext")){
      //char* file="fmsgain.20080526.txt";
      FILE* fp;
      int rew,rnstb,rch;
      float rgain;
      int nread = 0;
      cout << "Reading "<<dataspec<<"\n";
      if(fp=fopen(dataspec,"r")){
	  while(fscanf(fp,"%d %d %d %f",&rew,&rnstb,&rch,&rgain) != EOF){
	      int detid=getDetectorId(rew,rnstb);
	      int index=idx[detid][rch-1];
	      //printf("EW=%5d NSTB=%5d CH=%5d Gain=%6.4f | det=%2d idx=%4d\n",rew,rnstb,rch,rgain,detid,index);	  
	      if(detid != gain[index].detectorId) printf("ERR DetId %d != %d\n",detid,gain[index].detectorId); 
	      if(rch   != gain[index].ch)         printf("ERR Ch    %d != %d\n",rch,gain[index].ch);
	      gain[index].gain=rgain;
	      nread++;
	  }
      }
      fclose(fp);
      printf("read %d channels of gain\n",nread);
  }

  if(opt.Contains("readdb")){
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
    int ngain = 0;
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

  if(opt.Contains("writetext")){
    int ii = data.Length();
    data.Insert(ii-4,"_out");
    char* file=data.Data();
    FILE* fp;
    cout << "Writing "<<file<<"\n";
    int ngain = 0;
    if(fp=fopen(file,"w")){
	for(int i=0; i<MAX; i++){
	    if(gain[i].gain>=0.0){
		int detid=gain[i].detectorId;
		int iew=getEW(detid);
		int instb=getNSTB(detid);
		fprintf(fp,"%d %d %d %5.3f\n",iew,instb,
			gain[i].ch,gain[i].gain);
		ngain++;
	    }
	}
    }
    fclose(fp);
    printf("Write %d channels\n",ngain);
  }

  if(opt.Contains("writedb")) {
    gSystem->Setenv("DB_ACCESS_MODE","write");
    //putenv("DB_ACCESS_MODE=write");
    //char* env = getenv("DB_ACCESS_MODE");
    //cout <<  "Setting DB_ACCESS_MODE " << env << endl;
    StDbManager* mgr = StDbManager::Instance();
    StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
    StDbTable* table = node->addDbTable("fmsGain");
    mgr->setStoreTime(storeTime.Data());
    // store data in the table
    table->SetTable((char*)&gain,index+1);
    // set store time
    // store table in dBase
    if(isSim) table->setFlavor("sim");
    mgr->storeDbTable(table);
    //StDbTable* table = node->findTable("fmsGain");
    //node->removeTable(table);
    gSystem->Unsetenv("DB_ACCESS_MODE");
    //unsetenv("DB_ACCESS_MODE");
    std::cout << "Done with database upload \n";
  }
}

