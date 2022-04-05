#include <iostream.h> 
#include <fstream.h>

// max index dimensions
static const Int_t MAX = 2500;
static const Int_t MAXD = 14;
static const Int_t MAXCH= 578;

int getDetectorId(int ew, int nstb) {return (ew-1)*8+nstb-1;}
int getEW(int detid) {return detid/8 + 1;}
int getNSTB(int detid) {return detid%8 + 1;}

void fms_db_fmsgaincorrB(char* option = "writetext 15sim", char* dataspec="allone", 
			char* storetime="2007-11-09 12:00:00",
			int readDate=20071210, int readTime=0, int isSim=0) {
  // that's begin time for validity range for WRITING TO DB 
  // your data will be available from storeTime till 2037    
  TString opt(option);
  TString data(dataspec);
  TString storeTime(storetime);  
  if(opt.Contains("run8") && opt.Contains("dAu200")){
      storeTime = "2007-11-09 12:00:00";
  }else if(opt.Contains("run8") && opt.Contains("pp200")){
      storeTime = "2008-01-28 12:00:00";
  }else if(opt.Contains("run9") && opt.Contains("pp200")){
      storeTime = "2009-01-16 00:00:00";
  }else if(opt.Contains("15sim")){
      storeTime = "2014-12-10 00:00:01"; readDate = 20141215; readTime = 0; isSim=1;
  }else if(opt.Contains("15ofl")){
      storeTime = "2014-12-20 00:00:00"; readDate = 20141225; readTime = 0;
  }else if(opt.Contains("17sim")){
      storeTime = "2016-12-10 00:00:01"; readDate = 20161215; readTime = 0; isSim=1;
  }else if(opt.Contains("17ofl")){
      storeTime = "2016-12-20 00:00:00"; readDate = 20161225; readTime = 0;
  }else {
      std::cout<<"Invalid year range"<<std::endl;
  }

  std::cout << "Opt =" << option << "\n";
  std::cout << "testinput = " << opt.Contains("testinput")  << "\n";
  std::cout << "readtext  = " << opt.Contains("readtext")  << "\n";
  std::cout << "readdb    = " << opt.Contains("readdb")    << "\n";
  std::cout << "writedb   = " << opt.Contains("writedb")   << "\n";
  std::cout << "writetext = " << opt.Contains("writetext") << "\n";
  std::cout << "dataspec = "  << dataspec << endl; 
  std::cout << "storeTime = " << storeTime << endl; 
  std::cout << "readData = "  << readDate << " readTime="<<readTime<<endl; 
  std::cout << "isSim = "     << isSim << endl; 

  gROOT->Macro("LoadLogger.C");
  gSystem->Load("St_base.so");
  gSystem->Load("libStDb_Tables.so");
  gSystem->Load("StDbLib.so");

  // structure to fill up
  fmsGainCorrectionB_st corr;
  memset(&corr,0,sizeof(corr));

  if(data.Contains("allone")){
      int maxch[MAXD]  = {49, 49, 7, 7, 48, 48, 48, 48, 578, 578,  288, 288, 108, 108};      
      int index=0;
      for(int det=8; det<=11; det++){
	  for(int ch=0; ch<maxch[det]; ch++){
	      corr.detectorId[index]=det;
	      corr.ch        [index]=ch+1;
	      corr.corr      [index]=1.0;
	      printf("n=%4d det=%2d ch=%3d corr=%8.6f\n",
		     index,corr.detectorId[index],corr.ch[index],corr.corr[index]);
	      index++;
	  }
      }
  }
  
  if(opt.Contains("readtext")){
      FILE* fp;
      int rew,rnstb,rch;
      float rcorr;
      int nread = 0;
      cout << "Reading "<<dataspec<<"\n";
      if(fp=fopen(dataspec,"r")){
	  while(fscanf(fp,"%d %d %d %f",&rew,&rnstb,&rch,&rcorr) != EOF){
	      int detid=getDetectorId(rew,rnstb);
	      if(detid<0 || rch<1) {
		  printf("Invalid ch#: det=%d ew=%d nstb=%d rch=%d\n",detid,rew,rnstb,rch,rcorr);
		      continue; //hack! ignore 
	      }
	      if(rew==1) continue; //hack! ignore east
	      corr.detectorId[nread]=detid;
	      corr.ch[nread]=rch;
	      corr.corr[nread]=rcorr;
	      printf("n=%4d det=%2d ew=%1d nstb=%1d ch=%3d corr=%8.6f\n",nread,detid,rew,rnstb,rch,rcorr);
	      nread++;
	  }
      }
      fclose(fp);
      printf("read %d channels of gain corr\n",nread);
  }
  
  if(opt.Contains("writedb")) {
      gSystem->Setenv("DB_ACCESS_MODE","write");
      //putenv("DB_ACCESS_MODE=write");
      //char* env = getenv("DB_ACCESS_MODE");
      //cout <<  "Setting DB_ACCESS_MODE " << env << endl;
      StDbManager* mgr = StDbManager::Instance();
      StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
      StDbTable* table = node->addDbTable("fmsGainCorrectionB");
      mgr->setStoreTime(storeTime.Data());
      // store data in the table
      table->SetTable((char*)&corr,1);
      // set store time
      // store table in dBase
      if(isSim) table->setFlavor("sim");
      mgr->storeDbTable(table);
      //StDbTable* table = node->findTable("fmsGain");
      //node->removeTable(table);
      gSystem->Unsetenv("DB_ACCESS_MODE");
      //unsetenv("DB_ACCESS_MODE");
      std::cout << "Done with database upload"<< endl;
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
      if(isSim) dbMk->SetFlavor("sim"); // for simulations
      dbMk->Init();
      dbMk->Make();
      
      // this is done inside ::Make method
      TDataSet *DB = 0;
      // "dbMk->" will NOT be needed. 
      //  if done inside your FmsDbMaker. Simply use DB = GetInputDb("Calibrations/fms/mapping")
      DB = dbMk->GetInputDB("Calibrations/fms");
      if (!DB) { std::cout << "ERROR: no db maker?" << std::endl; }

      // fetch ROOT descriptor of db table
      St_fmsGainCorrectionB *dbppmap = 0;
      dbppmap = (St_fmsGainCorrectionB*) DB->Find("fmsGainCorrectionB");
      // fetch data and place it to appropriate structure
      int ngain = 0;
      if (dbppmap) {
	  std::cout << "Reading fmsGainCorrectionB table\n";
	  fmsGainCorrectionB_st *pptable = dbppmap->GetTable();
	  for(int i=0; i<MAX; i++){
              if(pptable[0].ch[i]>0){
                  printf("%3d%8d%8d%8.4f\n",
                         int(pptable[0].detectorId[i]/8+1),int(pptable[0].detectorId[i]%8+1),
                         int(pptable[0].ch[i]),float(pptable[0].corr[i]) );
                  ngain++;
              }
          }
      } else {
	  std::cout << "WARNING: No data in fmsGain table (wrong timestamp?). Nothing to return, then.\n";
      }
  }  
}

