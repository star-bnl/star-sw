#include <iostream.h> 
#include <fstream.h>

// max index dimensions
static const Int_t MAX = 2500;
static const Int_t MAXD = 14;
static const Int_t MAXCH= 578;

int getDetectorId(int ew, int nstb) {return (ew-1)*8+nstb-1;}
int getEW(int detid) {return detid/8 + 1;}
int getNSTB(int detid) {return detid%8 + 1;}

void fms_db_fmsgaincorrB_chong(char* option = "", 
			       char* filename = "chong/FmsGainCorr_16066033.txt",			       
			       char* flavor="fmsGainCorr-BNL-C",
			       int secOffset=10){ 


    TString opt(option), file(filename);
    int idx=file.Index("_");
    TString f2=file(idx+1,8);
    int run=f2.Atoi();
    int year=run/1000000-1;
    int port=3400+year-1;
    printf("%s run=%d year=%d port=%d\n",file.Data(),run,year,port);

    gROOT->Macro("loadlib.C");
    gSystem->Load("St_base.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");
    
    char cmd[300]=Form("mysql -h db04.star.bnl.gov --port=%d -N -s -e \"SELECT startRunTime FROM RunLog.runDescriptor WHERE runNumber=%d LIMIT 1\"",
                       port,run);
    TString st=gSystem->GetFromPipe(cmd);
    int starttime=st.Atoi();
    int readDate=gSystem->GetFromPipe(Form("date -u -d \@%d +%%Y%%m%%d",starttime)).Atoi();
    int readTime=gSystem->GetFromPipe(Form("date -u -d \@%d +%%H%%M%%S",starttime)).Atoi();
    printf("For run %d, online DB run starttime=%d - %d(secOffset) and DB readig date=%d time=%d\n",
	   run,starttime,secOffset,readDate,readTime);

    // structure to fill up
    fmsGainCorrectionB_st corr;
    memset(&corr,0,sizeof(corr));

    FILE* fp;
    int rew,rnstb,rch;
    float rcorr;
    int nread = 0;
    cout << "Reading "<<filename<<"\n";
    if(fp=fopen(filename,"r")){
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

    if(opt.Contains("writedb")) {
	gSystem->Setenv("DB_ACCESS_MODE","write");
	//putenv("DB_ACCESS_MODE=write");
	//char* env = getenv("DB_ACCESS_MODE");
	//cout <<  "Setting DB_ACCESS_MODE " << env << endl;
	StDbManager* mgr = StDbManager::Instance();
	StDbConfigNode* node = mgr->initConfig("Calibrations_fms");
	StDbTable* table = node->addDbTable("fmsGainCorrectionB");
	mgr->setStoreTime(starttime - secOffset);
	// store data in the table
	table->SetTable((char*)&corr,1);
	// set store time
	// store table in dBase
	table->setFlavor(flavor);
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
	dbMk->SetFlavor(flavor);
	//dbMk->SetFlavor("ofl");
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
	    std::cout << "WARNING: No data in fmsGainCorr table (wrong timestamp?). Nothing to return, then.\n";
	}
    }  
}

