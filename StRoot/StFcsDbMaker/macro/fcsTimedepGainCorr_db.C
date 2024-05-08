#include <iostream.h> 
#include <fstream.h>
#include <string>

class StFcsDb;
StFcsDb* mFcsDb=0;

static const int NPERIOD=9;
const int RUN0[NPERIOD+1]={22359013,23005043,23048036,23066056,23073059,23080057,23087057,23094050,23101043,23164000}; //starts of period
const int RUN1[NPERIOD]  ={23007007,23007007,23048050,23067001,23074017,23081008,23087070,23095010,23101063}; //1st calib RUN near start
const int RUN2[NPERIOD]  ={23007011,23007011,23048051,23067002,23074018,23081009,23087072,23095011,23101064}; //2nd calib run near start
const int RUN3[NPERIOD]  ={23048002,23048002,23066013,23073042,23080044,23087033,23094044,23101005,23108014}; //1st calib run near end
const int RUN4[NPERIOD]  ={23048003,23048003,23066017,23073043,23080045,23087034,23094045,23101013,23108015}; //2nd calib run near end
const char *gainfile[NPERIOD][2] ={{"period1/fcsgaincorr_007_final.txt","period1/fcsgaincorr_048P1_final.txt"},  //period0
				   {"period1/fcsgaincorr_007_final.txt","period1/fcsgaincorr_048P1_final.txt"},  //period1
				   {"period2/fcsgaincorr_048_03.txt",   "period2/fcsgaincorr_066_03.txt"},       //period2
				   {"period3/fcsgaincorr_067_05.txt",   "period3/fcsgaincorr_073_06.txt"},       //period3
				   {"period4/fcsgaincorr_074_final.txt","period4/fcsgaincorr_080_final.txt"},    //period4
				   {"period5/fcsgaincorr_081_03.txt",   "period5/fcsgaincorr_087_04.txt"},       //period5
				   {"period6/fcsgaincorr_70_3.txt",     "period6/fcsgaincorr_94_3.txt"},         //period6
				   {"period7/fcsgaincorr_095_2.txt",    "period7/fcsgaincorr_101_3.txt"},        //period7
				   {"period8/fcsgaincorr_101_4.txt",    "period8/fcsgaincorr_108_4.txt"}};       //period8
const int NIDX=1496;
float mGainCorrCalib[NIDX][2];
float mGainCorr[NIDX];
const int showidx=500;

const int MAXSCL=350000;
const int MAXRUN=10000;
const int LIMIT[4] = {5e6,1e7,2e6,2e6};
const int STARTRUN=22354029;

unsigned int NDATA=0;
unsigned int TIME[MAXSCL];
double BBCW[MAXSCL];
double BBCA[MAXSCL];
double ZDCW[MAXSCL];
double ZDCA[MAXSCL];

unsigned int NRUN=0;
unsigned int RUNT[MAXRUN];
unsigned int RUNN[MAXRUN];
double ITGL[MAXRUN];


//read gain file
void readGainCorr(int period){
    for(int i=0; i<2; i++){
	printf("Reading GainCorr from %s\n",gainfile[period][i]);
	FILE* F=fopen(gainfile[period][i],"r");
	if(F == NULL){
	    printf("Could not open %s\n",gainfile[period][i]);
	    return;
	}
	int ehp,ns,dep,ch;
	float gain;
	while(fscanf(F,"%d %d %d %d %f",&ehp,&ns,&dep,&ch,&gain) != EOF){
	    int det,id,crt,slt;
	    mFcsDb->getIdfromDep(ehp,ns,dep,ch,det,id,crt,slt);
	    int idx=det*748 + id;
	    mGainCorrCalib[idx][i]=gain;
	    if(idx==showidx){
		printf("GAINCORR  ehp%1d ns%1d dep%02d ch%02d id=%3d idx=%4d start/stop=%1d %f\n",
		       ehp,ns,dep,ch,id,idx,i,mGainCorrCalib[idx][i]);
	    }
	}
	fclose(F);
    }
}

//write gain file
void writeGainCorr(int run){    
    char file[200];
    sprintf(file,"corr/%d.txt",run);
    printf("Writing %s\n",file);
    FILE* F=fopen(file,"w");
    if(F == NULL){
	printf("Could not open %s\n",file);
	return;
    }
    int ehp=0;
    for(int ns=0; ns<2; ns++){
	for(int dep=0; dep<24; dep++){
	    for(int ch=0; ch<32; ch++){
		int det,id,crt,slt;
		mFcsDb->getIdfromDep(ehp,ns,dep,ch,det,id,crt,slt);
		int idx = -1;
		float gain=0; 
		if(det>=0 && det<2){
		    idx=det*748 + id;
		    gain=mGainCorr[idx];
		}
		fprintf(F,"%1d %1d %2d %2d %f\n",ehp,ns,dep,ch,gain);
	    }
	}
    }
    fclose(F);
}

// Reading scaler file
void readScaler(){
    char filename[100],line[200];
    sprintf(filename,"scaler.txt");
    FILE *F=fopen(filename,"r");
    if(F==NULL){
	cout << "Cannot open " << filename << endl;
	continue;
    }
    cout << "Reading " << filename << endl;
    while(fgets(line, 200, F) != NULL){
	unsigned int ts;
	double rs2,rs3,rs7,rs8;
	char d[100],t[100];
	sscanf(line,"%lf %lf %lf %lf %d %s %s",&rs2,&rs3,&rs7,&rs8,&ts,d,t);
	if(rs2>0 || rs3>0 || rs7>0 || rs8>0){
	    //if(rs2 > LIMIT[0]) continue;
	    //if(rs3 > LIMIT[1]) continue;
	    if(rs7 > LIMIT[2]) continue;
	    //if(rs8 > LIMIT[3]) continue;
	    BBCW[NDATA]=rs2;
	    BBCA[NDATA]=rs3;
	    ZDCW[NDATA]=rs7;
	    ZDCA[NDATA]=rs8;
	    TIME[NDATA]=ts;
	    NDATA++;
	    //printf("%d %f %f %f %f %d %s %s\n",NDATA,rs2,rs3,rs7,rs8,ts,d,t);
	}
    }
    fclose(F);
    printf("Read %d scaler data\n",NDATA);
}

// Reading run file
void readRun(){
    char filename[100],line[200];
    sprintf(filename,"runs.txt");
    FILE *F=fopen(filename,"r");
    if(F==NULL){
	cout << "Cannot open " << filename << endl;
	continue;
    }
    cout << "Reading " << filename << endl;
    while(fgets(line, 200, F) != NULL){
	unsigned int run, ts; 
	sscanf(line,"%d %d",&run,&ts);
	if(run>=STARTRUN){
	    RUNN[NRUN]=run;
	    RUNT[NRUN]=ts;
	    //printf("%d %d %d\n",NRUN,run,ts);
	    NRUN++;    
	}
    }
    fclose(F);
    printf("Read %d runs\n",NRUN);
}
   
void scaler(){    
    readScaler();
    readRun();
  
    FILE* F=fopen("integralLumi.txt","w");
    fprintf(F,"# run  bbcW bbcE*W zdcW zdcE*W  [10^9 counts]\n");
    unsigned int itime=0;
    double bbcw=0,bbca=0,zdcw=0,zdca=0;
    for(unsigned int irun=0; irun<NRUN; irun++){
	while(TIME[itime] < RUNT[irun]){
	    bbcw += BBCW[itime];
	    bbca += BBCA[itime];
	    zdcw += ZDCW[itime];
	    zdca += ZDCA[itime];
	    //printf("%8d %10.2f %10.2f %10.2f %10.2f TS=%12d %12d %12d %12d \n",RUNN[irun],bbcw,bbca,zdcw,zdca,RUNT[irun],TIME[itime],RUNT[irun]-TIME[itime],itime);
	    itime++;
	    if(itime >= NDATA) break;
	}
	double f=30e-9;
	printf("%8d %14.6f %14.6f %14.6f %14.6f TS=%12d %12d %12d %12d\n",
	       RUNN[irun],bbcw*f,bbca*f,zdcw*f,zdca*f,RUNT[irun],TIME[itime],RUNT[irun]-TIME[itime],itime);
	fprintf(F,"%8d %14.6f %14.6f %14.6f %14.6f\n",
		RUNN[irun],bbcw*f,bbca*f,zdcw*f,zdca*f);
	ITGL[irun]=bbca*f;
    }
    fclose(F);

    c1 = new TCanvas("c1","SCALER",50,0,1500,1200);
    gStyle->SetLabelSize(0.03,"xy");
    gStyle->SetPalette(1);
    gStyle->SetOptStat(0);
    c1->Divide(1,2);    
    c1->SaveAs("scaler.png");
}

double getIntgLumi(int run){
    for(unsigned int irun=0; irun<NRUN; irun++){
	if(run==RUNN[irun]) return ITGL[irun];
    }
    return 0;
}

//void fcsTimedepGainCorr_db(int period=1, char* opt = "writetxt", int onlydorun=23048002) {
void fcsTimedepGainCorr_db(int period=1, char* opt = "writetxt", int onlydorun=0) {
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("St_base.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");
    gSystem->Load("StChain.so");
    gSystem->Load("StBFChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_Tables");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");  
    gSystem->Load("StFcsDbMaker.so");

    //create and initialize StFcsDb
    mFcsDb=new StFcsDb; 
    mFcsDb->Init();

    // structure to fill up
    fcsEcalGainCorr_st ecorr;
    int readTime, readDate;
    
    TString option(opt);
    std::cout << "Opt =" << opt << "\n";
    std::cout << "writedb = " << option.Contains("writedb") << "\n";
    
    // scaler analsys
    scaler();

    int p=-1;
    double l12,l34;
    for(unsigned int irun=0; irun<NRUN; irun++){
	unsigned int run=RUNN[irun];
	//printf("irun=%d NRUN=%d run=%d\n",irun,NRUN,run);
	if(run==RUN0[p+1]){ //new period
	    p++;
	    if(period==-1 || period==p){ //do this period
		readGainCorr(p);
		double l1=getIntgLumi(RUN1[p]);
		double l2=getIntgLumi(RUN2[p]);
		double l3=getIntgLumi(RUN3[p]);
		double l4=getIntgLumi(RUN4[p]);
		l12=(l1+l2)/2.0;
		l34=(l3+l4)/2.0;
	    }
	}
	//printf("Run=%d p=%d Period=%d\n",run,p,period);
	if(p>=0 && (period==-1 || period==p) ){ //do this run
	    if(onlydorun>0 && run!=onlydorun) continue;
	    //getting run start time from DB
	    int year=run/1000000-1;
	    int port=3400+year-1;
	    //printf("Year=%d Port=%d\n",year,port);
	    char cmd[400];
	    sprintf(cmd,"mysql -h db04.star.bnl.gov --port=%d -N -s -e \"SELECT startRunTime FROM RunLog.runDescriptor WHERE runNumber=%d LIMIT 1\"",port,run);
	    //printf("cmd=%s\n",cmd);
	    TString st=gSystem->GetFromPipe(cmd);
	    int starttime=st.Atoi();
	    readDate=gSystem->GetFromPipe(Form("date -u -d \@%d +%%Y%%m%%d",starttime)).Atoi();
	    readTime=gSystem->GetFromPipe(Form("date -u -d \@%d +%%H%%M%%S",starttime)).Atoi();

	    //getting integrated lumionosity for the run and calc gain
	    double l=getIntgLumi(run);
	    double w1=(l34-l)/(l34-l12);
	    double w2=(l-l12)/(l34-l12);
	    for(int idx=0; idx<NIDX; idx++){		
		double c1 = mGainCorrCalib[idx][0];
		double c2 = mGainCorrCalib[idx][1];
		double cc = c1*w1 + c2*w2;
		mGainCorr[idx]=cc;
		ecorr.gaincorr[idx]=cc;
		if(idx==showidx){ 
		    printf("Run=%8d Period=%1d Start=%d Date=%06d Time=%06d idx=%4d %6.3f %6.3f %8.4f %8.4f %8.4f\n",
			   run,p,starttime,readDate,readTime,idx,w1,w2,c1,c2,ecorr.gaincorr[idx]);
		}
	    }
	    if(option.Contains("writetxt")) {
		writeGainCorr(run);
	    }
	    if(option.Contains("writedb")) {
		gSystem->Setenv("DB_ACCESS_MODE","write");
		cout << "DB_ACCESS_MODE="<<gSystem->Getenv("DB_ACCESS_MODE")<<endl;
		StDbManager* mgr = StDbManager::Instance();
		StDbConfigNode* node = mgr->initConfig("Calibrations_fcs");
		mgr->setStoreTime(starttime);	  
		StDbTable* table = node->addDbTable("fcsEcalGainCorr");
		table->SetTable((char*)&ecorr,1);
		table->setFlavor("ofl");
		mgr->storeDbTable(table);
	    }
	}		
    }    
    
    //reqading back DB
    printf("Reading back DB\n");
    int date,time,from=0,n=0;
    /*
    TString datetime(storeTime),token;
    datetime.ReplaceAll("-","");
    datetime.ReplaceAll(":","");
    while(datetime.Tokenize(token,from," ")){
	if(n==0) date=atoi(token.Data());
	if(n==1) time=atoi(token.Data());
	n++;
    }
    std::cout << "Readout time="<<datetime<<" Date="<<date<<" Time="<<time<<endl;
    */   
    std::cout << "Readout Date="<<readDate<<" Time="<<readTime<<endl; 

    St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
    dbMk->SetDebug();
    dbMk->SetDateTime(readDate,readTime);
    dbMk->SetFlavor("ofl"); 
    dbMk->Init();
    dbMk->Make();
    
    TDataSet *DB = 0;
    DB = dbMk->GetInputDB("Calibrations/fcs");
    if(!DB){std::cout << "ERROR: no db maker or Calibrations/fcs" << std::endl; }
    St_fcsEcalGainCorr *dbTable_ec = (St_fcsEcalGainCorr*) DB->Find("fcsEcalGainCorr");
    if(dbTable_ec){
	std::cout << "Reading fcsEcalGainCorr table from DB\n";
	fcsEcalGainCorr_st *dbSt_ec = dbTable_ec->GetTable();      
	Int_t rows = dbTable_ec->GetNRows();
	for(int i=0; i<rows; i++){
	    for(int id=0; id<1496; id++){
		printf("DbRead row=%2d id=%d gaincorr=%10.6f\n",
		       i,id,dbSt_ec[i].gaincorr[id]);
	    }
	}
      }else{
	std::cout << "WARNING: No data in fcsEcalGainCorr table\n";
    }
}

