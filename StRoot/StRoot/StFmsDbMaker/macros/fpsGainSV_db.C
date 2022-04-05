#include <iostream.h> 
#include <fstream.h> 

void fpsGainSV_db(char* opt="", char* year="", char* filename="fpsgain/pAu200_16125035.txt"){
    TString option(opt), yr(year),f1(filename);
    TString storeTime;     // storetime is begin time for validity range for WRITING DB
    time_t starttime=0;    //unix time for writing, if reading from file
    int date=0,time=0;     // time for READING DB
    int flag=0;
    
    std::cout << "Opt =" << opt << "\n";  
    std::cout << "write = " << option.Contains("writedb")   << "\n";
    
    gROOT->Macro("loadlib.C");
    
    const Int_t MAX_DB_INDEX = 252;
    fpsGain_st table[MAX_DB_INDEX];
    int idList[MAX_DB_INDEX];
    memset(table,0,sizeof(table));
    memset(idList,0,sizeof(idList));
    
    int idx=f1.Index("_");
    TString f2=f1(idx+1,8);
    int run=f2.Atoi();
    printf("%s idx=%d f2=%s run=%d\n",f1.Data(),idx,f2.Data(),run);
    
    char *onlserver="onldb",*bakserver="dbbak",*server=0;
    int  port;
    int y=run/1000000 -1;
    if(y == 17){
	server=onlserver;
	port=3501;
    }else{
	server=bakserver;
	port=3400+y-1;
    }
    char cmd[300]=Form("mysql -h %s.starp.bnl.gov --port=%d -N -s -e \"SELECT startRunTime FROM RunLog.runDescriptor WHERE runNumber=%d LIMIT 1\"",
		       server,port,run);
    TString st=gSystem->GetFromPipe(cmd);
    starttime=st.Atoi();

    date=gSystem->GetFromPipe(Form("date -u -d \@%d +%%Y%%m%%d",starttime)).Atoi();
    time=gSystem->GetFromPipe(Form("date -u -d \@%d +%%H%%M%%S",starttime)).Atoi();
    cout << cmd << endl;
    cout << "start time="<<starttime<<" date="<<date<<" time="<<time<< endl;    
    if(yr.Contains("15ofl")){
	storeTime="2014-12-20 00:00:01";
	date = 20141225; time = 0;
	flag=1;
    }else if(yr.Contains("15sim")){
	storeTime="2014-12-10 00:00:02";
	date = 20141215; time = 0;
	flag=1;
    }
    if(flag==0){
	std::cout << "unix storetime =" << starttime-10 << endl;
	std::cout << "read date,time =" << date <<" "<< time << "\n";
    }else{
	std::cout << "storetime =" << storeTime << "\n";
	std::cout << "read date,time =" << date <<" "<< time << "\n";
    }
    
    FILE* fp;
    int id;
    float mip,sigma,valley;
    int n = 0;
    cout << "Reading "<<filename<<" run#="<<run<<endl;
    if(fp=fopen(filename,"r")){
	while(fscanf(fp,"%d %f %f %f",&id,&mip,&sigma,&valley) != EOF){
	    table[n].slatid=id;
	    table[n].MIP=mip;
	    table[n].Sigma=sigma;
	    table[n].Valley=valley;
	    idList[n]=id+1;
	    printf("n=%3d Id=%5d MIP=%6.2f Sigma=%6.2f Valley=%6.2f idList=%3d\n",
		   n,table[n].slatid,table[n].MIP,table[n].Sigma,table[n].Valley,idList[n]);
	    n++;
	}
	fclose(fp);
    }
    printf("read %d channels from %s\n",n,filename);
    
    if(option.Contains("writedb")) {
	gSystem->Setenv("DB_ACCESS_MODE","write");
	StDbManager* mgr = StDbManager::Instance();
	StDbConfigNode* node = mgr->initConfig("Calibrations_fps");
	StDbTable* dbtable = node->addDbTable("fpsGain");
	if(flag==0){
	    mgr->setStoreTime(starttime-10);
	}else{
	    mgr->setStoreTime(storeTime.Data());
	}
	dbtable->SetTable((char*)&table, n, idList);
	if(yr.Contains("sim")) dbtable->setFlavor("sim");
	mgr->storeDbTable(dbtable);    
	std::cout << "INFO: table saved to database" << std::endl;
    }
    
    std::cout << "INFO: Reading database" << std::endl;  
    gSystem->Unsetenv("DB_ACCESS_MODE");
    //gSystem->Unsetenv("DB_SERVER_LOCAL_CONFIG");
    St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
    dbMk->SetDebug();
    dbMk->SetDateTime(date,time); // event or run start time, set to your liking
    if(yr.Contains("ofl"))      {dbMk->SetFlavor("ofl");}
    else if(yr.Contains("sim")) {dbMk->SetFlavor("sim");}
    dbMk->Init();
    dbMk->Make();
    TDataSet *DB = 0;
    DB = dbMk->GetDataBase("Calibrations/fps/fpsGain");
    if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
    St_fpsGain *dataset = 0;
    dataset = (St_fpsGain*) DB->Find("fpsGain");
    if (!dataset) {
	td::cout << "ERROR: dataset does not contain requested table" << std::endl;
	return;
    }
    Int_t rows = dataset->GetNRows();
    if (rows > 1) {
	std::cout << "INFO: found INDEXED table with " << rows << " rows" << std::endl;
    }
    TDatime val[2];
    dbMk->GetValidity((TTable*)dataset,val);
    std::cout << "Dataset validity range: [ " << val[0].GetDate() << "." << val[0].GetTime() << " - " 
	      << val[1].GetDate() << "." << val[1].GetTime() << " ] "
	      << std::endl;    
    fpsGain_st *tbl = dataset->GetTable();
    for (Int_t i = 0; i < rows; i++) {
	std::cout << Form("Row=%4d slatid=%3d MIP=%8.4f Sig=%8.4f Val=%8.4f\n",i,
			  tbl[i].slatid,tbl[i].MIP,tbl[i].Sigma,tbl[i].Valley);
    }
}
