#include <iostream.h> 
#include <fstream.h> 

void fpostPedSV2017_db(char* opt="", char* year="", char* filename="fpost_good_physics_ped/18080012.txt"){
  TString option(opt), yr(year);
  stringstream f1(filename);
  TString storeTime;     // storetime is begin time for validity range for WRITING DB
  time_t starttime=0;    //unix time for writing, if reading from file
  int date=0,time=0;     // time for READING DB
  int flag=0;
  
  std::cout << "Opt =" << opt << "\n";  
  std::cout << "write = " << option.Contains("writedb")   << "\n";
  
  gROOT->Macro("loadlib.C");
  
  const Int_t MAX_DB_INDEX = 241;
  fpostPed_st table[MAX_DB_INDEX];
  int idList[MAX_DB_INDEX];
  memset(table,0,sizeof(table));
  memset(idList,0,sizeof(idList));

  //int idx=f1.Index("_");
  //TString f2=f1(idx+1,8);
  //int run=f2.Atoi();
  
  int idx = f1.str().find("/");
  cout << "idx: " << idx << endl;
  stringstream f2(f1.str().substr(idx+1,8));
  cout << f2.str() << endl;
  int run = 0;
  f2 >> run;

  cout << f1.str() << " idx="<<idx  << " f2="<<f2.str() << " run="<<run << endl;
  
  char *onlserver="onldb", *bakserver="dbbak", *server=0;
  int  port = 0;
  int y=run/1000000 -1;
  if(y >= 18){
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
  if(yr.Contains("17ofl")){
	storeTime="2016-12-20 00:00:01";
	date = 20161220; time = 0;
	flag=1;
    }else if(yr.Contains("17sim")){
	storeTime="2016-12-10 00:00:02";
	date = 20161210; time = 0;
	flag=1;
    }
    if(flag==0){
	std::cout << "unix storetime =" << starttime-10 << endl;
	std::cout << "read date,time =" << date <<" "<< time << "\n";
    }else{
	std::cout << "storetime =" << storeTime << "\n";
	std::cout << "read date,time =" << date <<" "<< time << "\n";
    }
    
    ifstream fp(filename);
    int n = 0;
    cout << "Reading "<<filename<<" run#="<<run<<endl;
    if(fp.is_open()){
      while( !fp.eof() ){
	int id=-1;
	float mip=-1,sigma=-1;
	
	fp >> id >> mip >> sigma;
	if( id == -1 ){continue;}
	table[n].slatid=id;
	table[n].Mean=mip;
	table[n].Sigma=sigma;
	idList[n]=id+1;
	printf("n=%3d Id=%5d MIP=%6.2f Sigma=%6.2f idList=%3d\n",
	       n,table[n].slatid,table[n].Mean,table[n].Sigma,idList[n]);
	n++;
      }
      fp.close();
      printf("read %d channels from %s\n",n,filename);
    }
    else
      {
	cout << "Could not open: " << filename << endl;
	exit(0);
      }

    if(option.Contains("writedb")) {
	gSystem->Setenv("DB_ACCESS_MODE","write");
	StDbManager* mgr = StDbManager::Instance();
	StDbConfigNode* node = mgr->initConfig("Calibrations_fps");
	StDbTable* dbtable = node->addDbTable("fpostPed");
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
    DB = dbMk->GetDataBase("Calibrations/fps/fpostPed");
    if (!DB) std::cout << "ERROR: no table found in db, or malformed local db config" << std::endl;
    St_fpostPed *dataset = 0;
    dataset = (St_fpostPed*) DB->Find("fpostPed");
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
    fpostPed_st *tbl = dataset->GetTable();
    for (Int_t i = 0; i < rows; i++) {
	std::cout << Form("Row=%4d slatid=%3d MIP=%8.4f Sig=%8.4f\n",i,
			  tbl[i].slatid,tbl[i].Mean,tbl[i].Sigma);
    }
}
