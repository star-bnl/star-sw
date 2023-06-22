#include <iostream.h> 
#include <fstream.h>

void fcsDetectorPosition_db(char* opt = "", char* input) {
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("St_base.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");
    
    // structure to fill up
    fcsDetectorPosition_st detpos;
    
    TString option(opt);
    std::cout << "Opt =" << opt << "\n";
    std::cout << "writedb   = " << option.Contains("writedb")   << "\n";
    
    TString data(input);
    TString storeTime("");
    TString flavor;
    if(data.Contains("run21sim")){ //nominal
	storeTime = "2020-12-10 00:01:00"; flavor="sim"; 
	detpos.xoff[0]=-6.570*2.54; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 16.69
	detpos.xoff[1]= 6.570*2.54; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54; //z=710.16
	detpos.xoff[2]=-7.430*2.54; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 18.87
	detpos.xoff[3]= 7.430*2.54; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54; //z=782.63
    }
    if(data.Contains("run21ofl")){  //open
	storeTime = "2020-12-20 00:00:00"; flavor="ofl";
	detpos.xoff[0]=-6.570*2.54-50; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; 
	detpos.xoff[1]= 6.570*2.54+50; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54; 
	detpos.xoff[2]=-7.430*2.54-50; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; 
	detpos.xoff[3]= 7.430*2.54+50; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54; 
    }
    if(data.Contains("run21Feb17")){ //closed but not quite
	storeTime = "2021-02-17 18:00:00"; flavor="ofl";
	detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run21Mar31")){ //wide open
	storeTime = "2021-03-31 18:00:00"; flavor="ofl";
	detpos.xoff[0]=-6.850*2.54-50; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; 
	detpos.xoff[1]= 6.850*2.54+50; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54; 
	detpos.xoff[2]=-8.380*2.54-50; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; 
	detpos.xoff[3]= 8.380*2.54+50; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54; 
    }
    if(data.Contains("run21May06")){ //closed
	storeTime = "2021-05-06 20:00:00"; flavor="ofl";
	detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run21May20")){ //ecal open (hcal stayed closed)
	storeTime = "2021-05-20 18:00:00"; flavor="ofl";
	detpos.xoff[0]=-6.850*2.54-50; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54+50; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54;    detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54;    detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run21May24")){ //hcal open (ecal stayed open)
	storeTime = "2021-05-24 15:00:00"; flavor="ofl";
	detpos.xoff[0]=-6.850*2.54-50; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54+50; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54-50; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54+50; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run21Jun27")){ //closed
        storeTime = "2021-06-27 15:00:00"; flavor="ofl";
        detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22sim")){ //nominal
	storeTime = "2021-10-15 00:00:00"; flavor="sim"; 
        detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22simA")){ //nominal with measured Y
	storeTime = "2021-10-15 00:01:00"; flavor="sim"; 
        detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=-5.26; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=-5.26; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=+1.80; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=+1.80; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22ofl")){  //closed
	storeTime = "2021-10-25 00:00:00"; flavor="ofl";
        detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22oflA")){  //closed with measured Y
	storeTime = "2021-10-25 00:01:00"; flavor="ofl";
        detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=-5.26; detpos.zoff[0]=279.592*2.54; //x= 17.399
	detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=-5.26; detpos.zoff[1]=279.592*2.54;
	detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=+1.80; detpos.zoff[2]=308.123*2.54; //x= 21.285
	detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=+1.80; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22Dec01")){ //open
      storeTime = "2021-12-01 00:00:00"; flavor="ofl";
      detpos.xoff[0]=-6.850*2.54-50; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399                                   
      detpos.xoff[1]= 6.850*2.54+50; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
      detpos.xoff[2]=-8.380*2.54-50; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285                                   
      detpos.xoff[3]= 8.380*2.54+50; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22Dec01A")){ //open with measured Y 
      storeTime = "2021-12-01 00:01:00"; flavor="ofl";
      detpos.xoff[0]=-6.850*2.54-50; detpos.yoff[0]=-5.26; detpos.zoff[0]=279.592*2.54; //x= 17.399                                   
      detpos.xoff[1]= 6.850*2.54+50; detpos.yoff[1]=-5.26; detpos.zoff[1]=279.592*2.54;
      detpos.xoff[2]=-8.380*2.54-50; detpos.yoff[2]=+1.80; detpos.zoff[2]=308.123*2.54; //x= 21.285                                   
      detpos.xoff[3]= 8.380*2.54+50; detpos.yoff[3]=+1.80; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22Dec20")){ //closed
      storeTime = "2021-12-20 16:30:00"; flavor="ofl";
      detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=0.0; detpos.zoff[0]=279.592*2.54; //x= 17.399                                      
      detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=0.0; detpos.zoff[1]=279.592*2.54;
      detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=0.0; detpos.zoff[2]=308.123*2.54; //x= 21.285                                      
      detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=0.0; detpos.zoff[3]=308.123*2.54;
    }
    if(data.Contains("run22Dec20A")){ //closed with measured Y
      storeTime = "2021-12-20 16:30:10"; flavor="ofl";
      detpos.xoff[0]=-6.850*2.54; detpos.yoff[0]=-5.26; detpos.zoff[0]=279.592*2.54; //x= 17.399                                      
      detpos.xoff[1]= 6.850*2.54; detpos.yoff[1]=-5.26; detpos.zoff[1]=279.592*2.54;
      detpos.xoff[2]=-8.380*2.54; detpos.yoff[2]=+1.80; detpos.zoff[2]=308.123*2.54; //x= 21.285                                      
      detpos.xoff[3]= 8.380*2.54; detpos.yoff[3]=+1.80; detpos.zoff[3]=308.123*2.54;
    }

    if(storeTime==""){ 
	std::cout<<"Invalid year range"<<std::endl; 
	exit;
    }
    std::cout << "StoreTime="<<storeTime<<endl;
    std::cout << "Flavor   ="<<flavor<<endl;
    for(int det=0; det<4; det++){
	printf("INPUT det=%2d xoff=%8.3f yoff=%8.3f zoff=%8.3f\n",
	       det,detpos.xoff[det],detpos.yoff[det],detpos.zoff[det]);
    }
    
    if(option.Contains("writedb")) {
	gSystem->Setenv("DB_ACCESS_MODE","write");
	cout << "DB_ACCESS_MODE="<<gSystem->Getenv("DB_ACCESS_MODE")<<endl;
	StDbManager* mgr = StDbManager::Instance();
	StDbConfigNode* node = mgr->initConfig("Geometry_fcs");
	StDbTable* table = node->addDbTable("fcsDetectorPosition");
	mgr->setStoreTime(storeTime.Data());
	table->SetTable((char*)&detpos,1);
	table->setFlavor(flavor.Data());
	mgr->storeDbTable(table);
	gSystem->Unsetenv("DB_ACCESS_MODE");
	std::cout << "Done with database upload \n";
    }
    
    gSystem->Load("StChain");
    gSystem->Load("StBFChain");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("St_Tables");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    
    int date,time,from=0,n=0;
    TString datetime(storeTime),token;
    datetime.ReplaceAll("-","");
    datetime.ReplaceAll(":","");
    while(datetime.Tokenize(token,from," ")){
	if(n==0) date=atoi(token.Data());
	if(n==1) time=atoi(token.Data());
	n++;
    }
    std::cout << "Readout time="<<datetime<<" Date="<<date<<" Time="<<time<<endl;

    St_db_Maker *dbMk=new St_db_Maker("db", "MySQL:StarDb", "$STAR/StarDb");
    dbMk->SetDebug();
    dbMk->SetDateTime(date,time);
    dbMk->SetFlavor(flavor.Data()); 
    dbMk->Init();
    dbMk->Make();
    
    TDataSet *DB = 0;
    DB = dbMk->GetInputDB("Geometry/fcs");
    if(!DB){std::cout << "ERROR: no db maker or Geometry/fcs" << std::endl; }
    St_fcsDetectorPosition *dbTable = (St_fcsDetectorPosition*) DB->Find("fcsDetectorPosition");
    if(dbTable){
	std::cout << "Reading fcsDetectorPosition table from DB\n";
	fcsDetectorPosition_st *dbSt = dbTable->GetTable();      
	Int_t rows = dbTable->GetNRows();
	for(int i=0; i<rows; i++){
	    for(int det=0; det<4; det++){
		printf("DbRead row=%2d det=%2d xoff=%8.3f yoff=%8.3f zoff=%8.3f\n",
		       i,det,dbSt[i].xoff[det],dbSt[i].yoff[det],dbSt[i].zoff[det]);
	    }
	}
	memcpy(detpos,dbSt,sizeof(detpos));
    }else{
	std::cout << "WARNING: No data in fcsDetectorPosition table\n";
    }
}

