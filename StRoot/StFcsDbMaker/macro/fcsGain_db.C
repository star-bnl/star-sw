#include <iostream.h> 
#include <fstream.h>

void fcsGain_db(char* opt = "", char* input) {
    gROOT->Macro("LoadLogger.C");
    gSystem->Load("St_base.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("StDbLib.so");
    
    // structure to fill up
    fcsEcalGain_st egain;
    fcsHcalGain_st hgain;
    fcsPresGain_st pgain;
    fcsEcalGainCorr_st ecorr;
    fcsHcalGainCorr_st hcorr;
    fcsPresValley_st pcorr;
    
    TString option(opt);
    std::cout << "Opt =" << opt << "\n";
    std::cout << "writedb = " << option.Contains("writedb") << "\n";
    std::cout << "ecal    = " << option.Contains("ecal")    << "\n";
    std::cout << "hcal    = " << option.Contains("hcal")    << "\n";
    std::cout << "pres    = " << option.Contains("pres")    << "\n";
    std::cout << "ehp     = " << option.Contains("ehp")     << "\n";
    std::cout << "gain    = " << option.Contains("gain")     << "\n";
    std::cout << "corr    = " << option.Contains("corr")     << "\n";
    std::cout << "both    = " << option.Contains("both")     << "\n";
    
    int ecal=0, hcal=0, pres=0, gain=0, corr=0;
    if(option.Contains("ecal") || option.Contains("ehp")) ecal=1;
    if(option.Contains("hcal") || option.Contains("ehp")) hcal=1;
    if(option.Contains("pres") || option.Contains("ehp")) pres=1;
    if(option.Contains("gain") || option.Contains("both")) gain=1;
    if(option.Contains("corr") || option.Contains("both")) corr=1;

    TString data(input);
    TString storeTime("");
    TString flavor;
    if(data.Contains("run22sim")){ 
	storeTime = "2021-10-15 00:00:00"; flavor="sim"; 
	if(ecal && gain) for(int i=0; i<1496; i++) egain.gain[i]=0.0053; //default 5.3MeV/ch
	if(hcal && gain) for(int i=0; i< 520; i++) hgain.gain[i]=0.0053; //default 5.3MeV/ch
	if(pres && gain) for(int i=0; i< 384; i++) pgain.gain[i]=0.01;   //100ch for MIP
	if(ecal && corr) for(int i=0; i<1496; i++) ecorr.gaincorr[i]=1.0;//default 1
	if(hcal && corr) for(int i=0; i< 520; i++) hcorr.gaincorr[i]=1.0;//default 1
	if(pres && corr) for(int i=0; i< 384; i++) pcorr.valley[i]=0.5;  //0.5 for 1/2 MIP
    }
    if(data.Contains("run22ofl")){  
	storeTime = "2021-10-25 00:00:10"; flavor="ofl";
	if(ecal && gain) for(int i=0; i<1496; i++) egain.gain[i]=0.0053/5.31; 
	if(hcal && gain) for(int i=0; i< 520; i++) hgain.gain[i]=0.0053*1.3*1.21*1.65;     
	if(pres && gain) for(int i=0; i< 384; i++) pgain.gain[i]=1.0/600.0;  //600ch for MIP
	if(ecal && corr) for(int i=0; i<1496; i++) ecorr.gaincorr[i]=1.21;   //avg gaincorr=1.21
	if(hcal && corr) for(int i=0; i< 520; i++) hcorr.gaincorr[i]=1.0;    
	if(pres && corr) for(int i=0; i< 384; i++) pcorr.valley[i]=0.5;      //0.5 for 1/2 MIP
    } 
    if(data.Contains("run22Dec01")){
      storeTime = "2021-12-01 00:00:10 "; flavor="ofl";
      if(ecal && gain) for(int i=0; i<1496; i++) egain.gain[i]=0.0053/5.31*2.7;  //Attenuator 1/2.7
    }
    if(data.Contains("run22Dec21")){
      storeTime = "2021-12-22 03:50:10 "; flavor="ofl";
      if(ecal && gain) for(int i=0; i<1496; i++) egain.gain[i]=0.0053;  //Attenuator 1/5.31
    }
    if(data.Contains("run22Jan27")){
      storeTime = "2022-01-28 01:33:00 "; flavor="ofl";
      if(hcal && gain) for(int i=0; i< 520; i++) hgain.gain[i]=0.0053*1.3*1.21; //increased V for *1.65 gain
    }

    if(storeTime==""){
	std::cout<<"Invalid year range"<<std::endl; 
	exit;
    }
    std::cout << "StoreTime="<<storeTime<<endl;
    std::cout << "Flavor   ="<<flavor<<endl;
    if(ecal && gain) printf("INPUT EcalGain %f\n",egain.gain[0]);
    if(hcal && gain) printf("INPUT HcalGain %f\n",hgain.gain[0]);
    if(pres && gain) printf("INPUT PresGain %f\n",pgain.gain[0]);
    if(ecal && corr) printf("INPUT EcalGainCorr %f\n",ecorr.gaincorr[0]);
    if(hcal && corr) printf("INPUT HcalGainCorr %f\n",hcorr.gaincorr[0]);
    if(pres && corr) printf("INPUT PresValley %f\n",pcorr.valley[0]);
    
    if(option.Contains("writedb")) {
	gSystem->Setenv("DB_ACCESS_MODE","write");
	cout << "DB_ACCESS_MODE="<<gSystem->Getenv("DB_ACCESS_MODE")<<endl;
	StDbManager* mgr = StDbManager::Instance();
	StDbConfigNode* node = mgr->initConfig("Calibrations_fcs");
	mgr->setStoreTime(storeTime.Data());	  
	if(ecal && gain){
	  StDbTable* table = node->addDbTable("fcsEcalGain");
	  table->SetTable((char*)&egain,1);
	  table->setFlavor(flavor.Data());
	  mgr->storeDbTable(table);
	}
	if(hcal && gain){
	  StDbTable* table = node->addDbTable("fcsHcalGain");
	  table->SetTable((char*)&hgain,1);
	  table->setFlavor(flavor.Data());
	  mgr->storeDbTable(table);
	}
	if(pres && gain){
	  StDbTable* table = node->addDbTable("fcsPresGain");
	  table->SetTable((char*)&pgain,1);
	  table->setFlavor(flavor.Data());
	  mgr->storeDbTable(table);
	}
	if(ecal && corr){
	  StDbTable* table = node->addDbTable("fcsEcalGainCorr");
	  table->SetTable((char*)&ecorr,1);
	  table->setFlavor(flavor.Data());
	  mgr->storeDbTable(table);
	}
	if(hcal && corr){
	  StDbTable* table = node->addDbTable("fcsHcalGainCorr");
	  table->SetTable((char*)&hcorr,1);
	  table->setFlavor(flavor.Data());
	  mgr->storeDbTable(table);
	}
	if(pres && corr){
	  StDbTable* table = node->addDbTable("fcsPresValley");
	  table->SetTable((char*)&pcorr,1);
	  table->setFlavor(flavor.Data());
	  mgr->storeDbTable(table);
	}
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
    DB = dbMk->GetInputDB("Calibrations/fcs");
    if(!DB){std::cout << "ERROR: no db maker or Calibrations/fcs" << std::endl; }
    if(ecal && gain){
      St_fcsEcalGain *dbTable_eg = (St_fcsEcalGain*) DB->Find("fcsEcalGain");
      if(dbTable_eg){
	std::cout << "Reading fcsEcalGain table from DB\n";
	fcsEcalGain_st *dbSt_eg = dbTable_eg->GetTable();      
	Int_t rows = dbTable_eg->GetNRows();
	for(int i=0; i<rows; i++){
	  for(int id=0; id<1496; id++){
	    printf("DbRead row=%2d id=%d gain=%10.6f\n",
		   i,id,dbSt_eg[i].gain[id]);
	  }
	}
      }else{
	std::cout << "WARNING: No data in fcsEcalGain table\n";
      }
    }
    if(hcal && gain){
      St_fcsHcalGain *dbTable_hg = (St_fcsHcalGain*) DB->Find("fcsHcalGain");
      if(dbTable_hg){
	std::cout << "Reading fcsHcalGain table from DB\n";
	fcsHcalGain_st *dbSt_hg = dbTable_hg->GetTable();      
	Int_t rows = dbTable_hg->GetNRows();
	for(int i=0; i<rows; i++){
	  for(int id=0; id<520; id++){
	    printf("DbRead row=%2d id=%d gain=%10.6f\n",
		   i,id,dbSt_hg[i].gain[id]);
	  }
	}
      }else{
	std::cout << "WARNING: No data in fcsHcalGain table\n";
      }
    }
    if(pres && gain){
      St_fcsPresGain *dbTable_pg = (St_fcsPresGain*) DB->Find("fcsPresGain");
      if(dbTable_pg){
	std::cout << "Reading fcsPresGain table from DB\n";
	fcsPresGain_st *dbSt_pg = dbTable_pg->GetTable();      
	Int_t rows = dbTable_pg->GetNRows();
	for(int i=0; i<rows; i++){
	  for(int id=0; id<384; id++){
	    printf("DbRead row=%2d id=%d gain=%10.6f\n",
		   i,id,dbSt_pg[i].gain[id]);
	  }
	}
      }else{
	std::cout << "WARNING: No data in fcsPresGain table\n";
      }
    }
    if(ecal && corr){
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
    if(hcal && corr){
      St_fcsHcalGainCorr *dbTable_hc = (St_fcsHcalGainCorr*) DB->Find("fcsHcalGainCorr");
      if(dbTable_hc){
	std::cout << "Reading fcsHcalGainCorr table from DB\n";
	fcsHcalGainCorr_st *dbSt_hc = dbTable_hc->GetTable();      
	Int_t rows = dbTable_hc->GetNRows();
	for(int i=0; i<rows; i++){
	  for(int id=0; id<520; id++){
	    printf("DbRead row=%2d id=%d gaincorr=%10.6f\n",
		   i,id,dbSt_hc[i].gaincorr[id]);
	  }
	}
      }else{
	std::cout << "WARNING: No data in fcsHcalGainCorr table\n";
      }
    }
    if(pres && corr){
      St_fcsPresValley *dbTable_pc = (St_fcsPresValley*) DB->Find("fcsPresValley");
      if(dbTable_pc){
	std::cout << "Reading fcsPresValley table from DB\n";
	fcsPresValley_st *dbSt_pc = dbTable_pc->GetTable();      
	Int_t rows = dbTable_pc->GetNRows();
	for(int i=0; i<rows; i++){
	  for(int id=0; id<384; id++){
	    printf("DbRead row=%2d id=%d valley=%10.6f\n",
		   i,id,dbSt_pc[i].valley[id]);
	  }
	}
      }else{
	std::cout << "WARNING: No data in fcsPresValley table\n";
      }
    }
}

