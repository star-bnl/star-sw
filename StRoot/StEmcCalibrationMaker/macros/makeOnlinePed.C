////////////////////////////////////////////////////////////////////

class StChain;
class StEmcOnl;
class St_db_Maker;        
class StEmcCalibrationMaker;
class StEmcPedestalMaker;
class StEmcEqualMaker;
class StEmcMipMaker;
StChain *chain=0;
StEmcOnl *emcIo = 0;
St_db_Maker *dbMk = 0;
StEmcCalibrationMaker *calib = 0;
StEmcPedestalMaker *ped[4];

TMemStat memory;

ped[0] = 0;
ped[1] = 0;
ped[2] = 0;
ped[3] = 0;

int nEventsProc = 0;
int NEVENTS = 2000;
bool ok[4];

bool processFile(char* line, char* dir)
{
	bool DONE = false;
  	cout <<"Processing file "<<line<<endl;
	emcIo->createEvPool(line,dir);
	int istat =0 ;
	int NE[4];
	int NP = 0;
	do
	{
    		chain->Clear();  
    		if(emcIo)
    		{
 			istat = emcIo->Make();
      			if(istat==0) 
      			{	 
        			for(int i=0;i<4;i++) if(ped[i]) NE[i] =  ped[i]->getNEvents();
				if(nEventsProc%100==0)
				{ 
					//cout << "---------------------- Online Calibration Event : " << nEventsProc+1 << " ----------------------" << endl;							
				 	//for(int i=0;i<4;i++) if(ped[i]) cout <<"  NEvents for "<<ped[i]->GetName()<<" = "<< NE[i] <<endl;
					cout << "Event " << nEventsProc << endl;
				}
				if(dbMk)   dbMk->Make();
				if(calib)  calib->Make();
		    		DONE = true;
				bool HASPROCESSED = false;
				for(int i=0;i<4;i++) 
			  	{
					if(ped[i] && !ok[i]) 
					{
						ped[i]->Make();
						if(ped[i]->getNEvents()>NE[i]) HASPROCESSED = true;
						if(ped[i]->getNEvents()>=(NEVENTS+10)) ok[i] = true;
					}
					if(!ok[i]) DONE=false;
				}
				if(DONE) goto EXIT;
				nEventsProc++;
				if(!HASPROCESSED) NP++; else NP = 0;
				if(NP>50) 
				{
					cout <<"Skiping this file. No bemc data have been processed for more than 50 events\n"; 
					return false;
				}
				if ((nEventsProc % 100) == 0) memory.PrintMem(0);
			}  
		}
	} while(istat==0 || istat==1);			
	
	return false;
	
	EXIT:
	return true;
}

void makeOnlinePed(char* list = "./runlist.txt", bool isList = true) {
    TDatime startTime;
    TStopwatch timer;
    cout << "Started: " << startTime.AsSQLString() << endl;
    timer.Start();
    memory.PrintMem(0);
    	   
    const Char_t *EVP_READER_LIB = gSystem->Getenv("EVP_READER_LIB");
    TString dirStr = gSystem->Getenv("EVP_DIR");
    TString savePathStr = gSystem->Getenv("EMCONLINE_PED_BACKUP_DIR");
    TString tablesPathStr = gSystem->Getenv("EMCONLINE_PED_TABLES_DIR");
    TString lastTablePathStr = gSystem->Getenv("EMCONLINE_PED_LASTTABLES_DIR");
    TString tempPathStr = gSystem->Getenv("EMCONLINE_PED_TEMP_DIR");
    TString pedCrateFilenameFormatStr = gSystem->Getenv("CRATE_PEDESTAL_FILES_FORMAT");
    TString bemcStatusStr = gSystem->Getenv("EMCONLINE_PED_BEMCSTATUS_FILE");

    TString neventsStr = gSystem->Getenv("EMCONLINE_PED_NEVENTS");
    Int_t nevents = neventsStr.Atoi();
    TString saveDbStr = gSystem->Getenv("EMCONLINE_PED_SAVEDB");
    Bool_t saveDb = (saveDbStr == "true");
    TString saveTablesStr = gSystem->Getenv("EMCONLINE_PED_SAVETABLES");
    Bool_t saveTables = (saveTablesStr == "true");
    TString compareLastTableDBStr = gSystem->Getenv("EMCONLINE_PED_COMPARELASTTABLEDB");
    Bool_t compareLastTableDB = (compareLastTableDBStr == "true");
    TString useBemcStatusStr = gSystem->Getenv("EMCONLINE_PED_USEBEMCSTATUS");
    Bool_t useBemcStatus = (useBemcStatusStr == "true");

    // BTOW
    TString minPedDiffDBStr_BTOW = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFDB_BTOW");
    Float_t minPedDiffDB_BTOW = minPedDiffDBStr_BTOW.Atof();
    TString minPedDiffNumStr_BTOW = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFNUM_BTOW");
    Int_t minPedDiffNum_BTOW = minPedDiffNumStr_BTOW.Atoi();
    TString minPedDiffMinTimeStr_BTOW = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFMINTIME_BTOW");
    Float_t minPedDiffMinTime_BTOW = minPedDiffMinTimeStr_BTOW.Atof();

    // BPRS
    TString minPedDiffDBStr_BPRS = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFDB_BPRS");
    Float_t minPedDiffDB_BPRS = minPedDiffDBStr_BPRS.Atof();
    TString minPedDiffNumStr_BPRS = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFNUM_BPRS");
    Int_t minPedDiffNum_BPRS = minPedDiffNumStr_BPRS.Atoi();
    TString minPedDiffMinTimeStr_BPRS = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFMINTIME_BPRS");
    Float_t minPedDiffMinTime_BPRS = minPedDiffMinTimeStr_BPRS.Atof();

    // BSMD
    TString minPedDiffDBStr_BSMD = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFDB_BSMD");
    Float_t minPedDiffDB_BSMD = minPedDiffDBStr_BSMD.Atof();
    TString minPedDiffNumStr_BSMD = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFNUM_BSMD");
    Int_t minPedDiffNum_BSMD = minPedDiffNumStr_BSMD.Atoi();
    TString minPedDiffMinTimeStr_BSMD = gSystem->Getenv("EMCONLINE_PED_MINPEDDIFFMINTIME_BSMD");
    Float_t minPedDiffMinTime_BSMD = minPedDiffMinTimeStr_BSMD.Atof();

    // Load needed shared libs
    gSystem->Load(EVP_READER_LIB);
    gSystem->Load("St_base");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StChain");
    gSystem->Load("StEvent");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StEmcOnlineUtil");
    gSystem->Load("StEmcCalibrationMaker");
 			
    memory.PrintMem(0);
    NEVENTS = nevents;

    cout << "Backup directory: " << savePathStr << endl;
    cout << "Backup tables directory: " << tablesPathStr << endl;
    cout << "Last tables directory: " << lastTablePathStr << endl;
    cout << "Temp directory: " << tempPathStr << endl;
    cout << "Event pool: " << dirStr << endl;	    
    cout << "bemcStatus.txt: " << bemcStatusStr << endl;	    
    cout << "Number of events to process: " << NEVENTS << endl;
    cout << "Save tables to DB: " << saveDb << endl;
    cout << "Save tables locally: " << saveTables << endl;
    cout << "Compare to the last saved table: " << compareLastTableDB << endl;
    cout << "Use bemcStatus.txt: " << useBemcStatus << endl;
    cout << "Ped crate filename format: " << pedCrateFilenameFormatStr << endl;
    cout << "BTOW:" << endl;
    cout << "Min ped diff from the last table: " << minPedDiffDB_BTOW << endl;
    cout << "Min ped diff num from the last table: " << minPedDiffNum_BTOW << endl;
    cout << "Min time from the last table: " << minPedDiffMinTime_BTOW << endl;
    cout << "BPRS:" << endl;
    cout << "Min ped diff from the last table: " << minPedDiffDB_BPRS << endl;
    cout << "Min ped diff num from the last table: " << minPedDiffNum_BPRS << endl;
    cout << "Min time from the last table: " << minPedDiffMinTime_BPRS << endl;
    cout << "BSMD:" << endl;
    cout << "Min ped diff from the last table: " << minPedDiffDB_BSMD << endl;
    cout << "Min ped diff num from the last table: " << minPedDiffNum_BSMD << endl;
    cout << "Min time from the last table: " << minPedDiffMinTime_BSMD << endl;
    
    // create chain ///////////////////////////////////////////////    
    chain = new StChain("StChain"); 
   
    // create StEmcOnl    
    emcIo = new StEmcOnl();
		//emcIo->setPrintInfo(kTRUE);
        
    // create database   
    dbMk = new St_db_Maker("StarDb","$STAR/StarDb","MySQL:StarDb");
    
    // create calibraton maker     
	calib = new StEmcCalibrationMaker();
		
	TString name[] = {"bemcPed", "bprsPed", "bsmdePed", "bsmdpPed"};
	for(int i=0;i<4;i++) 
	{ 
		ok[i] = false;
		ped[i] = new StEmcPedestalMaker(name[i].Data()); 
		ped[i]->setDetector(i+1);
		TString filename = tempPathStr + "/" + name[i] + ".root";
		ped[i]->setFile(filename);
		ped[i]->setNPedEvents(NEVENTS);
		ped[i]->setPedInterval(24*100);
		ped[i]->setRange(300);
		ped[i]->setAutoSaveDB(saveDb);
		//ped[i]->setCTBMax(3000);
		ped[i]->setSavePath(savePathStr.Data());
		ped[i]->setTablesPath(tablesPathStr.Data());
		ped[i]->setLastTablePath(lastTablePathStr.Data());
		ped[i]->setSaveTables(saveTables);
		ped[i]->setCompareLastTableDB(compareLastTableDB);
		ped[i]->setPedCrateFilenameFormat(pedCrateFilenameFormatStr.Data());
		ped[i]->setBemcStatusFilename(bemcStatusStr.Data());
		ped[i]->setUseBemcStatus(useBemcStatus);
		if (i == 0) {
		    ped[i]->setPedDiffSaveDB(minPedDiffDB_BTOW);
		    ped[i]->setPedDiffSaveNum(minPedDiffNum_BTOW);
		    ped[i]->setPedDiffSaveMinTime(minPedDiffMinTime_BTOW);
		} else if (i == 1) {
		    ped[i]->setPedDiffSaveDB(minPedDiffDB_BPRS);
		    ped[i]->setPedDiffSaveNum(minPedDiffNum_BPRS);
		    ped[i]->setPedDiffSaveMinTime(minPedDiffMinTime_BPRS);
		} else if ((i == 2) || (i == 3)) {
		    ped[i]->setPedDiffSaveDB(minPedDiffDB_BSMD);
		    ped[i]->setPedDiffSaveNum(minPedDiffNum_BSMD);
		    ped[i]->setPedDiffSaveMinTime(minPedDiffMinTime_BSMD);
		}
	}

    memory.PrintMem(0);
    chain->Init();
    memory.PrintMem(0);
    nEventsProc = 0;
		
	if(isList)
	{
	  ifstream INPUT(list);
	  Char_t line[1024];		
	  while(!INPUT.eof())
	  {
	    INPUT >> line;
		bool isDone = processFile(line,dirStr.Data());
		if(isDone) goto EXIT;
	  }
	}
	else processFile(list,dirStr.Data());
		
    EXIT:
    cout <<"FINISHED\n";
    memory.PrintMem(0);
    
    TDatime stopTime;
    timer.Stop();
    cout << "Finished: " << stopTime.AsSQLString() << endl;
    timer.Print();
}
