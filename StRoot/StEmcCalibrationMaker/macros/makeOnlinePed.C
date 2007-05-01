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

void makeOnlinePed(char* list = "./runlist.txt"
		, bool isList = true
		, Int_t nevents = 2000
		, Bool_t saveDb = true
		, Bool_t saveTables = true
		, Bool_t compareLastTableDB = false
		, Float_t minPedDiffDB = 1.0
		) {
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
    cout << "Number of events to process: " << NEVENTS << endl;
    cout << "Save tables to DB: " << saveDb << endl;
    cout << "Save tables locally: " << saveTables << endl;
    cout << "Compare to the last saved table: " << compareLastTableDB << endl;
    cout << "Min ped diff from the last table: " << minPedDiffDB << endl;
    cout << "Ped crate filename format: " << pedCrateFilenameFormatStr << endl;
    
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
		ped[i]->setPedDiffSaveDB(minPedDiffDB);
		ped[i]->setPedCrateFilenameFormat(pedCrateFilenameFormatStr.Data());
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
