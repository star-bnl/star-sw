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
		, char* dir = "/evp"
		, char *savePath = "./backup"
		, char *tablesPath = "./tables"
		, char *tempPath = "./tmp"
		, char *EVP_READER_LIB = "libevpSO.2.0.so"
		, Int_t nevents = 2000
		, Bool_t saveDb = true
		, Bool_t saveTables = true
		) {
    TDatime startTime;
    TStopwatch timer;
    cout << "Started: " << startTime.AsSQLString() << endl;
    timer.Start();
    memory.PrintMem(0);
    	   
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
    TString savePathStr = savePath;
    TString tablesPathStr = tablesPath;
    TString tempPathStr = tempPath;
    NEVENTS = nevents;
    cout << "Backup directory: " << savePathStr << endl;
    cout << "Backup tables directory: " << tablesPathStr << endl;
    cout << "Temp directory: " << tempPathStr << endl;
    cout << "Event pool: " << dir << endl;	    
    cout << "Number of events to process: " << NEVENTS << endl;
    cout << "Save tables to DB: " << saveDb << endl;
    cout << "Save tables locally: " << saveTables << endl;
    
    // create chain ///////////////////////////////////////////////    
    chain = new StChain("StChain"); 
   
    // create StEmcOnl    
    emcIo = new StEmcOnl();
		//emcIo->setPrintInfo(kTRUE);
        
    // create database   
    dbMk = new St_db_Maker("StarDb","MySQL:StarDb");  
    
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
		ped[i]->setSaveTables(saveTables);
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
		bool isDone = processFile(line,dir);
		if(isDone) goto EXIT;
	  }
	}
	else processFile(list,dir);
		
    EXIT:
    cout <<"FINISHED\n";
    memory.PrintMem(0);
    
    TDatime stopTime;
    timer.Stop();
    cout << "Finished: " << stopTime.AsSQLString() << endl;
    timer.Print();
}
