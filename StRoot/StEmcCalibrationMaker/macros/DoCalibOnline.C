////////////////////////////////////////////////////////////////////

class StChain;
class StEmcOnl;
class St_db_Maker;        
class StEmcCalibrationMaker;
class StSmdPedMaker;
StChain *chain=0;
StEmcOnl *emcIo = 0;
St_db_Maker *dbMk = 0;
StEmcCalibrationMaker *calib = 0;
StSmdPedMaker *smdPed = 0;

int nEventsProc = 0;
int MaxEvents = 0;
int ntries = 0;
int delaytime = 1;
int delay = delaytime;

Bool_t realOnline = kFALSE;
Bool_t debug = kFALSE;

TTimer time(delaytime);
time.Stop();

void Fake() 																			{ if(calib) calib->SetFakeRun(kTRUE);  if(smdPed) smdPed->SetFakeRun(kTRUE);}
void Real() 																			{ if(calib) calib->SetFakeRun(kFALSE); if(smdPed) smdPed->SetFakeRun(kFALSE);}
void SetField(float field)  											{ if(emcIo) emcIo->setField(field);}
void Start(int miliseconds = 1) 									{ time.Start(miliseconds,kFALSE); delaytime = miliseconds; ntries=0; }
void Stop() 																			{ time.Stop(); }
void NextEvent()  																{ int istat = ProcessEvent(); time.Start(delay,kFALSE); }
void ProcessNewFile(char* file,char* dir = "/evp"){ realOnline = kFALSE; if(emcIo) { emcIo->createEvPool(file,dir); Start(delaytime); } }
void SetDebug(Bool_t d)                           { debug = d; }
void GoOnline()                                   { realOnline = kTRUE; if(emcIo) { emcIo->createEvPool("","/evp"); Start(delaytime); } }

void ProcessAll(char* dirN)
{
	time.Stop();
	realOnline = kFALSE;
	int nfiles = 0;
	gSystem->ChangeDirectory(dirN);
	void *dir = gSystem->OpenDirectory(dirN);
	const char* entry;
	do
	{
		entry = gSystem->GetDirEntry(dir);
		if(entry)
		{
			char completeName[200];
			sprintf(completeName,"%s%s",dirN,entry);
			emcIo->createEvPool(completeName,"");
			int istat =0 ;
			do
			{
			  cout <<"Reading from file "<<entry<<endl;
				istat = ProcessEvent();
				time.Stop();
			} while(istat==0);
		}
	} while(entry!=0);
}

void Reset(char* file = "", char* dir = "/evp", Int_t nevents = 0)
{
    time.Stop();
    
// delete old chain  
    if(smdPed)  delete smdPed;
    if(calib)   delete calib;
    if(dbMk)    delete dbMk;
    if(emcIo)   delete emcIo;
    if(chain)   delete chain;
    
// create chain ///////////////////////////////////////////////    
    chain = new StChain("StChain"); 
   
    // create StEmcOnl    
    emcIo = new StEmcOnl();
    emcIo->createEvPool(file,dir);
//    emcIo->setPrintInfo(kFALSE);
    if(!strcmp(file,"")) realOnline = kTRUE;
        
    // create database   
    dbMk = new St_db_Maker("StarDb","MySQL:StarDb");  
    
    // create calibraton maker     
    calib = new StEmcCalibrationMaker(); 
    smdPed = new StSmdPedMaker();
    
///////////////////////////////////////////////////////////////
    
// now configure the options on calibration maker

    // global calibration parameters (done before Init())
    
    Bool_t PED   = kTRUE;
    Bool_t EQUAL = kTRUE;
    Bool_t GAIN  = kTRUE;
    Bool_t MIP   = kTRUE;
    
    Bool_t isFake= kFALSE;
    
    calib->SetDoEqual(EQUAL);           
    calib->SetDoGain(GAIN);            
    calib->SetDoMip(MIP);             
    calib->SetDoPed(PED);   
    calib->SetFakeRun(isFake);
              
    calib->SetDoUseL3(kTRUE);           
    calib->SetSubPedestal(kTRUE);
    calib->SetPedInterval(12); //in hours
    calib->SetWaitForPed(kFALSE);        
    calib->SetUseLocalPed(kFALSE);       
    calib->SetGainMode(0);           
    calib->SetDetNum(0);                        
    calib->SetZVertexMax(40);       
    calib->SetNEtaBins(10);           
    calib->SetEtaBinSize(2);         
		
		calib->SetDir("~/EmcOnline/Calibration/");
    
    // options for SMD pedestal calculator
    if(smdPed)
    {
      smdPed->SetPedInterval(12); //in hours
      smdPed->SetMinEvents(5000);
    }
    // options fto save information on OFFLINE DATABASE
    
    calib->SetSavePedToDB(kTRUE);
    calib->SetSaveCalibToDB(kFALSE);
    
    if(smdPed) 
    {
      smdPed->SetSavePedToDB(kTRUE);
      smdPed->SetFakeRun(isFake);
    }
    // initializing chain
    Int_t initStat = chain->Init(); 
    if (initStat) chain->Fatal(initStat, "during Init()");
    chain->PrintInfo();
		
    // specific paremeters should be done here after chain is initialized
    StEmcPedSpectra    *ped   = NULL;
    StEmcEqualSpectra  *equal = NULL;
    StEmcEqualSpectra  *gain  = NULL;
    StEmcMipSpectra    *mip   = NULL;
    
    if(PED)
    {
      ped = calib->GetPedSpec();
      ped->SetMinHits(2000);
    }
    if(GAIN)
    {
      gain = calib->GetGainSpec();
      gain->SetMinHits(4000000);
    }
    if(EQUAL)
    {
      equal = calib->GetEqualSpec();
      equal->SetMinHits(4000000);      
    }
    if(MIP)
    {
      mip = calib->GetMipSpec();
      mip->SetMinHits(4000000);      
      mip->SetMaxMultiplicity(1000);
      mip->SetMinMomentum(1.2);
    }
  
}
int ProcessEvent()
{  
	int istat=0;
  chain->Clear();
  
  if(emcIo)
  {
		istat = emcIo->Make(); // This should call the Make() method in ALL makers
		if(debug) cout <<"ntries = "<<ntries<<"  istat = "<<istat<<endl;
		ntries++;
		if(istat==1) return 1;
    if(istat==0) 
    {
        cout << "---------------------- Processing Event : " << nEventsProc+1 << " ----------------------" << endl;
        if(dbMk)   dbMk->Make();
        if(calib)  calib->Make();
        if(smdPed) smdPed->Make();
        nEventsProc++;
    }
  } else istat =4;

  if(!realOnline && (istat>0 || (nEventsProc>=MaxEvents && MaxEvents!=0))) // reading file.... should stop
  {
     time.Stop();
     if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
     if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
     if (istat == 4) { cout << "Fatal Event Processed. Status = " << istat << endl; }
     if (nEventsProc>=MaxEvents && MaxEvents!=0) { cout << "Last Event requested" <<  endl; }
     time.Stop();
		 return istat;
	}
	
	if(realOnline && istat>1)
	{
		if(debug) cout <<"Online problem istat = "<<istat<<"  ntries = "<<ntries<<endl;
		if(emcIo) emcIo->createEvPool("","/evp");
	}

  if(istat == 0) ntries = 0;
  else 
  {
    ntries++;
    if(debug) if(delay>0) cout <<" EMC Online CALIBRATION: No events in event pool... ntries = "<<ntries<<". will try again in "<<(float)delay/1000.0<<" seconds \n";
  }

  
  if(ntries==0)     delay = delaytime;
  if(ntries>1000)  delay = 1000;
  //if(ntries>11000)  delay = 10000;
  //if(ntries>11000)  delay = 60000;
  
  //if((istat==3 || istat==4) && !realOnline) time.Stop();
	  
  return istat;
}

void DoCalibOnline(char* file = "", char* dir = "/evp", Int_t nevents = 0)
{
// Load needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDAQMaker");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StMagF");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("StEvent");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker"); 
    gSystem->Load("St_db_Maker"); 
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("/daqman/RTS/lib/LINUX/libevpSO.2.0.so");
    gSystem->Load("StEmcOnlineUtil");
    gSystem->Load("StEmcCalibrationMaker");


    Reset(file, dir, nevents); // create and initializs chain
    
///////////////////////////////////////////////////////////////
    MaxEvents = nevents;
    
    time.SetCommand("NextEvent()");
    time.Start(delaytime,kFALSE);
}

