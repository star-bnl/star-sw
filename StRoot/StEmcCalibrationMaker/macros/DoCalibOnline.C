
  
////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

int nEventsProc = 0;
int MaxEvents = 0;
int ntries = 0;
int delay = 1;

Bool_t realOnline = kFALSE;

TTimer time(1);

void Start(int miliseconds = 1) { time.Start(miliseconds,kFALSE); }

void Stop() { time.Stop(); }

void ProcessEvent()
{
  
  int istat=0;
  chain->Clear();
  cout << "---------------------- Processing Event : " << nEventsProc+1 << " ----------------------" << endl;
  istat = chain->Make(nEventsProc); // This should call the Make() method in ALL makers
  nEventsProc++;
  
  if(!realOnline && (istat==2 || istat ==4 || (nEventsProc>=MaxEvents && MaxEvents!=0))) // reading file.... should stop
  {
    time.Stop();
    if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
    if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
    if (istat == 4) { cout << "Fatal Event Processed. Status = " << istat << endl; }
    if (nEventsProc>=MaxEvents && MaxEvents!=0) { cout << "Last Event requested" <<  endl; }
  }

  if(istat == 0) ntries = 0;
  else 
  {
    ntries++;
    cout <<"No events in event pool... ntries = "<<ntries<<". will try again in "<<(float)delay/1000.0<<" seconds \n";
  }

  time.Start(delay,kFALSE);
  
  if(ntries==0)  delay = 1;
  if(ntries>10)  delay = 1000;
  if(ntries>50)  delay = 10000;
  if(ntries>100) delay = 60000;
  
  return;
}

void DoCalibOnline(char* file = "", Int_t nevents = 0)
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
    
    gSystem->Load("/daqman/RTS/lib/LINUX/libevpSO.so");
    gSystem->Load("StEmcADCtoEMaker");
  
    gSystem->Load("StEmcOnlineUtil");
    gSystem->Load("StEmcCalibrationMaker");

// create chain ///////////////////////////////////////////////    
    chain = new StChain("StChain"); 
   
    // create StEmcOnl    
    StEmcOnl *emcIo = new StEmcOnl();
    emcIo->createEvPool(file,"/evp");
    emcIo->setPrintInfo(kFALSE);
    if(!strcmp(file,"")) realOnline = kTRUE;
    
    
    // create database   
    St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");  
    
    // create calibraton maker     
    StEmcCalibrationMaker *calib = new StEmcCalibrationMaker(); 
    StSmdPedMaker *smdPed = new StSmdPedMaker();
    
///////////////////////////////////////////////////////////////
    
// now configure the options on calibration maker

    // global calibration parameters (done before Init())
    
    Bool_t PED   = kTRUE;
    Bool_t EQUAL = kTRUE;
    Bool_t GAIN  = kTRUE;
    Bool_t MIP   = kTRUE;
    
		calib->SetDoEqual(EQUAL);           
    calib->SetDoGain(GAIN);            
    calib->SetDoMip(MIP);             
    calib->SetDoPed(PED);   
              
		calib->SetDoUseL3(kTRUE);           
    calib->SetSubPedestal(kTRUE);
    calib->SetPedInterval(6); //in hours
    calib->SetWaitForPed(kTRUE);        
    calib->SetUseLocalPed(kTRUE);       
    calib->SetGainMode(0);           
		calib->SetDetNum(0);                        
		calib->SetZVertexMax(40);       
    calib->SetNEtaBins(5);           
    calib->SetEtaBinSize(4);         
    
    // options for SMD pedestal calculator
    if(smdPed)
    {
      smdPed->SetPedInterval(6); //in hours
      smdPed->SetMinEvents(10000);
    }
    // options fto save information on OFFLINE DATABASE
    
    calib->SetSavePedToDB(kFALSE);
    calib->SetSaveCalibToDB(kFALSE);
    
    if(smdPed) smdPed->SetSavePedToDB(kFALSE);

    // initializing chain
    Int_t initStat = chain->Init(); 
    if (initStat) chain->Fatal(initStat, "during Init()");
    
    // specific paremeters should be done here after chain is initialized
    StEmcPedSpectra    *ped   = NULL;
    StEmcEqualSpectra  *equal = NULL;
    StEmcEqualSpectra  *gain  = NULL;
    StEmcMipSpectra    *mip   = NULL;
    
    if(PED)
    {
      ped = calib->GetPedSpec();
      ped->SetMinHits(4950);
    }
    if(GAIN)
    {
      gain = calib->GetGainSpec();
      gain->SetMinHits(4000);
    }
    if(EQUAL)
    {
      equal = calib->GetEqualSpec();
      equal->SetMinHits(4000);      
    }
    if(MIP)
    {
      mip = calib->GetMipSpec();
      mip->SetMinHits(4000);      
      mip->SetMaxMultiplicity(1000);
      mip->SetMinMomentum(1.2);
    }
    
///////////////////////////////////////////////////////////////
    MaxEvents = nevents;
    
    time.SetCommand("ProcessEvent()");
    time.Start(1,kFALSE);
     
     //chain->Finish();
}

