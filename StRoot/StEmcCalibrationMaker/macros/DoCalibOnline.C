////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;
class StEmcOnl;
StEmcOnl *emcIo = 0;
class St_db_Maker;        
St_db_Maker *dbMk = 0;
class StEmcCalibrationMaker;
StEmcCalibrationMaker *calib = 0;
class StSmdPedMaker;
StSmdPedMaker *smdPed = 0;

int nEventsProc = 0;
int MaxEvents = 0;
int ntries = 0;
int delaytime = 10;
int delay = delaytime;

Bool_t realOnline = kFALSE;

TTimer time(delaytime);

void Start(int miliseconds = 10) { time.Start(miliseconds,kFALSE); delaytime = miliseconds; ntries=0; }

void Stop() { time.Stop(); }

void Reset(char* file = "", Int_t nevents = 0)
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
    emcIo->createEvPool(file,"/evp");
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
    
    Bool_t isFake= kTRUE;
    
    calib->SetDoEqual(EQUAL);           
    calib->SetDoGain(GAIN);            
    calib->SetDoMip(MIP);             
    calib->SetDoPed(PED);   
    calib->SetFakeRun(isFake);
              
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
    
    if(smdPed) 
    {
      smdPed->SetSavePedToDB(kFALSE);
      smdPed->SetFakeRun(isFake);
    }
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
      ped->SetMinHits(2000);
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
  
}

void ProcessEvent()
{  
  int istat=0;
  chain->Clear();
  
  if(emcIo)
  {
    istat = emcIo->Make(); // This should call the Make() method in ALL makers
    if(istat!=2 && istat!=3 && istat!=4) 
    {
        cout << "---------------------- Processing Event : " << nEventsProc+1 << " ----------------------" << endl;
        dbMk->Make();
        calib->Make();
        smdPed->Make();
        nEventsProc++;
    }
  } else istat =4;

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
    if(delay!=delaytime) cout <<" EMC Online CALIBRATION: No events in event pool... ntries = "<<ntries<<". will try again in "<<(float)delay/1000.0<<" seconds \n";
  }

  time.Start(delay,kFALSE);
  
  if(ntries==0)    delay = delaytime;
  if(ntries>1000)  delay = 1000;
  if(ntries>1050)  delay = 10000;
  if(ntries>1100)  delay = 60000;
  
  if(istat==3 || istat==4) time.Stop();
  
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
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("/daqman/RTS/lib/LINUX/libevpSO.2.0.so");
    gSystem->Load("StEmcOnlineUtil");
    gSystem->Load("StEmcCalibrationMaker");


    Reset(file, nevents); // create and initializs chain
    
///////////////////////////////////////////////////////////////
    MaxEvents = nevents;
    
    time.SetCommand("ProcessEvent()");
    time.Start(delaytime,kFALSE);
}

