
  
////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

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
    gSystem->Load("StEmcNewCalib");

// create chain ///////////////////////////////////////////////    
    chain = new StChain("StChain"); 
   
    // create Event pool interface
    evpReader *evp;
    if(!strcmp(file,"")) 
    {
      evp = new evpReader(NULL);
      evp->setEvpDisk("/evp");
    }
    else evp = new evpReader(file);

    // create StEmcOnl    
    StEmcOnl *emcIo = new StEmcOnl();
    emcIo->setEvPool(evp);
    
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
    
    calib->SetSavePedToDB(kTRUE);
    calib->SetSaveCalibToDB(kFALSE);
    
    if(smdPed) smdPed->SetSavePedToDB(kTRUE);

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
      ped = calib->GetPedSpectra();
      ped->SetMinHits(4000);
    }
    if(GAIN)
    {
      gain = calib->GetGainSpectra();
      gain->SetMinHits(4000);
    }
    if(EQUAL)
    {
      equal = calib->GetEqualSpectra();
      equal->SetMinHits(4000);      
    }
    if(MIP)
    {
      mip = calib->GetMipSpectra();
      mip->SetMinHits(4000);      
      mip->SetMaxMultiplicity(1000);
      mip->SetMinMomentum(1.2);
    }
    
///////////////////////////////////////////////////////////////
    
    int istat=0,iev=1;
EventLoop: 
     if ((iev<=nevents && nevents!=0) && istat!=2 && istat!=4) 
     {
       chain->Clear();
       cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
       istat = chain->Make(iev); // This should call the Make() method in ALL makers
       if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
       if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
       if (istat == 4) { cout << "Fatal Event Processed. Status = " << istat << endl; }
       iev++; 
       goto EventLoop;
     } // Event Loop
     
     chain->Finish();
}

