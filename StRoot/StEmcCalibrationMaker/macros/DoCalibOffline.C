
  
////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

void DoCalibOffline(Int_t nevents=1)
{
// Load needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StIOMaker");
    gSystem->Load("StMagF");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("StEvent");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("St_Tables");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker"); 
    gSystem->Load("St_db_Maker"); 
    
		gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");

// Load my maker
    gSystem->Load("StEmcNewCalib");

// create chain    
    chain = new StChain("StChain"); 
    chain->SetDebug();
   
// Now we add Makers to the chain...

    StEmcMicroDstMaker *microDst = new StEmcMicroDstMaker();
    //microDst->addMicroEventFile("/star/u/suaide/emc/calibration/minbias/FullField/runs/*.emcEvent.root");
    microDst->addMicroEventFile("/star/u/suaide/emc/calibration/minbias/FullField/runs/*00.emcEvent.root");
    //microDst->addMicroEventFile("/star/u/suaide/emc/calibration/minbias/FullField/runs/*1.emcEvent.root");
    //microDst->addMicroEventFile("/star/u/suaide/emc/calibration/minbias/FullField/runs/*2.emcEvent.root");
    //microDst->addMicroEventFile("/star/u/suaide/emc/calibration/minbias/FullField/runs/*3.emcEvent.root");
    //microDst->addMicroEventFile("/star/u/suaide/emc/calibration/minbias/FullField/runs/*4.emcEvent.root");
   
    St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");       
    StEmcCalibrationMaker *calib = new StEmcCalibrationMaker("emccalib"); 
    
// now execute the chain Init functions

    chain->PrintInfo();
    Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
    if (initStat) chain->Fatal(initStat, "during Init()");
    
    int istat=0,iev=1;

// do the event loop    
    Int_t MaxEvents=microDst->getNEvents();
EventLoop: 
     if (iev<=nevents && istat!=2 && istat!=4 && iev<=MaxEvents) 
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

