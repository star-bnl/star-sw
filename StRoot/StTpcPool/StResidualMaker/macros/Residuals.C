
class StChain;
StChain *chain=0;    
class StResidualMaker;
StResidualMaker* rm;

void Residuals(Int_t nevents=1,
	       const char *MainFile="./JustTheTens/*.root"
	       //	       const char *MainFile="/star/data05/reco/central/DEV01b/2000/08/*.dst.root"
	       )
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");  // new addition 22jul99
    gSystem->Load("StAnalysisUtilities");  // needed by V0dstMaker
    gSystem->Load("StMagF");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StResidualMaker");


    cout << "Dynamic loading done" << endl;

    chain = new StChain("StChain"); 
    chain->SetDebug();   



    // Now we add Makers to the chain...

    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug(); 
    ioMaker->SetIOMode("r"); 
    ioMaker->SetDebug();
    ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
    ioMaker->SetBranch("eventBranch",0,"r"); //activate EventBranch

    rm = new StResidualMaker();

    rm->SetNTpcHitsRange(20,80);   // cut on nhits
    rm->SetPtRange(0.1,0.6);       // cut on pT

  
    if (chain->Init()){ // This should call the Init() method in ALL makers
      cout << "Initialization failed \n";
      goto TheEnd;
    }
    chain->PrintInfo();


  // Event loop
    int istat=0,iev=1;
 EventLoop: if (iev <= nevents && !istat) {
   cout << "StHbtExample -- Working on eventNumber " << iev << " of " << nevents << endl;
   chain->Clear();
   istat = chain->Make(iev);
   if (istat) {cout << "Last event processed. Status = " << istat << endl;}
   iev++; goto EventLoop;
 }



  cout << "Residuals.C -- Done with event loop" << endl;

  chain->Finish(); // This should call the Finish() method in ALL makers

  rm->writeHistoFile();

 TheEnd:
}
