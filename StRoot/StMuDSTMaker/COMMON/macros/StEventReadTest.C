// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    



//==========================================================================================
//==========================================================================================
void StEventReadTest(const Char_t *file="/star/data16/reco/dAuMinBias/ReversedFullField/P03ia/2003/019/st_physics_4019019_raw_0040020.event.root")
{
    // Dynamically link needed shared libs
    gSystem->Load("libTable");
    gSystem->Load("libPhysics");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");        // new addition 22jul99
    gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
    gSystem->Load("StBichsel");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");
    gSystem->Load("StEmcUtil");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");  
    cout << " loading of shared libraries done" << endl;
    
    chain = new StChain("StChain"); 
    chain->SetDebug();
    
    StFile *setFiles= new StFile();
    setFiles->AddFile(file);
    
    // ********************************
    // Now we add Makers to the chain... 
    // ********************************
    
    // *************
    // file handling
    // *************
    StIOMaker* ioMaker = new StIOMaker("IOMaker","r",setFiles,"bfcTree");
    ioMaker->SetDebug();
    
    ioMaker->SetIOMode("r");
    ioMaker->SetDebug();
    ioMaker->SetBranch("*",0,"0");         //deactivate all branches
    ioMaker->SetBranch("eventBranch",0,"r"); //activate evt.root Branch
    ioMaker->SetBranch("emcBranch",0,"r");   //activate evt.root Branch
    ioMaker->SetBranch("runcoBranch",0,"r"); //activate evt.root Branch
    
    // *****************
    // now run the chain
    // *****************
    chain->Init(); // This should call the Init() method in ALL makers
    chain->PrintInfo();
    int iret=0;
    int iev=0;
    while (iret==0 && iev++ <10) {
	cout << "StExample -- Working on eventNumber " << iev++ << endl;
	chain->Clear();
	iret = chain->Make(iev); // This should call the Make() method in ALL makers    
    } // Event Loop
    chain->Finish(); // This should call the Finish() method in ALL makers
    
    cout << " End of Analysis " << endl;
}



