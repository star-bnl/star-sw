// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    


//=============================================================================
void StEztMaker(const Int_t nevents=100,
	            const Char_t *path="/star/data44/reco/ppMinBias/ReversedFullField/P04ij/2004/112/",
	            const Char_t *file="st_physics_5112018_raw_1020002.event.root",	
		    const Char_t* outDir="./",		
		    const Char_t* outFile="dummy",
		    const Char_t* appendix="test.microDst")		
{ 


  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  StEztMaker(nevents,fileListQQ,outDir,outFile);
}

//=============================================================================
void StEztMaker(const Int_t nevents, const Char_t **fileList, const Char_t* dirName, const Char_t* fileName)
{
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");

  chain = new StChain("StChain"); 
  chain->SetDebug();


  StFile *setFiles= new StFile();
  for (int ifil=0; fileList[ifil]; ifil++)
    setFiles->AddFile(fileList[ifil]);


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
  
  // need strange mudstmaker for mudst to work
  StStrangeMuDstMaker* v0dst = new StStrangeMuDstMaker("strangeMuDst");
  v0dst->DoV0(); //Set v0MiniDstMaker to find only v0s
  v0dst->DoXi(); //Set v0MiniDstMaker to find only v0s
  v0dst->DoKink(); //Set v0MiniDstMaker to find only v0s
  v0dst->SetNoKeep(); 

  StMuDstMaker* maker = new StMuDstMaker(1,1,dirName);

  //  To use a different PID file than the default, uncomment and
  // modify the above
  //  maker->setProbabilityPidFile("Path/PIDTable.root");
  maker->setProbabilityPidFile();
  
  maker->SetStatus("*",0);
  maker->SetStatus("EztAll",1);
  StMuDebug::setLevel(0);
  
  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "StEztMaker -- Working on eventNumber " << iev << endl;
    chain->Clear();
    int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers
  
  cout << " End of Analysis " << endl;
}



