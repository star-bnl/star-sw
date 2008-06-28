// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    

void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList, const Char_t*, const Char_t* );


//==========================================================================================
//==========================================================================================
void StMuDstMaker(const Int_t nevents=1000,
		    const Char_t *path="-",
		    //		    const Char_t *file="/star/data20/reco/productionCentral/ReversedFullField/P01gl/2001/303/st_physics_2303036_raw_0535.event.root",
		    const Char_t *file="/star/data48/reco/ppProduction2008/ReversedFullField/P08ic/2008/054/9054010/st_toftpx_9054010_raw_2400010.event.root",
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
  StHbtExampleQQ(nevents,fileListQQ,outDir,outFile);
}

//==========================================================================================
//==========================================================================================
void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList, const Char_t* dirName, const Char_t* fileName)
{
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");

  //  gSystem->Setenv("JPROF_FLAGS", "JP_START JP_PERIOD=0.001"); 
  //  gSystem->Load("/afs/rhic.bnl.gov/star/packages/DEV/.i386_redhat61/lib/libJprof"); 
 
 cout << " loading done " << endl;
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
  
  // ***********************
  // the StStrangeMuDstMaker
  // ***********************
  
  StStrangeMuDstMaker* v0dst = new StStrangeMuDstMaker("strangeMuDst");
  v0dst->DoV0(); //Set v0MiniDstMaker to find only v0s
  v0dst->DoXi(); //Set v0MiniDstMaker to find only v0s
  v0dst->DoKink(); //Set v0MiniDstMaker to find only v0s
  v0dst->SetNoKeep(); 
  //v0dst->SetWrite("depp.root"); // Set V0muDStMaker output file and Event output file
  // v0dst->SetWrite("StrangemuEventHBTPeriphdst.root","Strangemuv0HBTPeriphdst.root"); // Set V0muDStMaker output file and Event output file
  
  
  // *********
  // Hbt Maker
  // *********
  
  StMuDstMaker* maker = new StMuDstMaker(1,1,dirName);
  //  maker->setSplit(500);
  //  maker->setBufferSize(4000);
  //  To use a different PID file than the default, uncomment and
  // modify the above
  //  maker->setProbabilityPidFile("Path/PIDTable.root");
  maker->setProbabilityPidFile();
  
  StMuL3Filter* l3Filter = new StMuL3Filter(); maker->setL3TrackFilter(l3Filter);
  StMuFilter* filter = new StMuFilter();       maker->setTrackFilter(filter);
  //  filter->addEncodedMethod(32770);
  //  filter->addEncodedMethod(8,7);
  StMuDebug::setLevel(0);
  
  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "StExample -- Working on eventNumber " << iev << endl;
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



