//
// NOTE - chain needs to be declared global so for StHbtEventReader
// Modified from StRoot/StMuDstmaker/COMMON/macros i.e. initially
// written by Frank Laue.
//
// Extend to account for production purposes.
// Arguments are
//      mode             bitmaks CMuDST always on regardless of bit 1
//                       bit 2    EMC MuDST ON/OFF
//      nevents           
//      path             Path where the InputFile resides ; "-" for local
//      InputFile        Should be an event.root file
//      OutputDirectory  Optional output directory (does not fully work)
//      
//
class   StChain;
StChain *chain=0;    
void    ProcessQQ(const Int_t, const Int_t, const Char_t **, const Char_t*, const Char_t* );



//==========================================================================================
void StMuDstMaker(const Int_t   mode=0,
		  const Int_t   nevents=10,
		  const Char_t  *path="/star/data13/reco/dev/2001/10/",
		  const Char_t  *file="st_physics_2304060_raw_0303.event.root",
		  const Char_t* outDir="./")		
{ 

  const char *fileListQQ[]={0,0};

  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  ProcessQQ(mode,nevents,fileListQQ,outDir);
}


//==========================================================================================
void ProcessQQ(const Int_t mode, const Int_t nevents, 
	       const Char_t **fileList, const Char_t* dirName)
{
  cout << "Loading libraries ..." << endl;
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StDaqLib");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");

  if( mode & 0x2){
    // EMC specific
    gSystem->Load("StEmcUtil"); 
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
  }

  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");

  if( mode & 0x2 ){
    // EMC specific
    gSystem->Load("StEmcADCtoEMaker"); // analysis maker
    gSystem->Load("StPreEclMaker");    // analysis maker
    gSystem->Load("StEpcMaker");       // analysis maker
  }
  cout << "Loading done " << endl;


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
  ioMaker->SetBranch("*",0,"0");           //deactivate all branches
  ioMaker->SetBranch("eventBranch",0,"r"); //activate evt.root Branch


  // ***********************************************
  // MuDstMaker(s) instantiation / chain activation
  // ***********************************************
  // Common MuDST part
  StStrangeMuDstMaker* v0dst = new StStrangeMuDstMaker("StrangeMaker");
  v0dst->DoV0();      //Set v0MiniDstMaker to find only v0s
  v0dst->DoXi();      //Set v0MiniDstMaker to find only v0s
  v0dst->DoKink();    //Set v0MiniDstMaker to find only v0s
  v0dst->SetNoKeep(); 

  StMuDstMaker* maker = new StMuDstMaker(1,1,dirName);
  maker->setProbabilityPidFile("/afs/rhic/star/users/aihong/www/PIDTableP01gl.root");

  StMuL3Filter* l3Filter = new StMuL3Filter(); maker->setL3TrackFilter(l3Filter);
  StMuFilter* filter = new StMuFilter();       maker->setTrackFilter(filter);

  // EMC part
  if( mode & 0x2){
    St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");       	
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
    StPreEclMaker *pre = new StPreEclMaker();
    StEpcMaker *epc = new StEpcMaker();
    StEmcMicroDstMaker *write = new StEmcMicroDstMaker();
    write->setOutputDir(dirName);
  }

  chain->Init();      // This should call the Init() method in ALL makers
  chain->PrintInfo();

  // Event Loop
  Int_t iev=0;
  for ( ; iev < nevents ; iev++) {
    cout << "-----> Working on eventNumber " << iev << endl;
    chain->Clear();
    int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }
  } 
  chain->Finish();    // This should call the Finish() method in ALL makers

  iev--;
  cout << endl << "******************* Last event processed = " << iev << endl; 
}

