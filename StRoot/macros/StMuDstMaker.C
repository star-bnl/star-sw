//
// NOTE - chain needs to be declared global so for StHbtEventReader
// Modified from StRoot/StMuDstmaker/COMMON/macros i.e. initially
// written by Frank Laue.
//
// Extend to account for production purposes.
// Arguments are
//      mode             bitmaks 
//                       bit 1 0x1   CMuDST always on regardless of 1st bit
//                       bit 2 0x2   EMC 'MuDST' ON/OFF (different from EMC branch in CMuDst)
//                       bit 3 0x4   RICH mode ON/OFF    
//                       bit 4 0x8   V0Finder processing during MuDst production
//
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
  cout << "Backward compatibility method. Please, use 6 arguments ... " << endl;
  StMuDstMaker(mode,0,nevents,path,file,outDir);
}

void StMuDstMaker(const Int_t   mode=0,
		  const Int_t   fsti=0,
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
  ProcessQQ(mode,fsti,nevents,fileListQQ,outDir);
}


//==========================================================================================
void ProcessQQ(const Int_t mode, const Int_t fsti, const Int_t nevents,
	       const Char_t **fileList, const Char_t* dirName)
{
  cout << "ProcessQQ::Load : Loading libraries ..." << endl;
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  //New V0 finder
  if (mode & 0x8){
    gSystem->Load("StSecondaryVertexMaker");
  }

  //if( mode & 0x2){
  // EMC specific
  cout << "ProcessQQ::Load : MuDst-EMC mode is ON " << endl;
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  //}

  if( mode & 0x4){
    // RICH
    cout << "ProcessQQ::Load : RICH mode enabled" << endl;
    gSystem->Load("StRrsMaker");
    gSystem->Load("StRchMaker");
    gSystem->Load("StRichPIDMaker");
    gSystem->Load("StRichSpectraMaker");
  }

  gSystem->Load("StMcAnalysisMaker");

  if( mode & 0x2 ){
    // EMC specific
    gSystem->Load("StEmcADCtoEMaker"); // analysis maker
    gSystem->Load("StPreEclMaker");    // analysis maker
    gSystem->Load("StEpcMaker");       // analysis maker
  }
  cout << "ProcessQQ::Load : done " << endl;


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
  //  ioMaker->SetBranch("emcBranch",0,"r");   //activate evt.root Branch
  ioMaker->SetBranch("runcoBranch",0,"r"); //activate runcoBranch

  St_db_Maker dbMaker("db","MySQL:StarDb","$STAR/StarDb","StarDb");

  if (mode & 0x8){
    cout << " V0Finder mode enabled" << endl;
    StV0FinderMaker* secondaryFinder = new StV0FinderMaker;
  }

  // ***********************************************
  // MuDstMaker(s) instantiation / chain activation
  // ***********************************************
  // Common MuDST part
  StStrangeMuDstMaker* v0dst = new StStrangeMuDstMaker("strangeMuDst");
  v0dst->DoV0();      //Set v0MiniDstMaker to find only v0s
  v0dst->DoXi();      //Set v0MiniDstMaker to find only v0s
  v0dst->DoKink();    //Set v0MiniDstMaker to find only v0s
  v0dst->SetNoKeep();

  // New V0 finder part - specifically for ITTF testing, we will allow
  // it to consider BOTH pairs of ITTF tracks AND pairs of old tracks
  // but not mixed pairs (experts can tell difference in MuDst).
  // Could use kTrackerUseBOTH and kTrackerUseTPT if would <hash>include
  // StSecondaryMaker/StV0FinderMaker.h but no includes in this macro so 
  // I followed that convention. (LSB)
  if (mode & 0x8){
    if ( fsti == 1){
      secondaryFinder->SetTrackerUsage(2);
    } else {
      secondaryFinder->SetTrackerUsage(0);
    }
  }

  // RICH part
  if( mode & 0x4 ){
    StRichSpectraMaker* spectraMaker = new StRichSpectraMaker("spectraMaker");
  }

  StMuDstMaker* maker = new StMuDstMaker(1,1,dirName);
  // You can change the PIDTable map using the above method.
  //maker->setProbabilityPidFile("Path/PIDTable.root");
  maker->setProbabilityPidFile();

  StMuL3Filter* l3Filter = new StMuL3Filter(); maker->setL3TrackFilter(l3Filter);
  StMuFilter* filter = new StMuFilter();       maker->setTrackFilter(filter);

  // EMC part
  if( mode & 0x2){
    //St_db_Maker *dbMk = new St_db_Maker("StarDb","MySQL:StarDb");
    StEmcADCtoEMaker *adc = new StEmcADCtoEMaker();
    StPreEclMaker *pre = new StPreEclMaker();
    StEpcMaker *epc = new StEpcMaker();
    StEmcMicroDstMaker *write = new StEmcMicroDstMaker();
    write->setOutputDir(dirName);
  }


  if ( fsti == 1){
    //select for Sti tracks
    filter->addEncodedMethod(263);
  }



  // This should call the Init() method in ALL makers
  chain->Init();
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

//==========================================================================================
// $Id: StMuDstMaker.C,v 1.16 2017/11/29 20:29:44 genevb Exp $
// $Log: StMuDstMaker.C,v $
// Revision 1.16  2017/11/29 20:29:44  genevb
// Keep CVS id and log information
//
//
