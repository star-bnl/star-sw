// examples :
// tracks =  ((ntupleTrack*) (((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->FirstParticleCut()))->GetNtupleTrack()
// tracks->Draw("dca")
// events = (TTree*) ((ntupleEvent*) ((StHbtAnalysis*)((StHbtMaker*)chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->EventCut())->getNtupleEvent()
// events->Draw("numOfTracks")

// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    

// keep pointers to Analysis global, so you can have access to themm ...
class StHbtAnalysis;
StHbtAnalysis* anal;

// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.
Int_t usePath = 0;
Int_t nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;


void StHbtExampledd(const Int_t nevents, const Char_t **fileList, const char*);

//==========================================================================================
//==========================================================================================
void ntuple(const Int_t nevents=9999, const Char_t *path = "-", 
	    const Char_t *file = "/star/rcf/pwg/hbt/July2000/HalfFieldData_new2.microDst")
{ 
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  StHbtExampledd(nevents,fileListQQ);
}
//==========================================================================================
//==========================================================================================
void StHbtExampledd(const Int_t nevents, const Char_t **fileList)
{

  // Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities"); 
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StHbtMaker");   

  cout << " loading done " << endl;

  chain = new StChain("StChain"); 
  chain->SetDebug();
 
  // file handling
  StFile *setFiles= new StFile();
  for (int ifil=0; fileList[ifil]; ifil++)
    {
      setFiles->AddFile(fileList[ifil]);
    }

  // Hbt Maker
  StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
  cout << "StHbtMaker instantiated."<<endl;


  // set up of hbt stuff 
  cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;
  StHbtManager* TheManager = hbtMaker->HbtManager();
  
  // set up mikro dst Reader
  StHbtBinaryReader* Reader = new StHbtBinaryReader((const char*)0,*fileList,(const char*)0);
  TheManager->SetEventReader(Reader);
  cout << "READER SET UP.... " << endl;

  
 
  // ********************************************* // 
  // * ntuple chain
  // ********************************************* // 
  // 0) now define an analysis...
  StHbtAnalysis* ntupleAnal = new StHbtAnalysis;
  // 1) set the Event cuts for the analysis 
  mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
  evcut->SetEventMult(00,100000);           
  evcut->SetVertZPos(-50.0,50.0);            
  ntupleEvent* nevent = new ntupleEvent;    // OR PRODUCE EVENT NTUPLE !!!
  ntupleAnal->SetEventCut(nevent);     
  // 2) set the Track cut 
  franksTrackCut* piPlusTrkcut = new franksTrackCut(); // use "frank's" track cut object  
  piPlusTrkcut->SetNSigmaPion(-100.0,100.0);    
  piPlusTrkcut->SetNSigmaKaon(-100.0,100.0);    
  piPlusTrkcut->SetNSigmaProton(-100.0,100.0);  
  piPlusTrkcut->SetNHits(0,0 );                
  piPlusTrkcut->SetP(0.1,3.0);                  
  piPlusTrkcut->SetPt(0.0,2.0);                 
  piPlusTrkcut->SetRapidity(-1.0,1.0);          
  piPlusTrkcut->SetDCA(-1.0,1.0);               
  piPlusTrkcut->SetCharge(+1);                  
  piPlusTrkcut->SetMass(0.139);                 
  ntupleTrack* ntrack = new ntupleTrack();          // OR PRODUCE TRACK NTUPLE !!!
  ntupleAnal->SetFirstParticleCut(ntrack);   // this is the track cut for the "first" particle
  ntupleAnal->SetSecondParticleCut(piPlusTrkcut);  // this is the track cut for the "second" particle
  // 3) set the Pair cuts for the analysis, must be provided !
  qpc = new qualityPairCut();
  ntupleAnal->SetPairCut(qpc);         // this is the pair cut for this analysis
  // 4) set the number of events to mix (per event)
  ntupleAnal->SetNumEventsToMix(1);  
  // 5) now set up the correlation functions that this analysis will make
  QinvEbyECorrFctn* QinvEbyECF = new QinvEbyECorrFctn("QinvEbyE",20,0.,.2); 
  ntupleAnal->AddCorrFctn(QinvEbyECF);   
  // 6) add the Analysis to the AnalysisCollection
  TheManager->AddAnalysis(ntupleAnal);

 
  // now run the chain
  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();
 
  for (Int_t iev=0;iev<nevents; iev++) {
    cout << "StHbtExample -- Working on eventNumber " << iev << endl;
    chain->Clear();
    int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
    if (iret) {
      cout << "Bad return code!" << endl;
      break;
    }
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers

}



