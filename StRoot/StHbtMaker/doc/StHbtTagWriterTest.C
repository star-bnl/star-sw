// example how to access memberfunctions from a correlation function 
// ((QinvEbyECorrFctn*)((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->CorrFctn(0))->Ratio()->Draw()

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
//class StChain;
//StChain *chain=0;

void wait(int n=1) {
  for ( int i=0; i<n*1e6; i++) { /*no-op*/ }
}
void mess(const char* c="alive") {
  for ( int i=0; i<10; i++) { cout << c << endl; }
}


void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList, const char*);


//==========================================================================================
//==========================================================================================
void StHbtTagWriterTest(const Int_t nevents=9999, const Char_t *path, const Char_t *file)
{ 
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  StHbtExampleQQ(nevents,fileListQQ);
}
//==========================================================================================
//==========================================================================================
void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList)
{

// Dynamically link needed shared libs
gSystem->Load("St_base");
gSystem->Load("StChain");
gSystem->Load("St_Tables");
gSystem->Load("StMagF");
gSystem->Load("StUtilities");  // new addition 22jul99
gSystem->Load("StTreeMaker");
gSystem->Load("StIOMaker");
gSystem->Load("StarClassLibrary");
gSystem->Load("StEvent");
gSystem->Load("StEventMaker");

gSystem->Load("StHbtMaker");   
//gSystem->Load("StHbtTagMaker");   

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
StIOMaker* ioMaker = new StIOMaker("IO","r",setFiles,"bfcTree");
ioMaker->SetDebug();
ioMaker->SetIOMode("r");
ioMaker->SetDebug();
ioMaker->SetBranch("*",0,"r");                 //deactivate all branches
// ***********
// Event Maker 
// ***********
StEventMaker* eventMaker = new StEventMaker("events","title");
cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << endl;
// *********
// Hbt Maker
// *********
StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
 cout << "StHbtMaker instantiated"<<endl;
 StHbtTagMaker* hbtTagMaker = new StHbtTagMaker("HBTTAG");
 hbtTagMaker->SetShowTags(1);
 cout << "StHbtTagMaker instantiated"<<endl;

// -------------- set up of hbt stuff ----- //
cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;
StHbtManager* TheManager = hbtMaker->HbtManager();
// ***********************
// setup HBT event readers   
// ***********************
// *****************************************
// set up StHbtMcEventReader as Event Reader
// *****************************************
StStandardHbtEventReader* Reader = new StStandardHbtEventReader;
Reader->SetTheEventMaker(eventMaker);     // gotta tell the reader where it should read from
TheManager->SetEventReader(Reader);
cout << "READER SET UP.... " << endl;

// define example particle cut and cut monitors to use in the analyses
// example particle cut
 franksTrackCut* aTrackCut = new franksTrackCut;  // use "frank's" particle cut object
 aTrackCut->SetNSigmaPion(-3.0,3.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 aTrackCut->SetNSigmaKaon(-1000.,1000.);       // number of Sigma in TPC dEdx away from nominal kaon dEdx
 aTrackCut->SetNSigmaProton(-1000.,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 aTrackCut->SetNHits(10,50);            // range on number of TPC hits on the track
 aTrackCut->SetP(0.,0.8);               // range in P
 aTrackCut->SetPt(0.,2.);              // range in Pt
 aTrackCut->SetRapidity(-1.5,1.5);      // range in rapidity
 aTrackCut->SetDCA(0.0,2.);             // range in Distance of Closest Approach to primary vertex
 aTrackCut->SetCharge(+1);              // want positive kaons
 aTrackCut->SetMass(0.139);             // kaon mass
// now, we define the analysis
 
 // ********************************************* // 
 // * franks piPlus analysis - by Frank Laue, OSU //
 // ********************************************* // 
 // 0) now define an analysis...
 StHbtAnalysis* piPlusAnal = new StHbtAnalysis;
 // 1) set the Event cuts for the analysis
 mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
 evcut->SetEventMult(0,100000);      // selected multiplicity range
 evcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
 piPlusAnal->SetEventCut(evcut);          // this is the event cut object for this analsys
 // 2) set the Track (particle) cuts for the analysis
 franksTrackCut* piPlusTrkcut = new franksTrackCut( *aTrackCut );  // copy from example
 piPlusAnal->SetFirstParticleCut(piPlusTrkcut);  // this is the track cut for the "first" particle
 piPlusAnal->SetSecondParticleCut(piPlusTrkcut);  // this is the track cut for the "second" particle
 // 3) set the Pair cuts for the analysis
 mikesPairCut* pairCut = new mikesPairCut;  // use "frank's" pair cut object
 piPlusAnal->SetPairCut(pairCut);         // this is the pair cut for this analysis
 // 4) set the number of events to mix (per event)
 piPlusAnal->SetNumEventsToMix(5); 
 // ********************************************************************
 // 5) now set up the correlation functions that this analysis will make
 // ********************************************************************
 // define example Minv correlation function
 QinvEbyECorrFctn* QinvEbyECF = new QinvEbyECorrFctn("QinvEbyE",20,0.,.2); 
 QinvEbyECF->SetTagMeans("positivePionsMeans");
 QinvEbyECF->SetTagSigmas("positivePionsSigmas");
 piPlusAnal->AddCorrFctn(QinvEbyECF);   // adds the just-defined correlation function to the analysis
 // now add as many more correlation functions to the Analysis as you like..
 // 6) add the Analysis to the AnalysisCollection
 TheManager->AddAnalysis(piPlusAnal);

 // ********************************************* // 
 // * franks piPlus analysis - by Frank Laue, OSU //
 // ********************************************* // 
 // // 0) now define an analysis...
 StHbtAnalysis* kaonPlusAnal = new StHbtAnalysis;
 // 1) set the Event cuts for the analysis
 mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
 evcut->SetEventMult(0,100000);      // selected multiplicity range
 evcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
 kaonPlusAnal->SetEventCut(evcut);          // this is the event cut object for this analsys
 // 2) set the Track (particle) cuts for the analysis
 franksTrackCut* kaonPlusTrkcut = new franksTrackCut( *aTrackCut );  // copy from example
 kaonPlusTrkcut->SetNSigmaPion(3.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 kaonPlusTrkcut->SetNSigmaKaon(-3.,3.);       // number of Sigma in TPC dEdx away from nominal kaon dEdx
 kaonPlusTrkcut->SetNSigmaProton(-1000.0,-3.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 kaonPlusTrkcut->SetMass(0.494);
 kaonPlusAnal->SetFirstParticleCut(kaonPlusTrkcut);  // this is the track cut for the "first" particle
 kaonPlusAnal->SetSecondParticleCut(kaonPlusTrkcut);  // this is the track cut for the "second" particle
 // 3) set the Pair cuts for the analysis
 mikesPairCut* pairCut = new mikesPairCut;  // use "frank's" pair cut object
 kaonPlusAnal->SetPairCut(pairCut);         // this is the pair cut for this analysis
 // 4) set the number of events to mix (per event)
 kaonPlusAnal->SetNumEventsToMix(5); 
 // ********************************************************************
 // 5) now set up the correlation functions that this analysis will make
 // ********************************************************************
 // define example Minv correlation function
 QinvEbyECorrFctn* QinvEbyECFKaons = new QinvEbyECorrFctn("QinvEbyEKaons",20,0.,.2); 
 QinvEbyECFKaons->SetTagMeans("positiveKaonsMeans");
 QinvEbyECFKaons->SetTagSigmas("positiveKaonsSigmas");
 kaonPlusAnal->AddCorrFctn(QinvEbyECFKaons);   // adds the just-defined correlation function to the analysis
 // now add as many more correlation functions to the Analysis as you like..
 // 6) add the Analysis to the AnalysisCollection
 TheManager->AddAnalysis(kaonPlusAnal);



 // ------------------ end of setting up hbt stuff ------------------ //

 chain->Init(); // This should call the Init() method in ALL makers
 chain->PrintInfo();
 
 // exit(); 
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



