// example how to access memberfunctions from a correlation function 
//  ((MinvCorrFctn*) ((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->CorrFctn(0))->Difference()->Draw()

// ((MinvCorrFctnY_vs_Pt*) ((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->CorrFctn(2))->Difference()->Draw("colz")

// ((MinvCorrFctnArmenteros*) ((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->CorrFctn(1))->Difference()->Draw("colz")

// examples to access member function from cutMonitors (here the member functions return a pointer to a histogram)  
// ((trackCutMonitor_P_vs_Dedx*)phiAnal->FirstParticleCut()->PassMonitor(0))->Histo()->Draw()
// ((trackCutMonitor_P_vs_Dedx*)phiAnal->FirstParticleCut()->FailMonitor(0))->Histo()->Draw()
// ((eventCutMonitor_Mult*)phiAnal->EventCut()->PassMonitor(0))->Histo()->Draw()

// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    

// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.
Int_t usePath = 0;
Int_t nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
//class StChain;
//StChain *chain=0;

TBrowser *b=0;
const char *venusFile ="set*geant.root";
const char *venusPath ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfs_4/";
const char *dstFile ="/disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/ric0022_01_14552evts.dst.root";
const char *xdfFile ="/afs/rhic/star/data/samples/psc0054_07_40evts_dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *geantFile ="/disk00000/star/auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on/tfsr/set0041_01_53evts.geant.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void wait(int n=1) {
  for ( int i=0; i<n*1e6; i++) { /*no-op*/ }
}
void mess(const char* c="alive") {
  for ( int i=0; i<10; i++) { cout << c << endl; }
}


void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList, const Char_t*, const Char_t* );


//==========================================================================================
//==========================================================================================
void StHbtMicroDstMaker(const Int_t nevents=9999,
			const Char_t *path=venusPath,
			const Char_t *file=venusFile,
			const Char_t* outDir="/star/rcf/pwg/hbt/Laue/Test/",		
			const Char_t* outFile="")		
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

// Dynamically link needed shared libs
gSystem->Load("St_base");
gSystem->Load("StChain");
gSystem->Load("St_Tables");
gSystem->Load("StMagF");
gSystem->Load("StUtilities");  // new addition 22jul99
gSystem->Load("StTreeMaker");
gSystem->Load("StIOMaker");
gSystem->Load("StarClassLibrary");
gSystem->Load("xdf2root");
gSystem->Load("St_xdfin_Maker");
gSystem->Load("StEvent");
gSystem->Load("StEventMaker");
gSystem->Load("StMcEvent"); 
gSystem->Load("StMcEventMaker"); 
gSystem->Load("StAssociationMaker");
gSystem->Load("StMcAnalysisMaker");
gSystem->Load("StHbtMaker");   
gSystem->Load("global_Tables");   

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
ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch

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
//StHbtTagMaker* hbtTagMaker = new StHbtTagMaker("HBTTAG");
//cout << "StHbtMaker instantiated"<<endl;
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

  //   set up a microDstWriter 
 StHbtBinaryReader* pionWriter = new StHbtBinaryReader(ioMaker);  // retrieve filename from ioMaker
 pionWriter->SetDirName( dirName );
 pionWriter->SetFileName("test");
 pionWriter->SetAppendix(".pion.microDst");
 //StHbtBinaryReader* pionWriter = new StHbtBinaryReader();           // specify filename
 //pionWriter->SetDirName( dirName );
 //pionWriter->SetFileNameName( fileName );
 //pionWriter->SetAppendix( appendix );
 TheManager->AddEventWriter(pionWriter);
 // set up the front loaded cuts
 mikesEventCut* pionEventCut = new mikesEventCut;
 pionEventCut->SetEventMult(0,100000);      // selected multiplicity range
 pionEventCut->SetVertZPos(-35.0,35.0);     // selected range of vertex z-position
 pionWriter->SetEventCut(pionEventCut);
 franksTrackCut* pionTrackCut = new franksTrackCut;
 pionTrackCut->SetNSigmaPion(-3.0,+3.);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 pionTrackCut->SetNSigmaKaon(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 pionTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 pionTrackCut->SetNHits(5,1000);           // range on number of TPC hits on the track
 pionTrackCut->SetP(0.0,5.0);               // range in P
 pionTrackCut->SetPt(0.0,5.0);              // range in Pt
 pionTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 pionTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 pionTrackCut->SetCharge(0);                // no cut on charge
 pionTrackCut->SetMass(0.139);              // pion mass
 pionWriter->SetTrackCut(pionTrackCut);
 cout << "WRITER SET UP.... " << endl;
    
  //   set up a microDstWriter 
 StHbtBinaryReader* kaonWriter = new StHbtBinaryReader(ioMaker);  // retrieve filename from ioMaker
 kaonWriter->SetDirName( dirName );
 kaonWriter->SetFileName("test");
 kaonWriter->SetAppendix(".kaon.microDst");
 TheManager->AddEventWriter(kaonWriter);
 // set up the front loaded cuts
 mikesEventCut* kaonEventCut = new mikesEventCut;
 kaonEventCut->SetEventMult(0,100000);      // selected multiplicity range
 kaonEventCut->SetVertZPos(-35.0,35.0);     // selected range of vertex z-position
 kaonWriter->SetEventCut(kaonEventCut);
 franksTrackCut* kaonTrackCut = new franksTrackCut;
 kaonTrackCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 kaonTrackCut->SetNSigmaKaon(-3.,3.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 kaonTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 kaonTrackCut->SetNHits(5,1000);           // range on number of TPC hits on the track
 kaonTrackCut->SetP(0.0,5.0);               // range in P
 kaonTrackCut->SetPt(0.0,5.0);              // range in Pt
 kaonTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 kaonTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 kaonTrackCut->SetCharge(0);                // no cut on charge
 kaonTrackCut->SetMass(0.494);              // kaon mass
 kaonWriter->SetTrackCut(kaonTrackCut);
 cout << "WRITER SET UP.... " << endl;
    
  //   set up a microDstWriter 
 StHbtBinaryReader* protonWriter = new StHbtBinaryReader(ioMaker);  // retrieve filename from ioMaker
 protonWriter->SetDirName( dirName );
 protonWriter->SetFileName("test");
 protonWriter->SetAppendix(".proton.microDst");
 TheManager->AddEventWriter(protonWriter);
 // set up the front loaded cuts
 mikesEventCut* protonEventCut = new mikesEventCut;
 protonEventCut->SetEventMult(0,100000);      // selected multiplicity range
 protonEventCut->SetVertZPos(-35.0,35.0);     // selected range of vertex z-position
 protonWriter->SetEventCut(protonEventCut);
 franksTrackCut* protonTrackCut = new franksTrackCut;
 protonTrackCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 protonTrackCut->SetNSigmaKaon(-1000.,1000.);   // number of Sigma in TPC dEdx away from nominal proton dEdx
 protonTrackCut->SetNSigmaProton(-3.0,3.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 protonTrackCut->SetNHits(5,1000);           // range on number of TPC hits on the track
 protonTrackCut->SetP(0.0,5.0);               // range in P
 protonTrackCut->SetPt(0.0,5.0);              // range in Pt
 protonTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 protonTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 protonTrackCut->SetCharge(0);                // no cut on charge
 protonTrackCut->SetMass(0.938);              // proton mass
 protonWriter->SetTrackCut(protonTrackCut);
 cout << "WRITER SET UP.... " << endl;
    

  //   se up a microDstWriter  with multiple track cuts
 StHbtBinaryReader* pionProtonWriter = new StHbtBinaryReader(ioMaker);  // retrieve filename from ioMaker
 //StHbtBinaryReader* pionProtonWriter = new StHbtBinaryReader();
 pionProtonWriter->SetDirName( dirName );
 pionProtonWriter->SetFileName("test");
 pionProtonWriter->SetAppendix(".pion.proton.microDst");
 TheManager->AddEventWriter(pionProtonWriter);
 // set up front loaded event cuts
 mikesEventCut* kaonEventCut = new mikesEventCut;
 kaonEventCut->SetEventMult(0,100000);      // selected multiplicity range
 kaonEventCut->SetVertZPos(-35.0,35.0);     // selected range of vertex z-position
 pionProtonWriter->SetEventCut(kaonEventCut);
 // set up first front loaded track cuts
 franksTrackCut* piTrackCut = new franksTrackCut;
 piTrackCut->SetNSigmaPion(-3.0,+3.0);       // number of Sigma in TPC dEdx away from nominal pion dEdx
 piTrackCut->SetNSigmaKaon(-1000.,+1000.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 piTrackCut->SetNSigmaProton(-1000.,+1000.); // number of Sigma in TPC dEdx away from nominal proton dEdx
 piTrackCut->SetNHits(5,1000);            // range on number of TPC hits on the track
 piTrackCut->SetP(0.0,5.0);               // range in P
 piTrackCut->SetPt(0.0,5.0);              // range in Pt
 piTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 piTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 piTrackCut->SetCharge(0);                // no cut on charge
 piTrackCut->SetMass(0.139);              // pion mass
 // set up second front loaded track cuts
 franksTrackCut* prTrackCut = piTrackCut->Clone(); // copy above cut
 prTrackCut->SetNSigmaPion(-1000.,+1000.);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 prTrackCut->SetNSigmaKaon(-1000.,+1000.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 prTrackCut->SetNSigmaProton(-3.,+3.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 prTrackCut->SetMass(0.938);            // proton mass
 // set up multi track cut
 StHbtMultiTrackCut* multiTrackCut = new StHbtMultiTrackCut();
 multiTrackCut->AddTrackCut(piTrackCut);
 multiTrackCut->AddTrackCut(prTrackCut);
 pionProtonWriter->SetTrackCut(multiTrackCut);
 cout << "WRITER SET UP.... " << endl;
    

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

 cout << " End of Analysis " << endl;
}



