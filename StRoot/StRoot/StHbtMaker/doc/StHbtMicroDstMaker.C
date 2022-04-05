enum dataFormat {dst, evt};

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
const char *xdfFile ="/afs/rhic.bnl.gov/star/data/samples/psc0054_07_40evts_dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *geantFile ="/disk00000/star/auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on/tfsr/set0041_01_53evts.geant.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void wait(int n=1) {
  for ( int i=0; i<n*1e6; i++) { /*no-op*/ }
}
void mess(const char* c="alive") {
  for ( int i=0; i<10; i++) { cout << c << endl; }
}



void StHbtExampleQQ(const dataFormat format, const Int_t nevents, const Char_t **fileList, const Char_t*, const Char_t* );


//==========================================================================================
//==========================================================================================
void StHbtMicroDstMaker(const dataFormat format,
			const Int_t nevents=9999,
			const Char_t *path=venusPath,
			const Char_t *file=venusFile,
			const Char_t* outDir="/star/rcf/pwg/hbt/Laue/Test/SL00hg/",		
			const Char_t* outFile="dummy",
			const Char_t* appendix="test.microDst")		
{ 
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  StHbtExampleQQ(format,nevents,fileListQQ,outDir,outFile);
}
//==========================================================================================
//==========================================================================================
void StHbtExampleQQ(const dataFormat format, const Int_t nevents, const Char_t **fileList, const Char_t* dirName, const Char_t* fileName)
{

// Dynamically link needed shared libs
gSystem->Load("StarRoot");
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
gSystem->Load("StEventUtilities"); 
gSystem->Load("StMcEvent"); 
gSystem->Load("StMcEventMaker"); 
gSystem->Load("StAssociationMaker");
gSystem->Load("StMcAnalysisMaker");
gSystem->Load("StStrangeMuDstMaker");
gSystem->Load("StHbtMaker");   


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
ioMaker->SetBranch("*",0,"0");         //deactivate all branches
// **************
// StHbtTagReader
// **************
//StHbtTagReader* tagReader = new StHbtTagReader(ioMaker);
cout << "Just instantiated StHbtTagReader... lets go StHbtMaker!" << endl;

// ***********
// Event Maker 
// ***********
StEventMaker* eventMaker =0;

if (format==dst) {
  eventMaker = new StEventMaker("events","title");
  ioMaker->SetBranch("dstBranch",0,"r"); //activate dst.root Branch
  cout << " dst" << endl;
}
else if(format==evt) {  
  eventMaker =0;
  ioMaker->SetBranch("eventBranch",0,"r"); //activate evt.root Branch
  cout << " evt" << endl;
}
else return;
cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << eventMaker << endl;

// Set up the v0muDstMaker
    StStrangeMuDstMaker* v0dst = new StStrangeMuDstMaker("v0dst");
    v0dst->DoV0(); //Set v0MiniDstMaker to find only v0s
    v0dst->DoKink(); //Set v0MiniDstMaker to find only v0s
    v0dst->SetNoKeep(); 
    v0dst->SetWrite(); // Set V0muDStMaker output file and Event output file
// v0dst->SetWrite("StrangemuEventHBTPeriphdst.root","Strangemuv0HBTPeriphdst.root"); // Set V0muDStMaker output file and Event output file
 cout << "Just instantiated StStrangeMuDstMaker... lets go StHbt!" << endl;


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
Reader->SetTheV0Maker(v0dst); //Gotta tell the reader where to read the v0 stuff from
Reader->SetTheTagReader(0); //Gotta tell the reader where to read the tag stuff from
TheManager->SetEventReader(Reader);

cout << "READER SET UP.... " << endl;

// 

// ***** set up event cut *****
 mikesEventCut* EventCut = new mikesEventCut;
 EventCut->SetEventMult(0,100000);      // selected multiplicity range
 EventCut->SetVertZPos(-75.0,75.0);     // selected range of vertex z-position

// ***** set up anti track cut *****
 franksTrackCut* AntiTrackCut = new franksTrackCut;
 AntiTrackCut->SetNSigmaPion(+1,-1.);
// ***** set up all track cut *****
 franksTrackCut* AllTrackCut = new franksTrackCut;
 AllTrackCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 AllTrackCut->SetNSigmaKaon(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 AllTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 AllTrackCut->SetNHits(10,1000);           // range on number of TPC hits on the track
 AllTrackCut->SetP(0.0,50.0);               // range in P
 AllTrackCut->SetPt(0.0,50.0);              // range in Pt
 AllTrackCut->SetRapidity(-15,15);       // range in rapidity
 AllTrackCut->SetDCA(0.0,200.);              // range in Distance of Closest Approach to primary vertex
 AllTrackCut->SetCharge(0);                // no cut on charge
 AllTrackCut->SetMass(0.139);              // pion mass
// ***** set up pion cut *****
 franksTrackCut* PionTrackCut = new franksTrackCut;
 PionTrackCut->SetNSigmaPion(-3.0,+3.);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 PionTrackCut->SetNSigmaKaon(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 PionTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 PionTrackCut->SetNHits(10,1000);           // range on number of TPC hits on the track
 PionTrackCut->SetP(0.0,5.0);               // range in P
 PionTrackCut->SetPt(0.0,5.0);              // range in Pt
 PionTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 PionTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 PionTrackCut->SetCharge(0);                // no cut on charge
 PionTrackCut->SetMass(0.139);              // pion mass
// ***** set up pion+ cut *****
 franksTrackCut* PionPlusTrackCut = new franksTrackCut;
 PionPlusTrackCut->SetNSigmaPion(-3.0,+3.);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 PionPlusTrackCut->SetNSigmaKaon(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 PionPlusTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 PionPlusTrackCut->SetNHits(10,1000);           // range on number of TPC hits on the track
 PionPlusTrackCut->SetP(0.0,5.0);               // range in P
 PionPlusTrackCut->SetPt(0.0,5.0);              // range in Pt
 PionPlusTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 PionPlusTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 PionPlusTrackCut->SetCharge(+1);                // no cut on charge
 PionPlusTrackCut->SetMass(0.139);              // pion mass
// ***** set up kaon cut *****
 franksTrackCut* KaonTrackCut = new franksTrackCut;
 // KaonTrackCut->SetNSigmaPion(+1.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 KaonTrackCut->SetNSigmaKaon(-3.,3.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 // KaonTrackCut->SetNSigmaProton(-1000.0,-1.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 KaonTrackCut->SetNHits(10,1000);           // range on number of TPC hits on the track
 KaonTrackCut->SetP(0.0,2.0);               // range in P
 KaonTrackCut->SetPt(0.0,2.0);              // range in Pt
 KaonTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 KaonTrackCut->SetDCA(0.0,3.);              // range in Distance of Closest Approach to primary vertex
 KaonTrackCut->SetCharge(0);                // no cut on charge
 KaonTrackCut->SetMass(0.494);              // kaon mass
// ***** set up kaon cut *****
 franksTrackCut* KaonMinusTrackCut = new franksTrackCut;
 KaonMinusTrackCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 KaonMinusTrackCut->SetNSigmaKaon(-3.,3.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 KaonMinusTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 KaonMinusTrackCut->SetNHits(10,1000);           // range on number of TPC hits on the track
 KaonMinusTrackCut->SetP(0.0,5.0);               // range in P
 KaonMinusTrackCut->SetPt(0.0,5.0);              // range in Pt
 KaonMinusTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 KaonMinusTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 KaonMinusTrackCut->SetCharge(-1);                // no cut on charge
 KaonMinusTrackCut->SetMass(0.494);              // kaon mass
// ***** set up proton cut ******
 franksTrackCut* ProtonTrackCut = new franksTrackCut;
 ProtonTrackCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 ProtonTrackCut->SetNSigmaKaon(-1000.,1000.);   // number of Sigma in TPC dEdx away from nominal proton dEdx
 ProtonTrackCut->SetNSigmaProton(-3.0,3.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 ProtonTrackCut->SetNHits(10,1000);           // range on number of TPC hits on the track
 ProtonTrackCut->SetP(0.0,5.0);               // range in P
 ProtonTrackCut->SetPt(0.0,5.0);              // range in Pt
 ProtonTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 ProtonTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 ProtonTrackCut->SetCharge(0);                // no cut on charge
 ProtonTrackCut->SetMass(0.938);              // proton mass

// ***** set up anti v0 cut *****
 helensV0Cut* AntiV0Cut = new helensV0Cut;
 AntiV0Cut->SetV0Type("blabla");           // Which particle do I cut on
 AntiV0Cut->SetPt(+1,-1);
// ***** set up lambda cut *****
 helensV0Cut* LambdaCut = new helensV0Cut;  // use V0 particle cut object
 LambdaCut->SetV0Type("lambda");           // Which particle do I cut on
 LambdaCut->SetV0MassRange(1.105,1.125);     // Width cut around /\ mass keep it loose as I want k0s too
 LambdaCut->SetdcaV0daughters(0.,1.0);   // DCA between 2 tracks
 LambdaCut->SetdcaV0ToPrimVertex(0.,1.); // DCA between V0 and event vertex
 LambdaCut->SetdecayLengthV0(3.0,5000.);  // Decay length from prim. vertex
 LambdaCut->SettpcHitsPos(10,50);       // Number of TPC hits on + track
 LambdaCut->SettpcHitsNeg(10,50);       // Number of TPC hits on - track
 LambdaCut->SetdcaPosToPrimVertex(1.8,50.); // Min. value + track at intersect
 LambdaCut->SetdcaNegToPrimVertex(3.5,50.); // Min. value - track at intersect
 LambdaCut->SetptArmV0(0.,0.25);        //K0 (0.1,0.25), Lambda (0.,.25)
 LambdaCut->SetalphaV0(0.0,1.);         //K0 (-1,1.), Lambda (0.5,1.0)
 LambdaCut->SetPt(0.,10.0);            // range in Pt
 LambdaCut->SetRapidity(-50.0,50.0);     // range in rapidity
 LambdaCut->SetMass(1.11567);            // /\ mass
// ***** set up a K0 cut *****
 helensV0Cut* K0Cut = new helensV0Cut;  // use V0 particle cut object
 K0Cut->SetV0Type("K0");           // Which particle do I cut on
 K0Cut->SetV0MassRange(.05,.05);     // Width cut around /\ mass keep it loose as I want k0s too
 K0Cut->SetdcaV0daughters(0.,0.5);   // DCA between 2 tracks
 K0Cut->SetdcaV0ToPrimVertex(0.,.5); // DCA between V0 and event vertex
 K0Cut->SetdecayLengthV0(4.0,2000.);  // Decay length from prim. vertex
 K0Cut->SetdecayLengthV0(2.676*1.,2.676*5.);  // Decay length from prim. vertex   | gammaTau K0 = 2.674cm
 K0Cut->SettpcHitsPos(0,1000);       // Number of TPC hits on + track
 K0Cut->SettpcHitsNeg(0,1000);       // Number of TPC hits on - track
 K0Cut->SetdcaPosToPrimVertex(0.2,1000.); // Min. value + track at intersect
 K0Cut->SetdcaNegToPrimVertex(0.2,1000.); // Min. value - track at intersect
 K0Cut->SetptArmV0(0.1,0.25);        //K0 (0.1,0.25), Lambda (0.,.25)
 K0Cut->SetalphaV0(-1.,1.);         //K0 (-1,1.), Lambda (0.5,1.0)
 K0Cut->SetptArmV0(-100.,+100.);        //K0 (0.1,0.25), Lambda (0.,.25)
 K0Cut->SetalphaV0(-100.,+100.);         //K0 (-1,1.), Lambda (0.5,1.0)
 K0Cut->SetPt(0.1,2.0);            // range in Pt
 K0Cut->SetRapidity(-50.0,50.0);     // range in rapidity
 K0Cut->SetMass(0.49762);            // /\ mass




  //   set up a microDstWriter 
 StHbtTTreeReader* allWriter = new StHbtTTreeReader(1,ioMaker,"./","",".hbtTTreeMuDst","");
 // StHbtBinaryReader* allWriter = new StHbtBinaryReader(ioMaker,dirName,"dummy",".microDst");
 TheManager->AddEventWriter(allWriter);
 allWriter->SetEventCut(EventCut);
 allWriter->SetTrackCut(AllTrackCut);
 allWriter->SetV0Cut(AntiV0Cut);
 cout << "WRITER SET UP.... " << endl;

  //   set up a microDstWriter 
//     StHbtBinaryReader* sigma1385Writer = new StHbtBinaryReader(ioMaker,dirName,"dummy",".sigma1385.microDst");
//     TheManager->AddEventWriter(sigma1385Writer);
//     sigma1385Writer->SetEventCut(EventCut);
//     sigma1385Writer->SetTrackCut(PionPlusTrackCut);
//     sigma1385Writer->SetV0Cut(LambdaCut);
//    cout << "WRITER SET UP.... " << endl;

  //   set up a microDstWriter 
//     StHbtBinaryReader* pionWriter = new StHbtBinaryReader(ioMaker,dirName,"dummy",".pion.microDst");
//     TheManager->AddEventWriter(pionWriter);
//     pionWriter->SetEventCut(EventCut);
//     pionWriter->SetTrackCut(PionTrackCut);
//     pionWriter->SetV0Cut(AntiV0Cut);
//   cout << "WRITER SET UP.... " << endl;
    
  //   set up a microDstWriter 
//   StHbtTTreeReader* kaonWriter = new StHbtTTreeReader(ioMaker,"./",".kaon.hbtTTreeMuDst");
//   TheManager->AddEventWriter(kaonWriter);
//   kaonWriter->SetEventCut(EventCut);
//   kaonWriter->SetTrackCut(KaonTrackCut);
//   kaonWriter->SetV0Cut(AntiV0Cut);
//   cout << "WRITER SET UP.... " << endl;
    
  //   set up a microDstWriter 
//   StHbtBinaryReader* protonWriter = new StHbtBinaryReader(ioMaker,dirName,"dummy",".proton.microDst");
//   TheManager->AddEventWriter(protonWriter);
//   protonWriter->SetEventCut(EventCut);
//   protonWriter->SetTrackCut(ProtonTrackCut);
//   protonWriter->SetV0Cut(AntiV0Cut);
//   cout << "WRITER SET UP.... " << endl;
    
  //   set up a microDstWriter 
// StHbtBinaryReader* K0Writer = new StHbtBinaryReader(ioMaker,dirName,"dummy",".k0.microDst");
// TheManager->AddEventWriter(K0Writer);
// K0Writer->SetEventCut(EventCut);
// K0Writer->SetTrackCut(AntiTrackCut);
// K0Writer->SetV0Cut(K0Cut);
// cout << "WRITER SET UP.... " << endl;
    
  //   set up a microDstWriter 
//  StHbtBinaryReader* LPWriter = new StHbtBinaryReader(ioMaker,dirName,"dummy",".lam.prot.microDst");
//  TheManager->AddEventWriter(LPWriter);
//  LPWriter->SetEventCut(EventCut);
//  LPWriter->SetTrackCut(ProtonTrackCut);
//  LPWriter->SetV0Cut(LambdaCut);
//  cout << "WRITER SET UP.... " << endl;
    
  //   set up a microDstWriter 
//     StHbtBinaryReader* OmegaWriter = new StHbtBinaryReader(ioMaker,dirName,"dummy",".Omega.microDst");
//   TheManager->AddEventWriter(OmegaWriter);
//   OmegaWriter->SetEventCut(EventCut);
//   OmegaWriter->SetTrackCut(KaonMinusTrackCut);
//   OmegaWriter->SetV0Cut(LambdaCut);
//   cout << "WRITER SET UP.... " << endl;
 
 //  set up a microDstWriter  with multiple track cuts
//   StHbtBinaryReader* pionProtonWriter = new StHbtBinaryReader(ioMaker,dirName,"dummy",".pi.prot.microDst");
//   TheManager->AddEventWriter(pionProtonWriter);
//   pionProtonWriter->SetEventCut(EventCut);
//   // set up multi track cut
//   StHbtMultiTrackCut* multiTrackCut = new StHbtMultiTrackCut();
//   multiTrackCut->AddTrackCut(PionTrackCut);
//   multiTrackCut->AddTrackCut(ProtonTrackCut);
//   pionProtonWriter->SetTrackCut(multiTrackCut);
//   pionProtonWriter->SetV0Cut(AntiV0Cut);
//   cout << "WRITER SET UP.... " << endl;
    
 
 // ------------------ end of setting up hbt stuff ------------------ //

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

 cout << " End of Analysis " << endl;
}



