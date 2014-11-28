// example how to access memberfunctions from a correlation function 
//  ((MinvCorrFctn*) ((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->CorrFctn(0))->Difference()->Draw()

// ((MinvCorrFctnY_vs_Pt*) ((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->CorrFctn(2))->Difference()->Draw("colz")

// ((MinvCorrFctnArmenteros*) ((StHbtAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0))->CorrFctn(1))->Difference()->Draw("colz")

// examples to access member function from cutMonitors (here the member functions return a pointer to a histogram)  

// for Like Sign Analysis
// ((MinvLikeSignCorrFctn*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->CorrFctn(0))->Numerator()->Draw()
// ((MinvLikeSignCorrFctn*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->CorrFctn(0))->MixedEventDenominator()->Draw()
// ((MinvLikeSignCorrFctn*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->CorrFctn(0))->PositiveDenominator()->Draw()
// ((MinvLikeSignCorrFctn*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->CorrFctn(0))->NegativeDenominator()->Draw()
// ((MinvLikeSignCorrFctn*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->CorrFctn(0))->MixedEventDifference()->Draw()
// ((MinvLikeSignCorrFctn*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->CorrFctn(0))->LikeSignDifference()->Draw()
// ((trackCutMonitor_P_vs_Dedx*) ((franksTrackCut*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->FirstParticleCut())->PassMonitor(0))->Histo()->Draw()
// ((trackCutMonitor_P_vs_Dedx*) ((franksTrackCut*) ((StHbtLikeSignAnalysis*)((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(1))->FirstParticleCut())->FailMonitor(0))->Histo()->Draw()

#define STEVENT 0
#define MCEVENT 0  
#define ASSOCIATION 0 
#define STEVENTREADER 0 
#define MCEVENTREADER 0 
#define ASSOCIATIONREADER 0
#define XDFREADER 0
#define GSTARTXTREADER 0

#define ASCIIWRITER 0
#define ASCIIREADER 0

#define BINARYWRITER 0
#define BINARYREADER 1

#if ASCIIREADER
#define STEVENT 0
#define MCEVENT 0  
#define ASSOCIATION 0  
#define STEVENTREADER 0
#define MCEVENTREADER 0  
#define ASSOCIATIONREADER 0
#endif

#if BINARYREADER
#define STEVENT 0
#define MCEVENT 0  
#define ASSOCIATION 0  
#define STEVENTREADER 0
#define MCEVENTREADER 0  
#define ASSOCIATIONREADER 0
#endif

#define ANALYSIS

// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    

// keep pointers to Correlation Functions global, so you can have access to them...
class QinvCorrFctn;
QinvCorrFctn* QinvCF;

class QvecCorrFctn;
QvecCorrFctn* QvecCF;

class MinvCorrFctn;
MinvCorrFctn* MinvCF;
MinvCorrFctn* MinvCF2;
MinvCorrFctn* MinvCFrho;

class MinvCorrFctnM_vs_Pt;
MinvCorrFctnM_vs_Pt* MinvCFM_vs_Pt;

class MinvCorrFctnY_vs_Pt;
MinvCorrFctnY_vs_Pt* MinvCFY_vs_Pt;

class MinvCorrFctnM_vs_P;
MinvCorrFctnM_vs_P* MinvCFM_vs_P;

class p1_vs_p2CorrFctn;
p1_vs_p2CorrFctn* p1_vs_p2_CF;

class YCorrFctn;
YCorrFctn* YdiffCF;

class pDiffCorrFctn;
pDiffCorrFctn* pDiffCF;

class p_vs_angleCorrFctn;
p_vs_angleCorrFctn* p_vs_angleCF;

class phiMeas_vs_phiCalcCorrFctn;
phiMeas_vs_phiCalcCorrFctn* phiMeas_vs_phiCalcCF;

// keep pointers to Analysis global, so you can have access to themm ...
class StHbtAnalysis;
StHbtAnalysis* phiAnal;
StHbtAnalysis* phiAnal2;
StHbtAnalysis* phiAnal3;
StHbtAnalysis* phiAnalCopy;
StHbtAnalysis* deltaAnal;
StHbtAnalysis* rhoAnal;
StHbtAnalysis* lambdaAnal;

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


void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList, const char*);


//==========================================================================================
//==========================================================================================
void StHbtExampleII(const Int_t nevents=9999,
	  const Char_t *path=venusPath,
	  const Char_t *file=venusFile,
	  const char* histoFile="test.histo.root")
{ 
  const char *fileListQQ[]={0,0};
  if (path[0]=='-') {
    fileListQQ[0]=file;
  } else {
    fileListQQ[0] = gSystem->ConcatFileName(path,file);
  }
  StHbtExampleQQ(nevents,fileListQQ,histoFile);
}
//==========================================================================================
//==========================================================================================
void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList, const char* histoFile)
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
#if XDFREADER
gSystem->Load("xdf2root");
gSystem->Load("St_xdfin_Maker");
#endif
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
#if STEVENT
StIOMaker* ioMaker = new StIOMaker("IO","r",setFiles,"bfcTree");
ioMaker->SetDebug();

ioMaker->SetIOMode("r");
ioMaker->SetDebug();
ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch
#endif
#if MCEVENT
StIOMaker* ioMaker = new StIOMaker("IO","r",setFiles,"bfcTree");
ioMaker->SetDebug();

ioMaker->SetIOMode("r");
ioMaker->SetDebug();
ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
ioMaker->SetBranch("geantBranch",0,"r"); //activate EventBranch
#endif
#if ASSOCIATION
StIOMaker* ioMaker = new StIOMaker("IO","r",setFiles,"bfcTree");
ioMaker->SetDebug();

ioMaker->SetIOMode("r");
ioMaker->SetDebug();
ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
cout << " files open" << endl;
ioMaker->SetBranch("geantBranch",0,"r"); //activate EventBranch
cout << " files open" << endl;
ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch
cout << " files open" << endl;
#endif
#if XDFREADER
StIOMaker* ioMaker = new StIOMaker("IO","r",setFiles,"bfcTree");
ioMaker->SetDebug();
ioMaker->SetBranch("*",0,"r"); //activate EventBranch
#endif

// ***********
// Event Maker 
// ***********
#if STEVENT
StEventMaker* eventMaker = new StEventMaker("events","title");
 eventMaker->doLoadTpcHits=0;
 eventMaker->doLoadFtpcHits=0;
 eventMaker->doLoadSvtHits=0;
cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << endl;
#endif
#if MCEVENT
StMcEventMaker*     mcEventMaker = new StMcEventMaker; // Make an instance...
cout << "StMcEventMaker instantiated"<<endl; 
#endif
#if ASSOCIATION
StEventMaker* eventMaker = new StEventMaker("events","title");
cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << endl;
StMcEventMaker*     mcEventMaker = new StMcEventMaker; // Make an instance...
cout << "StMcEventMaker instantiated"<<endl; 
StAssociationMaker* associationMaker = new StAssociationMaker; // Make an instance...
cout << "StAssociationMaker instantiated"<<endl; 
#endif   

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
#if STEVENTREADER    
// *****************************************
// set up StHbtMcEventReader as Event Reader
// *****************************************
StStandardHbtEventReader* Reader = new StStandardHbtEventReader;
Reader->SetTheEventMaker(eventMaker);     // gotta tell the reader where it should read from
TheManager->SetEventReader(Reader);
cout << "READER SET UP.... " << endl;
#endif
#if MCEVENTREADER
// *****************************************
// set up StHbtMcEventReader as Event Reader
// *****************************************
StHbtMcEventReader* Reader = new StHbtMcEventReader;
Reader->SetTheMcEventMaker(mcEventMaker);     // gotta tell the reader where it should read from
TheManager->SetEventReader(Reader);
cout << "READER SET UP.... " << endl;
#endif
#if ASSOCIATIONREADER
// ********************************************
// set up StHbtAssociationReader as Event Reader
// ********************************************
//    StHbtAssociationReader* Reader = new StHbtAssociationReader();
StHbtAssociationReader* Reader = new StHbtAssociationReader();
Reader->SetTheEventMaker(eventMaker);     // gotta tell the reader where it should read from
Reader->SetTheMcEventMaker(mcEventMaker);     // gotta tell the reader where it should read from
Reader->SetTheAssociationMaker(associationMaker);     // gotta tell the reader where it should read from
Reader->SetTheMcEventMaker(mcEventMaker);     // gotta tell the reader where it should read from
TheManager->SetEventReader(Reader);
// define cuts for the association maker
StMcParameterDB* parameterDB = StMcParameterDB::instance();
parameterDB->setXCut(0.1);
parameterDB->setZCut(0.2);
parameterDB->setReqCommonHits(10);
cout << "READER SET UP.... " << endl;
#endif

#if ASCIIWRITER
 StHbtAsciiReader* asciiWriter = new StHbtAsciiReader;
 asciiWriter->SetFileName("test1.asc");
 TheManager->AddEventWriter(asciiWriter);
// set up the front loaded cuts
 mikesEventCut* asciiFrontLoadedEventCut = new mikesEventCut;
 asciiFrontLoadedEventCut->SetEventMult(0,100000);      // selected multiplicity range
 asciiFrontLoadedEventCut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
 asciiWriter->SetEventCut(asciiFrontLoadedEventCut);
 franksTrackCut* asciiFrontLoadedTrackCut = new franksTrackCut;
 asciiFrontLoadedTrackCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 asciiFrontLoadedTrackCut->SetNSigmaKaon(-3.,3.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 asciiFrontLoadedTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 asciiFrontLoadedTrackCut->SetNHits(5,1000);           // range on number of TPC hits on the track
 asciiFrontLoadedTrackCut->SetP(0.0,5.0);               // range in P
 asciiFrontLoadedTrackCut->SetPt(0.0,5.0);              // range in Pt
 asciiFrontLoadedTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 asciiFrontLoadedTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 asciiFrontLoadedTrackCut->SetCharge(0);                // no cut on charge
 asciiFrontLoadedTrackCut->SetMass(0.494);              // kaon mass
 //asciiWriter->SetTrackCut(asciiFrontLoadedTrackCut);
 //   set up a microDstWriter 
 cout << "WRITER SET UP.... " << endl;
#endif
#if ASCIIREADER
 //   set up a microDstWriter 
 StHbtAsciiReader* Reader = new StHbtAsciiReader;
 Reader->SetFileName(*fileList);
 //Reader->SetFileName("test1.asc");
 TheManager->SetEventReader(Reader);
 cout << "READER SET UP.... " << endl;
#endif
#if BINARYWRITER
 //   set up a microDstWriter 
 StHbtBinaryReader* binaryWriter = new StHbtBinaryReader(ioMaker);  // retrieve filename from ioMaker
 //StHbtBinaryReader* binaryWriter = new StHbtBinaryReader();           // specify filename
 binaryWriter->SetFileName("test1.bin");
 TheManager->AddEventWriter(binaryWriter);
 // set up the front loaded cuts
 mikesEventCut* binaryFrontLoadedEventCut = new mikesEventCut;
 binaryFrontLoadedEventCut->SetEventMult(0,100000);      // selected multiplicity range
 binaryFrontLoadedEventCut->SetVertZPos(-35.0,35.0);     // selected range of vertex z-position
 binaryWriter->SetEventCut(binaryFrontLoadedEventCut);
 franksTrackCut* binaryFrontLoadedTrackCut = new franksTrackCut;
 binaryFrontLoadedTrackCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 binaryFrontLoadedTrackCut->SetNSigmaKaon(-3.,3.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 binaryFrontLoadedTrackCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 binaryFrontLoadedTrackCut->SetNHits(5,1000);           // range on number of TPC hits on the track
 binaryFrontLoadedTrackCut->SetP(0.0,5.0);               // range in P
 binaryFrontLoadedTrackCut->SetPt(0.0,5.0);              // range in Pt
 binaryFrontLoadedTrackCut->SetRapidity(-1.5,1.5);       // range in rapidity
 binaryFrontLoadedTrackCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
 binaryFrontLoadedTrackCut->SetCharge(0);                // no cut on charge
 binaryFrontLoadedTrackCut->SetMass(0.494);              // kaon mass
 binaryWriter->SetTrackCut(binaryFrontLoadedTrackCut);
 cout << "WRITER SET UP.... " << endl;
#endif
#if BINARYREADER
 //   set up a microDstWriter 
 StHbtBinaryReader* Reader = new StHbtBinaryReader(0,*fileList,0);
 //Reader->SetFileName("/star/u2d/laue/MDC3/MicroDst/rcf0110.kaon.microDst");
 //Reader->AddFileList("test.lis");
  TheManager->SetEventReader(Reader);
 cout << "READER SET UP.... " << endl;
#endif
#if XDFREADER  
 // set up StHbtXDFReader as Event Reader
 StHbtXDFReader* Reader = new StHbtXDFReader("evgen","evgen/particle");
 Reader->AddAcceptedParticle(-321);  // kaon+
 Reader->AddAcceptedParticle(+321);  // kaon-
 Reader->AddAcceptedMother(333);     // phi 

 //Reader->AddAcceptedParticle(+211);  // pion+
 //Reader->AddAcceptedParticle(+2212); // proton
 //Reader->AddAcceptedMother(2224);    // delta++ 

 TheManager->SetEventReader(Reader);
 cout << "READER SET UP.... " << endl;
#endif
#if GSTARTXTREADER
 StHbtGstarTxtReader* Reader = new StHbtGstarTxtReader;
 // Reader->SetFileName("/star/rcf/pwg/hbt/GstarTextFiles/blended_events.txt");
 Reader->SetFileName("/star/rcf/data06/evgen/auau200/hbt/default/midcentral/year_2000/hadronic_on/gen/evgen.41.evt");
 TheManager->SetEventReader(Reader);
#endif 

    
 // define example particle cut and cut monitors to use in the analyses
 // example particle cut
 franksTrackCut* aTrackCut = new franksTrackCut;  // use "frank's" particle cut object
 aTrackCut->SetNSigmaPion(3.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 aTrackCut->SetNSigmaKaon(-3.,3.);       // number of Sigma in TPC dEdx away from nominal kaon dEdx
 aTrackCut->SetNSigmaProton(-1000.,-1.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 aTrackCut->SetNHits(5,50);             // range on number of TPC hits on the track
 aTrackCut->SetP(0.2,0.8);              // range in P
 aTrackCut->SetPt(0.0,5.0);             // range in Pt
 aTrackCut->SetRapidity(-1.5,1.5);      // range in rapidity
 aTrackCut->SetDCA(0.0,2.);             // range in Distance of Closest Approach to primary vertex
 aTrackCut->SetCharge(+1);              // want positive kaons
 aTrackCut->SetMass(0.494);             // kaon mass
 // define example track cut monitor
 trackCutMonitor_P_vs_Dedx* aDedxMoniPos = new trackCutMonitor_P_vs_Dedx(+1,"P_vs_Dedx +","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,1.2,100,0.,1e-5);
 trackCutMonitor_P_vs_Dedx* aDedxMoniNeg = new trackCutMonitor_P_vs_Dedx(-1,"P_vs_Dedx -","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,1.2,100,0.,1e-5);

 // now, we define another analysis that runs simultaneously with the previous one.
 // this one looks at K+K- correlations (so NONidentical particles) in invariant mass
 
 // ****************************************** // 
 // * franks phi analysis - by Frank Laue, OSU //
 // ****************************************** // 
 // 0) now define an analysis...
 // StHbtAnalysis* phiAnal = new StHbtAnalysis;
 phiAnal = new StHbtAnalysis;
 // 1) set the Event cuts for the analysis
 mikesEventCut* phiEvcut = new mikesEventCut;  // use "mike's" event cut object
 phiEvcut->SetEventMult(0,100000);      // selected multiplicity range
 phiEvcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
 eventCutMonitor_Mult* multMoniPass = new eventCutMonitor_Mult();
 eventCutMonitor_Mult* multMoniFail = new eventCutMonitor_Mult();
 phiEvcut->AddCutMonitor(multMoniPass, multMoniFail);
 phiAnal->SetEventCut(phiEvcut);          // this is the event cut object for this analsys
 // 2) set the Track (particle) cuts for the analysis
 franksTrackCut* kaonTrkcut = new franksTrackCut( *aTrackCut );  // copy from example
 // new particle cut moni
 trackCutMonitor_P_vs_Dedx* dedxMoniPosPass = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos);
 trackCutMonitor_P_vs_Dedx* dedxMoniPosFail = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos);
 kaonTrkcut->AddCutMonitor( dedxMoniPosPass, dedxMoniPosFail);
 // new particle cut moni
 trackCutMonitor_P*    pMoni1 = new  trackCutMonitor_P;
 trackCutMonitor_P*    pMoni2 = new  trackCutMonitor_P("P","momentum (GeV/c)",20, 0., 4.);
 kaonTrkcut->AddCutMonitor( pMoni1, pMoni2);
 // new particle cut moni
 trackCutMonitor_Pt*    ptMoni1 = new  trackCutMonitor_Pt;
 trackCutMonitor_Pt*    ptMoni2 = new  trackCutMonitor_Pt;
 kaonTrkcut->AddCutMonitor( ptMoni1, ptMoni2);
 // new particle cut moni
 trackCutMonitor_Y_vs_Pt*    yptMoni1 = new  trackCutMonitor_Y_vs_Pt;
 trackCutMonitor_Y_vs_Pt*    yptMoni2 = new  trackCutMonitor_Y_vs_Pt;
 kaonTrkcut->AddCutMonitor( yptMoni1, yptMoni2);
 // new particle cut moni
 trackCutMonitor_DCA*    DCAMoni1 = new  trackCutMonitor_DCA("DCA","DCA (cm)",100,0.,10.);
 trackCutMonitor_DCA*    DCAMoni2 = new  trackCutMonitor_DCA("DCA","DCA (cm)",100,0.,10.);
 kaonTrkcut->AddCutMonitor( DCAMoni1, DCAMoni2 );
 phiAnal->SetFirstParticleCut(kaonTrkcut);  // this is the track cut for the "first" particle
 
 // copy second particle cut from first particle cut
 franksTrackCut* antikaonTrkcut = new franksTrackCut( *((franksTrackCut*)phiAnal->FirstParticleCut()) );  
 antikaonTrkcut->SetCharge(-1); 
 phiAnal->SetSecondParticleCut(antikaonTrkcut);  // this is the track cut for the "first" particle
 // new particle cut
 trackCutMonitor_P_vs_Dedx* dedxMoniNegPass = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
 trackCutMonitor_P_vs_Dedx* dedxMoniNegFail = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
 antikaonTrkcut->AddCutMonitor( dedxMoniNegPass, dedxMoniNegFail);

 // 3) set the Pair cuts for the analysis
 mikesPairCut* phiPairCut = new mikesPairCut;  // use "mike's" pair cut object
 //franksPairCut* phiPairCut = new franksPairCut;  // use "frank's" pair cut object
 phiAnal->SetPairCut(phiPairCut);         // this is the pair cut for this analysis
 // 4) set the number of events to mix (per event)
 phiAnal->SetNumEventsToMix(3); 
 // ********************************************************************
 // 5) now set up the correlation functions that this analysis will make
 // ********************************************************************
 // define example Minv correlation function
 MinvCorrFctn* MinvCF = new MinvCorrFctn("Minv",100,0.95,1.25); 
 phiAnal->AddCorrFctn(MinvCF);   // adds the just-defined correlation function to the analysis

 MinvCorrFctnArmenteros* MinvCFArm = new MinvCorrFctnArmenteros("ArmenterosPodolanski",200,-1.,1.,300,0.,.3); 
 phiAnal->AddCorrFctn(MinvCFArm);   // adds the just-defined correlation function to the analysis
 
 MinvCFY_vs_Pt = new MinvCorrFctnY_vs_Pt("MinvCF dn/d(Y_vs_Pt)",31, -1.5, 1.5, 20, 0., 2. ); // defines a Minv function
 phiAnal->AddCorrFctn(MinvCFY_vs_Pt);   // adds the just-defined correlation function to the analysis
 
 MinvCFM_vs_Pt = new MinvCorrFctnM_vs_Pt("MinvCM dn/d(M_vs_Pt)",100,0.98, 1.18, 40, 0., 2. ); // defines a Minv function
 phiAnal->AddCorrFctn(MinvCFM_vs_Pt);   // adds the just-defined correlation function to the analysis
 
 /*
   MinvCFM_vs_P  = new MinvCorrFctnM_vs_P("Mass (GeV/c^2) vs Momentum (GeV/c)",50,0.98,1.08,20,0.,2.);
   phiAnal->AddCorrFctn(MinvCFM_vs_P);   // adds the just-defined correlation function to the analysis
   
   p1_vs_p2CF  = new p1_vs_p2CorrFctn("K+ momentum  (GeV/c^) vs K- momentum (GeV/c)",20,0.,2., 20,0.,2.);
   phiAnal->AddCorrFctn(p1_vs_p2CF);   // adds the just-defined correlation function to the analysis
   
   YCF  = new YCorrFctn("Rapidity",20,-2.,2.);
   phiAnal->AddCorrFctn(YCF);   // adds the just-defined correlation function to the analysis
   
   pDiffCF  = new pDiffCorrFctn(" p1-p2 (GeV/c)",100,0.,.1);
   phiAnal->AddCorrFctn(pDiffCF);   // adds the just-defined correlation function to the analysis
   
   p_vs_angleCF  = new p_vs_angleCorrFctn("K+/- momentum  (GeV/c^) vs opening angle",20,0.,2., 20,0.,2.*3.1415927);
   phiAnal->AddCorrFctn(p_vs_angleCF);   // adds the just-defined correlation function to the analysis
   
   phiMeas_vs_phiCalcCF  = new phiMeas_vs_phiCalcCorrFctn(" phiMeas vs phiCalc",20,-1.,1.,20,-1.,1.);
   phiAnal->AddCorrFctn(phiMeas_vs_phiCalcCF);   // adds the just-defined correlation function to the analysis
 */
 
 // now add as many more correlation functions to the Analysis as you like..
 // 6) add the Analysis to the AnalysisCollection
 TheManager->AddAnalysis(phiAnal);


 // ***************************************************** // 
 // * franks phi ---> e+ e- analysis - by Frank Laue, OSU //
 // ***************************************************** // 
 // 0) now define an analysis...
 StHbtAnalysis* eeAnal = new StHbtLikeSignAnalysis;
 // 1) set the Event cuts for the analysis
 mikesEventCut* eeEvcut = new mikesEventCut;  // use "mike's" event cut object
 eeEvcut->SetEventMult(0,100000);      // selected multiplicity range
 eeEvcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
 eeAnal->SetEventCut(eeEvcut);          // this is the event cut object for this analsys
 // 2) set the Track (particle) cuts for the analysis
 franksTrackCut* electronTrkCut = new franksTrackCut( *aTrackCut );  // copy from example
 electronTrkCut->SetNSigmaElectron(-1e2,+1e2);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 electronTrkCut->SetNSigmaPion(-1e5,+1e5);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 electronTrkCut->SetNSigmaKaon(-1e5,+1e5);       // number of Sigma in TPC dEdx away from nominal kaon dEdx
 electronTrkCut->SetNSigmaProton(-1e5,+1e5); // number of Sigma in TPC dEdx away from nominal proton dEdx
#ifdef MCEVENT
 electronTrkCut->SetNSigmaElectron(-3,+3);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 electronTrkCut->SetNSigmaPion(-1e5,+1e5);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 electronTrkCut->SetNSigmaKaon(-1e5,+1e5);       // number of Sigma in TPC dEdx away from nominal kaon dEdx
 electronTrkCut->SetNSigmaProton(-1e5,+1e5); // number of Sigma in TPC dEdx away from nominal proton dEdx
#endif
 electronTrkCut->SetNHits(5,100);             // range on number of TPC hits on the track
 electronTrkCut->SetP(0.,2.0);              // range in P
 electronTrkCut->SetPt(0.0,2.0);             // range in Pt
 electronTrkCut->SetRapidity(-1.5,1.5);      // range in rapidity
 electronTrkCut->SetDCA(2.0,50);             // range in Distance of Closest Approach to primary vertex
 electronTrkCut->SetCharge(+1);              // want positive kaons
 electronTrkCut->SetMass(0.000511);          // kaon mass
 // new particle cut moni
 trackCutMonitor_P_vs_Dedx* dedxMoniPosPass = new trackCutMonitor_P_vs_Dedx(+1,"P_vs_Dedx +","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,5.0,100,0.,1e-5);
 trackCutMonitor_P_vs_Dedx* dedxMoniPosFail = new trackCutMonitor_P_vs_Dedx(-1,"P_vs_Dedx -","Momentum (GeV/c) vs Energy loss (a.u.)",100,0.,5.0,100,0.,1e-5);
 electronTrkCut->AddCutMonitor( dedxMoniPosPass, dedxMoniPosFail);
 // new particle cut moni
 trackCutMonitor_P*    pMoni1 = new  trackCutMonitor_P;
 trackCutMonitor_P*    pMoni2 = new  trackCutMonitor_P("P","momentum (GeV/c)",20, 0., 4.);
 electronTrkCut->AddCutMonitor( pMoni1, pMoni2);
 // new particle cut moni
 trackCutMonitor_Pt*    ptMoni1 = new  trackCutMonitor_Pt;
 trackCutMonitor_Pt*    ptMoni2 = new  trackCutMonitor_Pt;
 electronTrkCut->AddCutMonitor( ptMoni1, ptMoni2);
 // new particle cut moni
 trackCutMonitor_Y_vs_Pt*    yptMoni1 = new  trackCutMonitor_Y_vs_Pt;
 trackCutMonitor_Y_vs_Pt*    yptMoni2 = new  trackCutMonitor_Y_vs_Pt;
 electronTrkCut->AddCutMonitor( yptMoni1, yptMoni2);
 // new particle cut moni
 trackCutMonitor_DCA*    DCAMoni1 = new  trackCutMonitor_DCA("DCA","DCA (cm)",100,0.,10.);
 trackCutMonitor_DCA*    DCAMoni2 = new  trackCutMonitor_DCA("DCA","DCA (cm)",100,0.,10.);
 electronTrkCut->AddCutMonitor( DCAMoni1, DCAMoni2 );
 eeAnal->SetFirstParticleCut(electronTrkCut);  // this is the track cut for the "first" particle
 
 // copy second particle cut from first particle cut
 franksTrackCut* antiElectronTrkCut = new franksTrackCut( *((franksTrackCut*)eeAnal->FirstParticleCut()) );  
 antiElectronTrkCut->SetCharge(-1); 
 eeAnal->SetSecondParticleCut(antiElectronTrkCut);  // this is the track cut for the "first" particle
 // new particle cut
 trackCutMonitor_P_vs_Dedx* dedxMoniNegPass = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
 trackCutMonitor_P_vs_Dedx* dedxMoniNegFail = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
 antiElectronTrkCut->AddCutMonitor( dedxMoniNegPass, dedxMoniNegFail);

 // 3) set the Pair cuts for the analysis
 mikesPairCut* eePairCut = new mikesPairCut;  // use "mike's" pair cut object
 //franksPairCut* eePairCut = new franksPairCut;  // use "frank's" pair cut object
 eeAnal->SetPairCut(eePairCut);         // this is the pair cut for this analysis
 // 4) set the number of events to mix (per event)
 eeAnal->SetNumEventsToMix(2); 
 // ********************************************************************
 // 5) now set up the correlation functions that this analysis will make
 // ********************************************************************
 // define example MinvLikeSign correlation function
 MinvLikeSignCorrFctn* MinvLikeSignCFconv = new MinvLikeSignCorrFctn("MinvLikeSign",500,0.,.25); 
 eeAnal->AddCorrFctn(MinvLikeSignCFconv);   // adds the just-defined correlation function to the analysis
 MinvLikeSignCorrFctn* MinvLikeSignCF = new MinvLikeSignCorrFctn("MinvLikeSign",100,0.95,1.15); 
 eeAnal->AddCorrFctn(MinvLikeSignCF);   // adds the just-defined correlation function to the analysis

 // now add as many more correlation functions to the Analysis as you like..
 // 6) add the Analysis to the AnalysisCollection
 TheManager->AddAnalysis(eeAnal);

 // ********************************************* // 
 // * franks lambda analysis - by Frank Laue, OSU //
 // ********************************************* // 
 //StHbtAnalysis* lambdaAnal = new StHbtAnalysis( *phiAnal );
 lambdaAnal = new StHbtAnalysis( *phiAnal );
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetNSigmaPion(-1000.0,1000.0); // proton
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetNSigmaKaon(-1000.0,1000.0);
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetNSigmaProton(-3.0,3.0);
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetP(0.,2.);
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetPt(0.,2.);
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetCharge(-1);
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetMass(0.938);
 ((franksTrackCut*)lambdaAnal->FirstParticleCut())->SetDCA(8.0,16.);
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetNSigmaPion(-3.0,3.0); //pion
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetNSigmaKaon(-1000.0,1000.0);
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetNSigmaProton(-1000.0,1000.0);
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetP(0.,2.);
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetPt(0.,2.);
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetCharge(-1);
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetMass(0.139);
 ((franksTrackCut*)lambdaAnal->SecondParticleCut())->SetDCA(8.0,16.);
 //TheManager->AddAnalysis(lambdaAnal);
 
 // ********************************************* // 
 // * franks lambda analysis - by Frank Laue, OSU //
 // ********************************************* // 
 StHbtAnalysis* lambdaAnal2 = new StHbtAnalysis( *lambdaAnal );
 delete  ((franksPairCut*)lambdaAnal2->PairCut());
 franksPairCut* lambdaAnal2PairCut = new franksPairCut;  // use "frank's" pair cut object
 lambdaAnal2->SetPairCut(lambdaAnal2PairCut);            // this is the pair cut for this analysis
 //TheManager->AddAnalysis(lambdaAnal2);

 // ****************************************** // 
 // * franks rho analysis - by Frank Laue, OSU //
 // ****************************************** // 
 // 0) now define an analysis...
 // StHbtAnalysis* rhoAnal = new StHbtAnalysis;
 rhoAnal = new StHbtAnalysis;
 // 1) set the Event cuts for the analysis
 mikesEventCut* rhoEvcut = new mikesEventCut;  // use "mike's" event cut object
 rhoEvcut->SetEventMult(0,100000);      // selected multiplicity range
 rhoEvcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
 rhoAnal->SetEventCut(rhoEvcut);          // this is the event cut object for this analsys
 // 2) set the Track (particle) cuts for the analysis
 franksTrackCut* piPosTrkcut = new franksTrackCut;  // use "frank's" particle cut object
 piPosTrkcut->SetNSigmaPion(-2.0,+2.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 piPosTrkcut->SetNSigmaKaon(-1000.,+1000.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 piPosTrkcut->SetNSigmaProton(-1000.,+1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 piPosTrkcut->SetNHits(10,50);            // range on number of TPC hits on the track
 piPosTrkcut->SetP(0.1,0.5);              // range in P
 piPosTrkcut->SetPt(0.1,2.0);             // range in Pt
 piPosTrkcut->SetRapidity(-1.5,1.5);      // range in rapidity
 piPosTrkcut->SetDCA(0.0,2.);             // range in Distance of Closest Approach to primary vertex
 piPosTrkcut->SetCharge(+1);              // want positive kaons
 piPosTrkcut->SetMass(0.139);             // kaon mass
 rhoAnal->SetFirstParticleCut(piPosTrkcut);  // this is the track cut for the "first" particle
 // copy second particle cut from first particle cut
 franksTrackCut* piNegTrkCut = new franksTrackCut( *((franksTrackCut*)rhoAnal->FirstParticleCut()) );  
 piNegTrkCut->SetCharge(-1); 
 rhoAnal->SetSecondParticleCut(piNegTrkCut);  // this is the track cut for the "first" particle
 // 3) set the Pair cuts for the analysis
 mikesPairCut* rhoPairCut = new mikesPairCut;  // use "frank's" pair cut object
 //franksPairCut* rhoPairCut = new franksPairCut;  // use "frank's" pair cut object
 //rhoPairCut->SetPDiff(0.,1000.);             // difference of pairs in momentum
 //    rhoPairCut->SetAngle(0.,180.);           // opening angle
 rhoAnal->SetPairCut(rhoPairCut);         // this is the pair cut for this analysis
 // 4) set the number of events to mix (per event)
 rhoAnal->SetNumEventsToMix(10);        
 // ********************************************************************
 // 5) now set up the correlation functions that this analysis will make
 // ********************************************************************
 // define example Minv correlation function
 MinvCorrFctn* MinvCFrho = new MinvCorrFctn("Minv",100,0.4,.9); 
 // rhoAnal->AddCorrFctn(MinvCFrho);   // adds the just-defined correlation function to the analysis
 
 // now add as many more correlation functions to the Analysis as you like..
 // 6) add the Analysis to the AnalysisCollection
 //TheManager->AddAnalysis(rhoAnal);
 
 // ******************************************** // 
 // * franks delta analysis - by Frank Laue, OSU //
 // ******************************************** // 
 // 0) now define an analysis...
 //StHbtAnalysis* deltaAnal = new StHbtAnalysis;
 deltaAnal = new StHbtAnalysis;
 // 1) set the Event cuts for the analysis
 mikesEventCut* deltaEvcut = new mikesEventCut;  // use "mike's" event cut object
 deltaEvcut->SetEventMult(0,100000);      // selected multiplicity range
 deltaEvcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
 deltaAnal->SetEventCut(deltaEvcut);          // this is the event cut object for this analsys
 // 2) set the Track (particle) cuts for the analysis
 franksTrackCut* pionTrkcut = new franksTrackCut;  // use "frank's" particle cut object
 pionTrkcut->SetNSigmaPion(-2.0,+2.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 pionTrkcut->SetNSigmaKaon(-1000.,+1000.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
 pionTrkcut->SetNSigmaProton(-1000.,+1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 pionTrkcut->SetNHits(10,50);            // range on number of TPC hits on the track
 pionTrkcut->SetP(0.1,0.5);              // range in P
 pionTrkcut->SetPt(0.1,2.0);             // range in Pt
 pionTrkcut->SetRapidity(-1.5,1.5);      // range in rapidity
 pionTrkcut->SetDCA(0.0,2.);             // range in Distance of Closest Approach to primary vertex
 pionTrkcut->SetCharge(+1);              // want positive kaons
 pionTrkcut->SetMass(0.139);             // kaon mass
 deltaAnal->SetFirstParticleCut(pionTrkcut);  // this is the track cut for the "first" particle
 // copy second particle cut from first particle cut
 franksTrackCut* protonTrkcut = new franksTrackCut( *((franksTrackCut*)deltaAnal->FirstParticleCut()) );  
 protonTrkcut->SetNSigmaPion(-1000.0,+1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
 protonTrkcut->SetNSigmaProton(-2.,+2.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
 protonTrkcut->SetMass(0.938);             // kaon mass
 deltaAnal->SetSecondParticleCut(protonTrkcut);  // this is the track cut for the "first" particle
 // 3) set the Pair cuts for the analysis
 mikesPairCut* deltaPairCut = new mikesPairCut;  // use "frank's" pair cut object
 //franksPairCut* deltaPairCut = new franksPairCut;  // use "frank's" pair cut object
 //deltaPairCut->SetPDiff(0.,1000.);             // difference of pairs in momentum
 //    deltaPairCut->SetAngle(0.,180.);           // opening angle
 deltaAnal->SetPairCut(deltaPairCut);         // this is the pair cut for this analysis
 // 4) set the number of events to mix (per event)
 deltaAnal->SetNumEventsToMix(10);        
 // ********************************************************************
 // 5) now set up the correlation functions that this analysis will make
 // ********************************************************************
 // define example Minv correlation function
 MinvCorrFctn* MinvCFdelta = new MinvCorrFctn("Minv",100,1.0,1.4); 
 deltaAnal->AddCorrFctn(MinvCFdelta);   // adds the just-defined correlation function to the analysis
 
 // now add as many more correlation functions to the Analysis as you like..
 // 6) add the Analysis to the AnalysisCollection
 //TheManager->AddAnalysis(deltaAnal);
 


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
 // write some histos into a file
 TFile histoOutput(histoFile,"recreate");
 MinvCF->Numerator()->Write();
 MinvCF->Denominator()->Write();
 MinvCF->Difference()->Write();
 MinvCFY_vs_Pt->Numerator()->Write();
 MinvCFY_vs_Pt->Denominator()->Write();
 MinvCFY_vs_Pt->Difference()->Write();
 MinvCFM_vs_Pt->Numerator()->Write();
 MinvCFM_vs_Pt->Denominator()->Write();
 MinvCFM_vs_Pt->Difference()->Write();
 MinvCFArm->Numerator()->Write();
 MinvCFArm->Denominator()->Write();
 MinvCFArm->Difference()->Write();
((eventCutMonitor_Mult*)phiAnal->EventCut()->PassMonitor(0))->Histo()->Write();
 histoOutput->Write();
 histoOutput->Close();
}



