
// example how to access memberfunctions from a correlation function 
// ((MinvCorrFctn*) ((StHbtMaker*) chain->GetMaker("HBT"))->HbtManager()->Analysis(0)->CorrFctn(0))->Difference()->Draw()

// examples to access member function from cutMonitors (here the member functions return a pointer to a histogram)  
// ((trackCutMonitor_P_vs_Dedx*)phiAnal->FirstParticleCut()->PassMonitor(0))->Histo()->Draw()
// ((trackCutMonitor_P_vs_Dedx*)phiAnal->FirstParticleCut()->FailMonitor(0))->Histo()->Draw()
// ((eventCutMonitor_Mult*)phiAnal->EventCut()->PassMonitor(0))->Histo()->Draw()

#define STEVENT 1
#define MCEVENT 0  
#define ASSOCIATION 0 
#define STEVENTREADER 1
#define MCEVENTREADER 0 
#define ASSOCIATIONREADER 0

#define DSTWRITER 0
#define DSTREADER 0

#define ANALYSIS

#if DSTREADER
#define STEVENT 0
#define MCEVENT 0  
#define ASSOCIATION 0  
#define STEVENTREADER 0
#define MCEVENTREADER 0  
#define ASSOCIATIONREADER 0
#define DSTWRITER 0
#endif

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
StHbtAnalysis* deltaAnal;
StHbtAnalysis* rhoAnal;

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
const char *venusFile ="*geant.root";
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


void StHbtExampleQQ(const Int_t nevents, const Char_t **fileList);


//==========================================================================================
//==========================================================================================
void rho(const Int_t nevents=9999,
	  const Char_t *path=venusPath,
	  const Char_t *file=venusFile)
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
//gSystem->Load("StTreeMaker");
gSystem->Load("StIOMaker");
gSystem->Load("StarClassLibrary");
gSystem->Load("StEvent");
//gSystem->Load("StEventReaderMaker");
gSystem->Load("StEventMaker");
gSystem->Load("StMcEvent"); 
gSystem->Load("StMcEventMaker"); 
gSystem->Load("StAssociationMaker");
gSystem->Load("StMcAnalysisMaker");

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

// ***********
// Event Maker 
// ***********
#if STEVENT
StEventMaker* eventMaker = new StEventMaker("events","title");
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

#if DSTWRITER
// set up the front loaded cuts
mikesEventCut* frontLoadedEventCut = new mikesEventCut;
frontLoadedEventCut->SetEventMult(0,100000);      // selected multiplicity range
frontLoadedEventCut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
Reader->SetEventCut(frontLoadedEventCut);
franksTrackCut* frontLoadedParticleCut = new franksTrackCut;
frontLoadedParticleCut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
frontLoadedParticleCut->SetNSigmaKaon(-3.,3.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
frontLoadedParticleCut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
frontLoadedParticleCut->SetNHits(5,1000);           // range on number of TPC hits on the track
frontLoadedParticleCut->SetP(0.0,1.0);               // range in P
frontLoadedParticleCut->SetPt(0.0,1.0);              // range in Pt
frontLoadedParticleCut->SetRapidity(-1.5,1.5);       // range in rapidity
frontLoadedParticleCut->SetDCA(0.0,2.);              // range in Distance of Closest Approach to primary vertex
frontLoadedParticleCut->SetCharge(0);                // no cut on charge
frontLoadedParticleCut->SetMass(0.494);              // kaon mass
Reader->SetParticleCut(frontLoadedParticleCut);
//   set up a microDstWriter 
StHbtAsciiReader* Writer = new StHbtAsciiReader;
Writer->SetFileName("/scr22/laue/test.asc");
TheManager->SetEventWriter(Writer);
cout << "WRITER SET UP.... " << endl;
#endif

#if DSTREADER
    //   set up a microDstWriter 
    StHbtAsciiReader* Reader = new StHbtAsciiReader;
    Reader->SetFileName(*fileList);
    TheManager->SetEventReader(Reader);
    
    cout << "READER SET UP.... " << endl;
#endif

    
    // define example particle cut and cut monitors to use in the analyses
    // example particle cut
    franksTrackCut* aParticleCut = new franksTrackCut;  // use "frank's" particle cut object
    aParticleCut->SetNSigmaPion(3.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
    aParticleCut->SetNSigmaKaon(-3.,3.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    aParticleCut->SetNSigmaProton(-1000.,-1.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
    aParticleCut->SetNHits(10,50);            // range on number of TPC hits on the track
    aParticleCut->SetP(0.0,1.0);              // range in P
    aParticleCut->SetPt(0.1,2.0);             // range in Pt
    aParticleCut->SetRapidity(-1.5,1.5);      // range in rapidity
    aParticleCut->SetDCA(0.0,2.);             // range in Distance of Closest Approach to primary vertex
    aParticleCut->SetCharge(+1);              // want positive kaons
    aParticleCut->SetMass(0.494);             // kaon mass
    // example cut monitor
    trackCutMonitor_P_vs_Dedx* aDedxMoniPos = new trackCutMonitor_P_vs_Dedx(+1,"P_vs_Dedx +","Momentum (GeV/c) vs Energy loss (a.u.)",
								       100,0.,1.2,100,0.,1e-5);
    trackCutMonitor_P_vs_Dedx* aDedxMoniNeg = new trackCutMonitor_P_vs_Dedx(-1,"P_vs_Dedx -","Momentum (GeV/c) vs Energy loss (a.u.)",
								       100,0.,1.2,100,0.,1e-5);
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
    franksTrackCut* kaonTrkcut = new franksTrackCut( *aParticleCut );  // copy from example
    // new particle cut moni
    trackCutMonitor_P_vs_Dedx* dedxMoniPosPass = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos);
    trackCutMonitor_P_vs_Dedx* dedxMoniPosFail = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos);
    mess("alive I");
    kaonTrkcut->AddCutMonitor( dedxMoniPosPass, dedxMoniPosFail);
    mess("alive II");
     // new particle cut moni
    trackCutMonitor_P*    pMoni1 = new  trackCutMonitor_P;
    trackCutMonitor_P*    pMoni2 = new  trackCutMonitor_P("P","momentum (Gev/c)",20, 0., 4.);
    mess("alive III");
    kaonTrkcut->AddCutMonitor( pMoni1, pMoni2);
    mess("alive IV");
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
    franksPairCut* phiPairCut = new franksPairCut;  // use "frank's" pair cut object
    phiPairCut->SetPDiff(0.,1000.);             // difference of pairs in momentum
    //    phiPairCut->SetAngle(0.,180.);           // opening angle
    phiAnal->SetPairCut(phiPairCut);         // this is the pair cut for this analysis
    // 4) set the number of events to mix (per event)
    phiAnal->SetNumEventsToMix(5); 
    mess("alive III");
    // ********************************************************************
    // 5) now set up the correlation functions that this analysis will make
    // ********************************************************************
    // define example Minv correlation function
    MinvCorrFctn* MinvCF = new MinvCorrFctn("Minv",100,0.98,1.18); 
    MinvCF->SetEventCut(phiEvcut);
    phiAnal->AddCorrFctn(MinvCF);   // adds the just-defined correlation function to the analysis

    MinvCFY_vs_Pt = new MinvCorrFctnY_vs_Pt("MinvCF dn/d(Y_vs_Pt)",20, -2., 2., 20, 0., 2. ); // defines a Minv function
    phiAnal->AddCorrFctn(MinvCFY_vs_Pt);   // adds the just-defined correlation function to the analysis

    MinvCFM_vs_Pt = new MinvCorrFctnM_vs_Pt("MinvCM dn/d(M_vs_Pt)",50, 1.0, 1.04, 20, 0., 1. ); // defines a Minv function
    phiAnal->AddCorrFctn(MinvCFM_vs_Pt);   // adds the just-defined correlation function to the analysis

    
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

    // now add as many more correlation functions to the Analysis as you like..
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(phiAnal);

    // **************************** // 
    // * set up second phi analysis //
    // **************************** // 
    // 0) now define an analysis...
    phiAnal2 = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* phiEvcut2 = new mikesEventCut;  // use "mike's" event cut object
    phiEvcut2->SetEventMult(0,10000);      // selected multiplicity range
    phiEvcut2->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
    phiAnal2->SetEventCut(phiEvcut2); // use same 
    // 2) set the Track (particle) cuts for the analysis
    // 1st particle cut
    franksTrackCut* kaonTrkcut2 = new franksTrackCut( *aParticleCut );
    kaonTrkcut2->SetNSigmaKaon(-2.,2.); // but tighter cut
    trackCutMonitor_P_vs_Dedx* dedxMoniPosPass2 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos); // copy cutMoni from example
    trackCutMonitor_P_vs_Dedx* dedxMoniPosFail2 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos); 
    kaonTrkcut2->AddCutMonitor( dedxMoniPosPass2, dedxMoniPosFail2);
    phiAnal2->SetFirstParticleCut(kaonTrkcut2);  // this is the track cut for the "first" particle
    // 2nd particle cut
    franksTrackCut* antikaonTrkcut2 = new franksTrackCut( *((franksTrackCut*)phiAnal2->FirstParticleCut()) );// copy from 1st cut
    antikaonTrkcut2->SetCharge(-1); // but set negative charge
    trackCutMonitor_P_vs_Dedx* dedxMoniNegPass2 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg); // copy cutMoni from example
    trackCutMonitor_P_vs_Dedx* dedxMoniNegFail2 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
    antikaonTrkcut2->AddCutMonitor( dedxMoniNegPass2, dedxMoniNegFail2);
    phiAnal2->SetSecondParticleCut(antikaonTrkcut2);  // this is the track cut for the "first" particle
    // 3) set the Pair cuts for the analysis
    phiAnal2->SetPairCut(phiPairCut); // use same
    // 4) set the number of events to mix (per event)
    phiAnal2->SetNumEventsToMix(10);        
    // 5) now set up the correlation functions that this analysis will make
    MinvCorrFctn* MinvCF2 = new MinvCorrFctn(*MinvCF); // copy from 1st analysis
    MinvCF2->SetEventCut(phiEvcut);
    phiAnal2->AddCorrFctn(MinvCF2);
    // 6) add the Analysis to the AnalysisCollection
    //    TheManager->AddAnalysis(phiAnal2);

    // **************************** // 
    // * set up second phi analysis //
    // **************************** // 
    // 0) now define an analysis...
    phiAnal3 = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* phiEvcut3 = new mikesEventCut;  // use "mike's" event cut object
    phiEvcut3->SetEventMult(0,10000);      // selected multiplicity range
    phiEvcut3->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
    phiAnal3->SetEventCut(phiEvcut3); // use same 
    // 2) set the Track (particle) cuts for the analysis
    // 1st particle cut
    franksTrackCut* kaonTrkcut3 = new franksTrackCut( *aParticleCut );
    kaonTrkcut3->SetNSigmaKaon(-1.,1.);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    trackCutMonitor_P_vs_Dedx* dedxMoniPosPass3 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos); // copy cutMoni from example
    trackCutMonitor_P_vs_Dedx* dedxMoniPosFail3 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniPos); 
    kaonTrkcut3->AddCutMonitor( dedxMoniPosPass3, dedxMoniPosFail3);
    phiAnal3->SetFirstParticleCut(kaonTrkcut3);  // this is the track cut for the "first" particle
    // 2nd particle cut
    franksTrackCut* antikaonTrkcut3 = new franksTrackCut( *((franksTrackCut*)phiAnal3->FirstParticleCut()) );// copy from 1st cut
    antikaonTrkcut3->SetCharge(-1); // but set negative charge
    trackCutMonitor_P_vs_Dedx* dedxMoniNegPass3 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg); // copy cutMoni from example
    trackCutMonitor_P_vs_Dedx* dedxMoniNegFail3 = new  trackCutMonitor_P_vs_Dedx( *aDedxMoniNeg);
    antikaonTrkcut3->AddCutMonitor( dedxMoniNegPass3, dedxMoniNegFail3);
    phiAnal3->SetSecondParticleCut(antikaonTrkcut3);  // this is the track cut for the "first" particle
    // 3) set the Pair cuts for the analysis
    phiAnal3->SetPairCut(phiPairCut); // use same
    // 4) set the number of events to mix (per event)
    phiAnal3->SetNumEventsToMix(10);        
    // 5) now set up the correlation functions that this analysis will make
    MinvCorrFctn* MinvCF3 = new MinvCorrFctn(*MinvCF); // copy from 1st analysis
    MinvCF3->SetEventCut(phiEvcut);
    phiAnal3->AddCorrFctn(MinvCF3);
    // 6) add the Analysis to the AnalysisCollection
    //    TheManager->AddAnalysis(phiAnal3);

    // ------------------ end of setting up hbt stuff ------------------ //

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
    franksPairCut* rhoPairCut = new franksPairCut;  // use "frank's" pair cut object
    rhoPairCut->SetPDiff(0.,1000.);             // difference of pairs in momentum
    //    rhoPairCut->SetAngle(0.,180.);           // opening angle
    rhoAnal->SetPairCut(rhoPairCut);         // this is the pair cut for this analysis
    // 4) set the number of events to mix (per event)
    rhoAnal->SetNumEventsToMix(3);        
    // ********************************************************************
    // 5) now set up the correlation functions that this analysis will make
    // ********************************************************************
    // define example Minv correlation function
    MinvCFrho = new MinvCorrFctn("Minv",100,0.5,0.8); 
    MinvCFrho->SetEventCut(rhoEvcut);
    rhoAnal->AddCorrFctn(MinvCFrho);   // adds the just-defined correlation function to the analysis

    // now add as many more correlation functions to the Analysis as you like..
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(rhoAnal);

  // now execute the chain member functions
  
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



