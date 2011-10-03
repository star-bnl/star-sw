
class StChain;
StChain *chain=0;    // NOTE - chain needs to be declared global so for StHbtEventReader

// keep pointers to Correlation Functions global, so you can have access to them...
class QinvCorrFctn;
QinvCorrFctn* QinvCF;
class QvecCorrFctn;
QvecCorrFctn* QvecCF;
class MinvCorrFctn;
MinvCorrFctn* MinvCF;

void StHbtExample(Int_t nevents=1,
		  const char *MainFile="/disk00000/star/test/new/tfs_Solaris/year_2a/psc0210_01_40evts.dst.root")
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StMagF");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StEventReaderMaker");
    gSystem->Load("StHbtMaker");

    cout << "Dynamic loading done" << endl;

    chain = new StChain("StChain"); 
    chain->SetDebug();
   

    // Now we add Makers to the chain...

    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug();

    ioMaker->SetIOMode("r");
    ioMaker->SetDebug();
    ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
    ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch


    StEventReaderMaker* eventReader = new StEventReaderMaker("events","title"); //our way

    cout << "Just instantiated StEventReaderMaker... lets go StHbtMaker!" << endl;

    StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
    cout << "StHbtMaker instantiated"<<endl;



    /* -------------- set up of hbt stuff ----- */
    cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;

    StHbtManager* TheManager = hbtMaker->HbtManager();

    // here, we instantiate the appropriate StHbtEventReader
    // for STAR analyses in root4star, we instantiate StStandardHbtEventReader
    StStandardHbtEventReader* Reader = new StStandardHbtEventReader;
    Reader->SetTheChain(chain);     // gotta tell the reader where it should read from
    TheManager->SetEventReader(Reader);

    cout << "READER SET UP.... " << endl;

    // 0) now define an analysis...
    StHbtAnalysis* anal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
    evcut->SetEventMult(0,10000);      // selected multiplicity range
    evcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
    anal->SetEventCut(evcut);          // this is the event cut object for this analsys
    // 2) set the Track (particle) cuts for the analysis
    mikesParticleCut* trkcut = new mikesParticleCut;  // use "mike's" particle cut object
    trkcut->SetNSigmaPion(-1.5,1.5);   // number of Sigma in TPC dEdx away from nominal pion dEdx
    trkcut->SetNSigmaKaon(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    trkcut->SetNSigmaProton(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal proton dEdx
    trkcut->SetNHits(5,50);            // range on number of TPC hits on the track
    trkcut->SetPt(0.1,1.0);            // range in Pt
    trkcut->SetRapidity(-1.0,1.0);     // range in rapidity
    trkcut->SetDCA(0.0,0.5);           // range in Distance of Closest Approach to primary vertex
    trkcut->SetCharge(-1);             // want negative pions
    trkcut->SetMass(0.139);            // pion mass
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle
    anal->SetSecondParticleCut(trkcut); // NOTE - it is also for the "second" particle -- i.e. identical particle HBT
    // 3) set the Pair cuts for the analysis
    mikesPairCut* paircut = new mikesPairCut;  // use "mike's" pair cut object
    anal->SetPairCut(paircut);         // this is the pair cut for this analysis
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(5);        
    // 5) now set up the correlation functions that this analysis will make
    // this particular analysis will have two: the first is a Q-invariant correlation function
    QinvCF = new QinvCorrFctn("mikesQinvCF",50,0.0,0.2);  // defines a Qinv correlation function
    anal->AddCorrFctn(QinvCF); // adds the just-defined correlation function to the analysis
    // for this analysis, we will also (simultaneously) build a Q-vector correlation function
    QvecCF = new QvecCorrFctn("randysQvecCF",50,0.0,0.2);
    anal->AddCorrFctn(QvecCF); // adds the just-defined correlation function to the analysis
    
    // now add as many more correlation functions to the Analysis as you like..
    
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);

    // now, we define another analysis that runs simultaneously with the previous one.
    // this one looks at K+K- correlations (so NONidentical particles) in invariant mass

    /* ****************************************** */ 
    /* * franks phi analysis - by Frank Laue, OSU */
    /* ****************************************** */ 
    // 0) now define an analysis...
    StHbtAnalysis* phiAnal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* phiEvcut = new mikesEventCut;  // use "mike's" event cut object
    phiEvcut->SetEventMult(0,10000);      // selected multiplicity range
    phiEvcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
    phiAnal->SetEventCut(phiEvcut);          // this is the event cut object for this analsys
    // 2) set the Track (particle) cuts for the analysis
    mikesParticleCut* kaonTrkcut = new mikesParticleCut;  // use "mike's" particle cut object
    kaonTrkcut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
    kaonTrkcut->SetNSigmaKaon(-3.0,3.0);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    kaonTrkcut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
    kaonTrkcut->SetNHits(0,50);            // range on number of TPC hits on the track
    kaonTrkcut->SetPt(0.1,2.0);            // range in Pt
    kaonTrkcut->SetRapidity(-2.0,2.0);     // range in rapidity
    kaonTrkcut->SetDCA(0.0,0.5);            // range in Distance of Closest Approach to primary vertex
    kaonTrkcut->SetCharge(+1);              // want positive kaons
    kaonTrkcut->SetMass(0.494);             // kaon mass
    phiAnal->SetFirstParticleCut(kaonTrkcut);  // this is the track cut for the "first" particle
    mikesParticleCut* antikaonTrkcut = new mikesParticleCut;  // use "mike's" particle cut object
    antikaonTrkcut->SetNSigmaPion(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
    antikaonTrkcut->SetNSigmaKaon(-3.0,3.0);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    antikaonTrkcut->SetNSigmaProton(-1000.0,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
    antikaonTrkcut->SetNHits(0,50);            // range on number of TPC hits on the track
    antikaonTrkcut->SetPt(0.1,2.0);            // range in Pt
    antikaonTrkcut->SetRapidity(-2.0,2.0);     // range in rapidity
    antikaonTrkcut->SetDCA(0.0,0.5);            // range in Distance of Closest Approach to primary vertex
    antikaonTrkcut->SetCharge(-1);              // want negative kaons
    antikaonTrkcut->SetMass(0.494);             // kaon mass
    phiAnal->SetSecondParticleCut(antikaonTrkcut); // this is the track cut for the "second" particle
    // 3) set the Pair cuts for the analysis
    mikesPairCut* phiPaircut = new mikesPairCut;  // use "mike's" pair cut object
    phiAnal->SetPairCut(phiPaircut);         // this is the pair cut for this analysis
    // 4) set the number of events to mix (per event)
    phiAnal->SetNumEventsToMix(5);        
    // 5) now set up the correlation functions that this analysis will make
    MinvCF = new MinvCorrFctn("franksMinvCF",100,0.98,1.18); // defines a Minv correlation function
    phiAnal->AddCorrFctn(MinvCF);   // adds the just-defined correlation function to the analysis
    // now add as many more correlation functions to the Analysis as you like..
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(phiAnal);





    /* ------------------ end of setting up hbt stuff ------------------ */


  // now execute the chain member functions
  
  chain->Init(); // This should call the Init() method in ALL makers
  chain->PrintInfo();

  for (Int_t iev=0;iev<nevents; iev++) {
    chain->Clear();
    int iret = chain->Make(iev); // This should call the Make() method in ALL makers
    if (iret) break;
    
    
    
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers
}
