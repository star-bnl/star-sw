
class StChain;
StChain *chain=0;    

// keep pointers to Correlation Functions global, so you can have access to them...
class QinvCorrFctn;
QinvCorrFctn* QinvCF;
class QvecCorrFctn;
QvecCorrFctn* QvecCF;
class MinvCorrFctn;
MinvCorrFctn* MinvCF;

void StHbtExample_McEvent(Int_t nevents=1,
		  const char *MainFile="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfs_4/set0364_01_35evts.geant.root")
{

    // Dynamically link needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");  // new addition 22jul99
    gSystem->Load("StAnalysisUtilities");  // needed by V0dstMaker
    gSystem->Load("StMagF");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StHbtMaker");
    gSystem->Load("StV0MiniDstMaker");

    cout << "Dynamic loading done" << endl;

    chain = new StChain("StChain"); 
    chain->SetDebug();
   

    // Now we add Makers to the chain...

    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug();

    ioMaker->SetIOMode("r");
    ioMaker->SetDebug();
    ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
    //    ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch
    ioMaker->SetBranch("geantBranch",0,"r"); //activate EventBranch


    //    StEventMaker* eventMaker = new StEventMaker("events","title");
    //    cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << endl;
    StMcEventMaker* mcEventMaker = new StMcEventMaker("mcEvents","title");
    cout << "Just instantiated StMcEventMaker... lets go StHbtMaker!" << endl;

    // UNCOMMENT THIS NEXT PART OUT IF YOU WANT V0's
    //StV0MiniDstMaker* v0dst = new StV0MiniDstMaker("v0dst"); 
    //cout << "Just instantiated StV0MiniDstMaker... lets go StHbt!" << endl;
    //v0dst.SetV0VertexType(); //Set v0MiniDstMaker to find v0s not Xis
    //v0dst.SetOutputFile("muv0dst.root"); // Set V0MiniDStMaker output file



    StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
    cout << "StHbtMaker instantiated"<<endl;



    /* -------------- set up of hbt stuff ----- */
    cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;

    StHbtManager* TheManager = hbtMaker->HbtManager();

    // here, we instantiate the appropriate StHbtEventReader
    // for STAR analyses in root4star, we instantiate StStandardHbtEventReader
    //StStandardHbtEventReader* Reader = new StStandardHbtEventReader;
    //Reader->SetTheEventMaker(eventMaker);     // gotta tell the reader where it should read from
    StHbtMcEventReader* Reader = new StHbtMcEventReader;
    Reader->SetTheMcEventMaker(mcEventMaker);     // gotta tell the reader where it should read from

    // UNCOMMENT THIS NEXT LINE OUT IF YOU WANT V0's
    //    Reader->SetTheV0Maker(v0dst); //Gotta tell the reader where to read the v0 stuff from

    // here would be the palce to plug in any "front-loaded" Event or Particle Cuts...
    TheManager->SetEventReader(Reader);

    cout << "READER SET UP.... " << endl;

    // Hey kids! Let's make a microDST!
    // in StHbt we do this by instantiating and plugging in a StHbtEventReader as a writer!
    // the particular StHbtEventReader that we will use will write (and read) ASCII files
    //
    //    StHbtAsciiReader* Writer = new StHbtAsciiReader;
    //    Writer->SetFileName("FirstMicroDst.asc");
    //    TheManager->SetEventWriter(Writer);
    //    cout << "WRITER SET UP.... " << endl;

    // 0) now define an analysis...
    StHbtAnalysis* anal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
    evcut->SetEventMult(0,100000);      // selected multiplicity range
    evcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
    anal->SetEventCut(evcut);          // this is the event cut object for this analsys
    // 2) set the Track (particle) cuts for the analysis
    mikesTrackCut* trkcut = new mikesTrackCut;  // use "mike's" particle cut object
    trkcut->SetNSigmaPion(-0.5,0.5);   // number of Sigma in TPC dEdx away from nominal pion dEdx
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
    phiEvcut->SetEventMult(0,100000);      // selected multiplicity range
    phiEvcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
    phiAnal->SetEventCut(phiEvcut);          // this is the event cut object for this analsys
    // 2) set the Track (particle) cuts for the analysis
    mikesTrackCut* kaonTrkcut = new mikesTrackCut;  // use "mike's" particle cut object
    kaonTrkcut->SetNSigmaPion(-1000,1000.0);   // number of Sigma in TPC dEdx away from nominal pion dEdx
    kaonTrkcut->SetNSigmaKaon(-3.0,3.0);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    kaonTrkcut->SetNSigmaProton(-1000.,1000.0); // number of Sigma in TPC dEdx away from nominal proton dEdx
    kaonTrkcut->SetNHits(0,50);            // range on number of TPC hits on the track
    kaonTrkcut->SetPt(0.1,2.0);            // range in Pt
    kaonTrkcut->SetRapidity(-2.0,2.0);     // range in rapidity
    kaonTrkcut->SetDCA(0.0,0.5);            // range in Distance of Closest Approach to primary vertex
    kaonTrkcut->SetCharge(+1);              // want positive kaons
    kaonTrkcut->SetMass(0.494);             // kaon mass
    phiAnal->SetFirstParticleCut(kaonTrkcut);  // this is the track cut for the "first" particle
    mikesTrackCut* antikaonTrkcut = new mikesTrackCut;  // use "mike's" particle cut object
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
  
  if (chain->Init()){ // This should call the Init() method in ALL makers
    cout << "Initialization failed \n";
    goto TheEnd;
  }
  chain->PrintInfo();


  // Event loop
  int istat=0,iev=1;
 EventLoop: if (iev <= nevents && !istat) {
   cout << "StHbtExample -- Working on eventNumber " << iev << " of " << nevents << endl;
   chain->Clear();
   istat = chain->Make(iev);
   if (istat) {cout << "Last event processed. Status = " << istat << endl;}
   iev++; goto EventLoop;
 }

//  good old Cint can't even handle a for-loop
//   for (Int_t iev=1;iev<=nevents; iev++) {
//     chain->Clear();
//     int iret = chain->Make(iev); // This should call the Make() method in ALL makers
//     if (iret) {
//       cout << "StHbtExample.C -- chain returned nonzero value " << iret 
// 	   << " on event " << iev << endl;
//       break;
//     } 
//   } // Event Loop



  cout << "StHbtExample -- Done with event loop" << endl;

  chain->Finish(); // This should call the Finish() method in ALL makers
 TheEnd:
}
