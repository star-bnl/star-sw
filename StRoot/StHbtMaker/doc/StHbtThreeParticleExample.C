
class StChain;
StChain *chain=0;    

// keep pointers to Correlation Functions global, so you can have access to them...
class Q3invCorrFctn;
Q3invCorrFctn* Q3invCF;

void StHbtThreeParticleExample(Int_t nevents=1,
		  const char *MainFile="/star/rcf/pwg/hbt/July2000/HalfFieldData_new2.microDst")
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
    gSystem->Load("StHbtMaker");

    cout << "Dynamic loading done" << endl;

    chain = new StChain("StChain"); 
    chain->SetDebug();
   

    // Now we add Makers to the chain...

    StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
    cout << "StHbtMaker instantiated"<<endl;



    /* -------------- set up of hbt stuff ----- */
    cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;

    StHbtManager* TheManager = hbtMaker->HbtManager();

    // use the binary reader
    StHbtBinaryReader* Reader = new StHbtBinaryReader;
    Reader->SetFileName(MainFile);

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
    StHbtThreeParticleAnalysis* anal = new StHbtThreeParticleAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
    evcut->SetEventMult(0,10000);      // selected multiplicity range
    evcut->SetVertZPos(-35.0,35.0);    // selected range of vertex z-position
    anal->SetEventCut(evcut);          // this is the event cut object for this analsys
    // 2) set the Track (particle) cuts for the analysis
    mikesTrackCut* trkcut = new mikesTrackCut;  // use "mike's" particle cut object
    trkcut->SetNSigmaPion(-1.5,1.5);   // number of Sigma in TPC dEdx away from nominal pion dEdx
    trkcut->SetNSigmaKaon(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    trkcut->SetNSigmaProton(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal proton dEdx
    trkcut->SetNHits(5,50);            // range on number of TPC hits on the track
    trkcut->SetPt(0.1,1.0);            // range in Pt
    trkcut->SetRapidity(-10.0,10.0);     // range in rapidity
    trkcut->SetDCA(0.0,0.5);           // range in Distance of Closest Approach to primary vertex
    trkcut->SetCharge(-1);             // want negative pions
    trkcut->SetMass(0.139);            // pion mass
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle
    anal->SetSecondParticleCut(trkcut); // NOTE - it is also for the "second" particle -- i.e. identical particle HBT
    anal->SetThirdParticleCut(trkcut); // NOTE - it is also for the "third" particle -- i.e. identical particle HBT
    // 3) set the Triplet cuts for the analysis
    GenericTripletCut* tripletcut = new GenericTripletCut;  // use gereric triplet cut object
    anal->SetTripletCut(tripletcut);         // this is the triplet cut for this analysis
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(2);        
    // 5) now set up the correlation functions that this analysis will make
    // this particular analysis will have one: a Q-invariant correlation function
    Q3invCF = new Q3invCorrFctn("Q3invCF",100,0.0,1.0);  // defines a Q3inv correlation function
    anal->AddCorrFctn(Q3invCF); // adds the just-defined correlation function to the analysis
    
    // now add as many more correlation functions to the Analysis as you like..
    
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);


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


  cout << "StHbtExample -- Done with event loop" << endl;

  chain->Finish(); // This should call the Finish() method in ALL makers
 TheEnd:
}
