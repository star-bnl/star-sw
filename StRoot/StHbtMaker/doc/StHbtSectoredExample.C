
// Example of SectoredAnalysis using MDC3 data

class StChain;
StChain *chain=0;    

// keep pointers to Correlation Functions global, so you can have access to them...

class QinvCorrFctn;
QinvCorrFctn* QinvCF;

void StHbtSectoredExample(Int_t nevents=100,  const char *MainFile="/direct/star+data01/pwg/hbt/MDC3/MicroDst/Lanny/Peripheral/rcf0106_80_120evts.dst.root.pion.microDst")
  	      
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

    // here, we instantiate the appropriate StHbtEventReader

    // use the binary reader
    StHbtBinaryReader* Reader = new StHbtBinaryReader;
    Reader->SetFileName(MainFile);

    // here would be the palce to plug in any "front-loaded" Event or Particle Cuts...
    TheManager->SetEventReader(Reader);

    cout << "READER SET UP.... " << endl;

    // We will inistantiate an StHbtSectoredAnalysis.  The cuts are the same as in StHbtExample.C
    // with the exception of some extra initialization for the sectoring.

    //  TWO PARTICLE CORRELATION FUNCTION

    // 0) now define a sectored two particle analysis.
    StHbtSectoredAnalysis* anal = new StHbtSectoredAnalysis;
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
    // this particular analysis will have one: a Q-invariant correlation function
    QinvCF = new QinvCorrFctn("mikesQinvCF - SectoredAnalysis",50,0.0,0.2);  // defines a Qinv correlation function
    anal->AddCorrFctn(QinvCF); // adds the just-defined correlation function to the analysis
    
    // Now we must set up the sectoring.  Momentum space will be sectored into identical cubes, 
    // of length DeltaP.  The amount of momentum space to sector is determined by the min and max
    // values of PX, PY, and PZ.  Any particles that fall outside these boundaries will be 
    // collected in one large "overflow" bin.  The effect is that you should see no change in the 
    // calculation of the correlation function regardless of your choice of parameters, except
    // that you will notice the calculation at large Q will not be correct, since the sectoring 
    // eliminates those pairs.
    //
    // The analysis will not allow incorrect entries (such as DeltaP=0), and there are default values
    // which are given in StHbtSectoredAnalysis.cxx

    // 6) Set parameters for sectoring
    //    Here we will use sectors of 150 MeV cubed, going from -1.05 Gev to 1.05 Gev; this gives 2744 sectors 
    anal->SetDeltaP(0.15);
    anal->SetPXmax(1.05);
    anal->SetPXmin(-1.05);
    anal->SetPYmax(1.05);
    anal->SetPYmin(-1.05);
    anal->SetPZmax(1.05);
    anal->SetPZmin(-1.05);
    
    // 7) add the Analysis to the AnalysisCollection
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
