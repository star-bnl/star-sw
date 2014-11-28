
// HERE, WE USE THE NEW StHbtGeantTxtReader to read Geant-text input files
// so no IoMaker
// and no StEventMaker (or StEvent!!)

// this macro looks at the "blended" event file and makes correlation functions
// for proton-proton, (k+)-(k+), (k+)-(k-), <-- for these, look at ALL events
// and (pi+)-(pi+), (pi-)-(pi-), (pi+)-(pi-) <-- for these, just look at a few!!


#define EXAMINE_LOW_MULT_PARTICLES  // this means look at protons and kaons...

class StChain;
StChain *chain=0;    

// keep pointers to Correlation Functions global, so you can have access to them...
class QvecCorrFctn;
#ifdef EXAMINE_LOW_MULT_PARTICLES
QvecCorrFctn* QvecCF_kp_kp;
QvecCorrFctn* QvecCF_kp_km;
QvecCorrFctn* QvecCF_prot_prot;
#else
QvecCorrFctn* QvecCF_pip_pip;
QvecCorrFctn* QvecCF_pip_pim;
#endif

void ExamineTxtFile(Int_t nevents=1,
	       const char *MainFile="/home/scratch/lisa/blended_events.txt")
  //	       const char *MainFile="/star/u2b/lisa/Lanny/code/correl/RUN_AREA/event_hbt_text.out")
  //		  const char *MainFile="/direct/star+u2b/lisa/correlated_pions_from_vdgus1.out")

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
    gSystem->Load("StV0MiniDstMaker");

    cout << "Dynamic loading done" << endl;

    chain = new StChain("StChain"); 
    chain->SetDebug();
   

    // Now we add Makers to the chain...

    //    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    //    ioMaker->SetDebug();
    //    ioMaker->SetIOMode("r");
    //    ioMaker->SetDebug();
    //    ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
    //    ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch


    //     StEventMaker* eventMaker = new StEventMaker("events","title");
    //    cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << endl;



    StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
    cout << "StHbtMaker instantiated"<<endl;



    /* -------------- set up of hbt stuff ----- */
    cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;

    StHbtManager* TheManager = hbtMaker->HbtManager();

    // here, we instantiate the appropriate StHbtEventReader
    // for STAR analyses in root4star, we instantiate StStandardHbtEventReader
    //    StStandardHbtEventReader* Reader = new StStandardHbtEventReader;
    //    Reader->SetTheEventMaker(eventMaker);     // gotta tell the reader where it should read from


    // use the newly written StHbtGeantTxtReader -- reads Geant-text files
    StHbtGstarTxtReader* Reader = new StHbtGstarTxtReader;
    Reader->SetFileName(MainFile);


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

#ifdef EXAMINE_LOW_MULT_PARTICLES
    /*---------------- proton-proton analysis ---------------*/
    // 0) now define an analysis...
    // This one looks for PROTONS
    StHbtAnalysis* anal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* evcut = new mikesEventCut;
    evcut->SetEventMult(0,10000);      
    evcut->SetVertZPos(-35.0,35.0);    
    anal->SetEventCut(evcut);          
    // 2) set the Track (particle) cuts for the analysis
    mikesTrackCut* trkcut = new mikesTrackCut;
    trkcut->SetNSigmaPion(1.0,1000.0);
    trkcut->SetNSigmaKaon(1.0,1000.0);
    trkcut->SetNSigmaProton(-1.5,1.5);
    trkcut->SetNHits(5,50);           
    trkcut->SetPt(0.1,0.8);           
    trkcut->SetRapidity(-1.0,1.0);    
    trkcut->SetDCA(-0.5,0.5);         
    trkcut->SetCharge(1);             
    trkcut->SetMass(0.938);           
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle
    anal->SetSecondParticleCut(trkcut); // NOTE - it is also for the "second" particle -- i.e. identical particle HBT
    // 3) set the Pair cuts for the analysis
    mikesPairCut* paircut = new mikesPairCut; 
    anal->SetPairCut(paircut);         
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(5);        
    // 5) now set up the correlation functions that this analysis will make
    QvecCF_prot_prot = new QvecCorrFctn("proton-proton",50,0.0,0.2);
    anal->AddCorrFctn(QvecCF_prot_prot); // adds the just-defined correlation function to the analysis
    
    // now add as many more correlation functions to the Analysis as you like..
    
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);

    /*---------------- (k+)-(k+) analysis ---------------*/
    // 0) now define an analysis...
    // This one looks for KAONS
    anal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    evcut = new mikesEventCut;
    evcut->SetEventMult(0,10000);      
    evcut->SetVertZPos(-35.0,35.0);    
    anal->SetEventCut(evcut);          
    // 2) set the Track (particle) cuts for the analysis
    trkcut = new mikesTrackCut;
    trkcut->SetNSigmaPion(1.0,1000.0);
    trkcut->SetNSigmaKaon(-1.5,1.5);
    trkcut->SetNSigmaProton(-1000.0,-1.0);
    trkcut->SetNHits(5,50);           
    trkcut->SetPt(0.1,0.8);           
    trkcut->SetRapidity(-1.0,1.0);    
    trkcut->SetDCA(-0.5,0.5);         
    trkcut->SetCharge(1);             
    trkcut->SetMass(0.494);           
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle
    anal->SetSecondParticleCut(trkcut); // NOTE - it is also for the "second" particle -- i.e. identical particle HBT
    // 3) set the Pair cuts for the analysis
    mikesPairCut* paircut = new mikesPairCut; 
    anal->SetPairCut(paircut);         
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(5);        
    // 5) now set up the correlation functions that this analysis will make
    QvecCF_kp_kp = new QvecCorrFctn("Kplus-Kplus",50,0.0,0.2);
    anal->AddCorrFctn(QvecCF_kp_kp); // adds the just-defined correlation function to the analysis
    
    // now add as many more correlation functions to the Analysis as you like..
    
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);

    /*---------------- (k+)-(k-) analysis ---------------*/
    // 0) now define an analysis...
    // This one looks for KAONS
    anal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    evcut = new mikesEventCut;
    evcut->SetEventMult(0,10000);      
    evcut->SetVertZPos(-35.0,35.0);    
    anal->SetEventCut(evcut);          
    // 2) set the Track (particle) cuts for the analysis
    trkcut = new mikesTrackCut;
    trkcut->SetNSigmaPion(1.0,1000.0);
    trkcut->SetNSigmaKaon(-1.5,1.5);
    trkcut->SetNSigmaProton(-1000.0,-1.0);
    trkcut->SetNHits(5,50);           
    trkcut->SetPt(0.1,0.8);           
    trkcut->SetRapidity(-1.0,1.0);    
    trkcut->SetDCA(-0.5,0.5);         
    trkcut->SetCharge(1);             
    trkcut->SetMass(0.494);           
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle

    trkcut = new mikesTrackCut;
    trkcut->SetNSigmaPion(1.0,1000.0);
    trkcut->SetNSigmaKaon(-1.5,1.5);
    trkcut->SetNSigmaProton(-1000.0,-1.0);
    trkcut->SetNHits(5,50);           
    trkcut->SetPt(0.1,0.8);           
    trkcut->SetRapidity(-1.0,1.0);    
    trkcut->SetDCA(-0.5,0.5);         
    trkcut->SetCharge(-1);             
    trkcut->SetMass(0.494);           
    anal->SetSecondParticleCut(trkcut); // NOTE DIFFERENT 2ND PARTICLE CUT -- NONIDENTICAL HBT
    // 3) set the Pair cuts for the analysis
    mikesPairCut* paircut = new mikesPairCut; 
    anal->SetPairCut(paircut);         
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(5);        
    // 5) now set up the correlation functions that this analysis will make
    QvecCF_kp_km = new QvecCorrFctn("Kplus-Kminus",50,0.0,0.2);
    anal->AddCorrFctn(QvecCF_kp_km); // adds the just-defined correlation function to the analysis
    
    // now add as many more correlation functions to the Analysis as you like..
    
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);


#else             // look at pions instead of protons and kaons

    /*---------------- piplus-piplus analysis ---------------*/
    // 0) now define an analysis...
    // This one looks for PIONS
    StHbtAnalysis* anal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* evcut = new mikesEventCut;
    evcut->SetEventMult(0,10000);      
    evcut->SetVertZPos(-35.0,35.0);    
    anal->SetEventCut(evcut);          
    // 2) set the Track (particle) cuts for the analysis
    mikesTrackCut* trkcut = new mikesTrackCut;
    trkcut->SetNSigmaPion(-1.5,1.5);
    trkcut->SetNSigmaKaon(-1000.0,-1.0);
    trkcut->SetNSigmaProton(-1000.0,-1.0);
    trkcut->SetNHits(5,50);           
    trkcut->SetPt(0.1,0.8);           
    trkcut->SetRapidity(-1.0,1.0);    
    trkcut->SetDCA(-0.5,0.5);         
    trkcut->SetCharge(1);             
    trkcut->SetMass(0.138);           
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle
    anal->SetSecondParticleCut(trkcut); // NOTE - it is also for the "second" particle -- i.e. identical particle HBT
    // 3) set the Pair cuts for the analysis
    mikesPairCut* paircut = new mikesPairCut; 
    anal->SetPairCut(paircut);         
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(5);        
    // 5) now set up the correlation functions that this analysis will make
    QvecCF_pip_pip = new QvecCorrFctn("piplus-piplus",50,0.0,0.2);
    anal->AddCorrFctn(QvecCF_pip_pip); // adds the just-defined correlation function to the analysis
    
    // now add as many more correlation functions to the Analysis as you like..
    
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);

    /*---------------- piplus-piminus analysis ---------------*/
    // 0) now define an analysis...
    // This one looks for PION
    anal = new StHbtAnalysis;
    // 1) set the Event cuts for the analysis
    evcut = new mikesEventCut;
    evcut->SetEventMult(0,10000);      
    evcut->SetVertZPos(-35.0,35.0);    
    anal->SetEventCut(evcut);          
    // 2) set the Track (particle) cuts for the analysis
    trkcut = new mikesTrackCut;
    trkcut->SetNSigmaPion(-1.5,1.5);
    trkcut->SetNSigmaKaon(-1000.0,-1.0);
    trkcut->SetNSigmaProton(-1000.0,-1.0);
    trkcut->SetNHits(5,50);           
    trkcut->SetPt(0.1,0.8);           
    trkcut->SetRapidity(-1.0,1.0);    
    trkcut->SetDCA(-0.5,0.5);         
    trkcut->SetCharge(1);             
    trkcut->SetMass(0.138);           
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle


    trkcut = new mikesTrackCut;
    trkcut->SetNSigmaPion(-1.5,1.5);
    trkcut->SetNSigmaKaon(-1000.0,-1.0);
    trkcut->SetNSigmaProton(-1000.0,-1.0);
    trkcut->SetNHits(5,50);           
    trkcut->SetPt(0.1,0.8);           
    trkcut->SetRapidity(-1.0,1.0);    
    trkcut->SetDCA(-0.5,0.5);         
    trkcut->SetCharge(-1);             
    trkcut->SetMass(0.138);           

    anal->SetSecondParticleCut(trkcut); // NOTE DIFFERNT CUT for 2nd particle -- nonidentical hbt
    // 3) set the Pair cuts for the analysis
    mikesPairCut* paircut = new mikesPairCut; 
    anal->SetPairCut(paircut);         
    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(5);        
    // 5) now set up the correlation functions that this analysis will make
    QvecCF_pip_pim = new QvecCorrFctn("piplus-piminus",50,0.0,0.2);
    anal->AddCorrFctn(QvecCF_pip_pim); // adds the just-defined correlation function to the analysis
    
    // now add as many more correlation functions to the Analysis as you like..
    
    // 6) add the Analysis to the AnalysisCollection
    TheManager->AddAnalysis(anal);

#endif            // EXAMINE_LOW_MULT_PARTICLES

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
