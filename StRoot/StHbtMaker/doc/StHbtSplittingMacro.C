
// this macro is not really for HBT, it is a tool requested by the
// reco group to estimate the track splittin in the TPC from the data
// it makes an upper and lower limit on its estimate of the splitting
// malisa 15aug2000
// an example of an analysis done with this is at:
//http://sol.star.bnl.gov/star/starlib/doc/www/protected/hbt/lisa/SplittingEstimate/

class StChain;
StChain *chain=0;    

// keep pointers to Correlation Functions global, so you can have access to them...
class QinvCorrFctn;
QinvCorrFctn* QinvCF;
class QvecCorrFctn;
QvecCorrFctn* QvecCF;
class MinvCorrFctn;
MinvCorrFctn* MinvCF;

void StHbtSplittingMacro(Int_t nevents=1,
		  const char *MainFile="/star/rcf/pwg/hbt/July2000/HalfFieldData_new2.microDst")
  // this is an OLD xdf file   const char *MainFile="
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
    //    gSystem->Load("StV0MiniDstMaker");

    cout << "Dynamic loading done" << endl;

    chain = new StChain("StChain"); 
    chain->SetDebug();
   

    // Now we add Makers to the chain...

    //    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    //    ioMaker->SetDebug();

//     ioMaker->SetIOMode("r");
//     ioMaker->SetDebug();
//     ioMaker->SetBranch("*",0,"0");                 //deactivate all branches
//     ioMaker->SetBranch("dstBranch",0,"r"); //activate EventBranch


    //    StEventMaker* eventMaker = new StEventMaker("events","title");
    //    cout << "Just instantiated StEventMaker... lets go StHbtMaker!" << endl;

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
    //    StStandardHbtEventReader* Reader = new StStandardHbtEventReader;
    //    Reader->SetTheEventMaker(eventMaker);     // gotta tell the reader where it should read from

    // UNCOMMENT THIS NEXT LINE OUT IF YOU WANT V0's
    //    Reader->SetTheV0Maker(v0dst); //Gotta tell the reader where to read the v0 stuff from

    // here would be the palce to plug in any "front-loaded" Event or Particle Cuts...

    StHbtBinaryReader* Reader = new StHbtBinaryReader;
    Reader->SetFileName(MainFile);



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
    StHbtSplitEvalAnalysis* anal = new StHbtSplitEvalAnalysis;
    // 1) set the Event cuts for the analysis
    mikesEventCut* evcut = new mikesEventCut;  // use "mike's" event cut object
    evcut->SetEventMult(0,10000);      // selected multiplicity range
    evcut->SetVertZPos(-70.0,70.0);    // selected range of vertex z-position
    anal->SetEventCut(evcut);          // this is the event cut object for this analsys
    // 2) set the Track (particle) cuts for the analysis
    mikesTrackCut* trkcut = new mikesTrackCut;  // use "mike's" particle cut object
    trkcut->SetNSigmaPion(-1.5,1.5);   // number of Sigma in TPC dEdx away from nominal pion dEdx
    trkcut->SetNSigmaKaon(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal kaon dEdx
    trkcut->SetNSigmaProton(-1000.0,1000.0);   // number of Sigma in TPC dEdx away from nominal proton dEdx
    trkcut->SetNHits(5,50);            // range on number of TPC hits on the track
    trkcut->SetPt(0.1,2.0);            // range in Pt
    trkcut->SetRapidity(-1.0,1.0);     // range in rapidity
    trkcut->SetDCA(0.0,0.5);           // range in Distance of Closest Approach to primary vertex
    trkcut->SetCharge(-1);             // want negative pions
    trkcut->SetMass(0.139);            // pion mass
    anal->SetFirstParticleCut(trkcut); // this is the track cut for the "first" particle
    anal->SetSecondParticleCut(trkcut); // NOTE - it is also for the "second" particle -- i.e. identical particle HBT
    // 3) set the Pair cuts for the analysis
    qualityPairCut* qpc = new qualityPairCut;
    qpc->SetQualityCut(-0.5,0.7);

    anal->SetPairCut(qpc);         // this is the pair cut for this analysis

    // 4) set the number of events to mix (per event)
    anal->SetNumEventsToMix(1);        
    // 5) now set up the correlation functions that this analysis will make
    // this particular analysis will have two: the first is a Q-invariant correlation function
//     QinvCF = new QinvCorrFctn("mikesQinvCF",50,0.0,0.2);  // defines a Qinv correlation function
//     anal->AddCorrFctn(QinvCF); // adds the just-defined correlation function to the analysis
//     // for this analysis, we will also (simultaneously) build a Q-vector correlation function
//     QvecCF = new QvecCorrFctn("randysQvecCF",50,0.0,0.2);
//     anal->AddCorrFctn(QvecCF); // adds the just-defined correlation function to the analysis
    
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


  realAll->Draw();
  c1->Divide(2,3);
  c1->cd(1);
  realAll->Draw();
  c1->cd(2);
  realSplit->Draw();
  c1->cd(3);
  mixedAll->Draw();
  c1->cd(4);
  mixedSplit->Draw();
  
  c1->cd(5);
  
  TH1D realRat = *realSplit;
  realRat.Divide(realSplit,realAll);
  realRat.Draw();

  c1->cd(6);
  TH1D Diff = *realSplit;
  Diff.Add(realSplit,mixedSplit,1.0,-1.0);
  TH1D diffRat = *realSplit;
  TH1D* PDiff = &Diff;
  diffRat.Divide(PDiff,realAll);
  diffRat.Draw();

}
