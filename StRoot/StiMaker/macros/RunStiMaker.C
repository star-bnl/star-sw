// RunStiMaker.C
// M.L. Miller
//  5/00

class StChain;
StChain *chain=0;

void RunStiMaker(Int_t nevents=5,
		 
		 bool simulated=true, /*!sim or data?*/
		 
		 //bool draw=true, /*! use gui, click your way around */
		 bool draw=false, /*! console version, run through nevents */
		 
		 bool doFit=true, /*! true->fit track only */
		 //bool doFit=false, /*! false->find track only */

                 //This file points to 10 events of 10 neg muons w/ pt=.9 
                 //const char *MainFile="/star/data17/ITTF/data/simple_geant/DEV_10_04_01/*.event.root")

                 //This file points to 30 events of 10 neg muons w/ pt=.9
                 //const char* MainFile="/star/data17/ITTF/data/simple_geant/DEV_10_8_01/*.event.root")

                 //This file points to 110 events from mevsim (homebrew had. cocktail)
                 const char* MainFile="/star/data17/ITTF/data/mevsim/10_9_01/*.event.root")
    
{    
    // Dynamically link needed shared libs
    
    cout <<"Loading St_base"<<endl;
    gSystem->Load("St_base");
    
    cout <<"Loading StChain"<<endl;
    gSystem->Load("StChain");
    
    cout <<"Loading St_Tables"<<endl;
    gSystem->Load("St_Tables");
    
    cout <<"Loading StUtilities"<<endl;
    gSystem->Load("StUtilities");
    
    cout <<"Loading StIOMaker"<<endl;
    gSystem->Load("StIOMaker");
    
    cout <<"Loading StarClassLibrary"<<endl;
    gSystem->Load("StarClassLibrary");
    
    cout <<"Loading DataBase"<<endl;
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("StTpcDb");
    
    cout <<"Loading StEvent"<<endl;
    gSystem->Load("StEvent");

    cout <<"Loading StEventMaker"<<endl;
    gSystem->Load("StEventMaker");

    cout <<"Loading StEmcUtil"<<endl;
    gSystem->Load("StEmcUtil");
    
    cout <<"Loading StMcEvent"<<endl;
    gSystem->Load("StMcEvent");

    cout <<"Loading StMcEventMaker"<<endl;
    gSystem->Load("StMcEventMaker");

    cout <<"Loading AssociationMaker"<<endl;
    gSystem->Load("StAssociationMaker");
    
    cout <<"Loading Sti"<<endl;
    gSystem->Load("Sti");
    //gSystem->Load(".i386_redhat61/LIB/Sti.so"); //For optimized

    cout <<"Loading StiGui"<<endl;
    gSystem->Load("StiGui");
    //gSystem->Load(".i386_redhat61/LIB/StiGui"); //For optimized

    cout <<"Loading StiEvaluator"<<endl;
    gSystem->Load("StiEvaluator");
    
    cout <<"Loading StiMaker"<<endl;
    gSystem->Load("StiMaker");
    //gSystem->Load(".i386_redhat61/LIB/StiMaker"); //For optimized
    
    // create a new instance of the chain
    
    chain = new StChain("StChain"); 
    chain->SetDebug();
    
    // add makers to the chain

    StIOMaker* ioMaker = new StIOMaker("IO","r",MainFile,"bfcTree");
    ioMaker->SetDebug();
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");            //deactivate all branches
    ioMaker->SetBranch("geantBranch",0,"r");  //activate geant Branch
    ioMaker->SetBranch("dstBranch",0,"r");    //activate Event Branch
    ioMaker->SetBranch("runcoBranch",0,"r");  //activate runco Branch

    //Calibration Maker (StarDB,not a real Database!)
    const char* calibDB = "MySQL:StarDb";
    const char* paramsDB = "$STAR/StarDb";
    St_db_Maker* calibMk = new St_db_Maker("StarDb",calibDB,paramsDB);
    //calibMk->SetDateTime("year_2b");
    calibMk->SetDateTime("year_1h");
    calibMk->SetDebug();

    //Read Tpc Database access
    StTpcDbMaker *tpcDbMk = new StTpcDbMaker("tpcDb");

    //StEventMaker
    StEventMaker*       eventReader   = new StEventMaker("events","title");
    eventReader->doPrintEventInfo = 0;

    //StMcEventMaker
    StMcEventMaker* mcEventReader = 0;
    //Association
    StAssociationMaker* assocMaker = 0;

    if (simulated) {
	mcEventReader = new StMcEventMaker();
	assocMaker = new StAssociationMaker();
    }
    
    //StiMaker
    StiMaker* anaMk = StiMaker::instance();

    anaMk->setDoFit(doFit);
    //enum SeedFinderType {kUndefined=0, kComposite=1, kEvaluable=2};
    anaMk->setSeedFinderType(StiMaker::kEvaluable);
    //anaMk->setSeedFinderType(StiMaker::kComposite);

    anaMk->setSimulation(simulated);
    anaMk->setGui(draw);
    
    if (simulated) {
	anaMk->setMcEventMaker(mcEventReader);
	anaMk->setAssociationMaker(assocMaker);
    }

    // now execute the chain member functions    
    chain->PrintInfo();
    
    //Make Control-Bar
    StiControlBar* sti=0;
    if (draw==true) {
	sti = new StiControlBar();
	sti->setStChain(chain);
    }
    
    cout <<"Calling Init() Methods "<<endl;
    chain->Init();
    
    cout <<"Starting Event Loop"<<endl;

    int istat=0,iev=1;
    
 EventLoop: if (iev<=nevents && !istat) {
     chain->Clear();
     cout << "---------------------- Processing Event : " << iev << endl;
     istat = chain->Make(iev);
     if (istat) {
	 cout << "Last Event Processed. Status = " << istat << endl;
     }
     iev++; goto EventLoop;
 }
    
    //Now we can do diagnostics
    /*! if (draw==false) {
      cout <<"\n\n***** Processed "<<iev-1<<" events with ";
      cout <<TestTree->GetEntries()<<" tracks ******\n"<<endl;
      
      //This needs to be done by hand (ugly!):
      TH1D* gids = new TH1D("gids","Geant Id",101, -.5, 100.5);
      TestTree->Draw("mcTrackId>>gids");
      cout <<"\tGeant Id:\tNumber of Tracks"<<endl;
      cout <<"\t---------\t----------------"<<endl;
      for (int i=0; i<gids->GetNbinsX(); ++i) {
      if (gids->GetBinContent(i)>0.) {
      cout <<"\t"<<gids->GetBinCenter(i)<<"\t\t"<<gids->GetBinContent(i)<<endl;
      }
      }
      cout <<endl;
      
      cout <<"\t\tVariable\tMean\t\tRMS"<<endl;
      cout <<"\t\t--------\t----\t\t---\n"<<endl;
      
      cout <<"ITTF Tracks"<<endl;
      
      canvas1 = new TCanvas("canvas1","ITTF Track Benchmarks",100,50,800,700);
      canvas1->Divide(2,2);
      
      canvas1->cd(1);
      TestTree->Draw("stiTrackPt");
      htemp->SetXTitle("Pt (GeV)");
      cout <<"\t\tPt\t\t"<<htemp->GetMean()<<"\t\t"<<htemp->GetRMS()<<endl;
      
      canvas1->cd(2);
      //TestTree->Draw("stiTrackEta");
      //htemp->SetXTitle("Eta");
      
      canvas1->cd(3);
      TestTree->Draw("stiTrackChi2");
      htemp->SetXTitle("Chi2");
      cout <<"\t\tChi2\t\t"<<htemp->GetMean()<<"\t\t"<<htemp->GetRMS()<<endl;
      
      cout <<"Global Tracks"<<endl;
      
      canvas2 = new TCanvas("canvas2","Global Track Benchmarks",150,50,850,700);
      canvas2->Divide(2,2);
      
      canvas2->cd(1);
      TestTree->Draw("globalTrackPt");
      htemp->SetXTitle("Pt (GeV)");
      cout <<"\t\tPt\t\t"<<htemp->GetMean()<<"\t\t"<<htemp->GetRMS()<<endl;
      
      canvas2->cd(2);
      TestTree->Draw("globalTrackEta");
      htemp->SetXTitle("Eta");
      
      canvas2->cd(3);
      TestTree->Draw("globalTrackChi2");
      htemp->SetXTitle("Chi2");
      cout <<"\t\tChi2\t\t"<<htemp->GetMean()<<"\t\t"<<htemp->GetRMS()<<endl;
      
      }
    */
 
 return;
}

