// 
//   macro: find_vertex.C
// 
//   author: Marco van Leeuwen
// 
//   notes: placed in CVS by Gene Van Buren
//          Need to load libs before running this macro,
//          because of function with StEvent, StPrimaryVertex
//          When you run this, it will actually add 
//          new vertices to the event, so you have to
//          keep track of how many were there when
//          you read the file at first; I think the
//          new vertices are added after the old one.
// 

//class StEmcDetector;
class StChain;
//class StEmcRawData;
//class EEfeeDataBlock;
//class Collection;
//class StSPtrVecTrackNodeIterator ;
//class StTriggerIdCollection;
class StMinuitVertexFinder;
class StGenericVertexFinder;
class StGenericVertexMaker;

StChain *chain=0;
TH1F *dca_z_h=0;
TNtuple *vtx_tuple=0;

float eval_print_vertex(StEvent *event, StPrimaryVertex *vtx) {
  cout << "Vertex at " << vtx->position() << " chisq " << vtx->chiSquared() << endl;
  cout << "CTB matches: " << vtx->numMatchesWithCTB() << " BEMC: " 
       << vtx->numMatchesWithBEMC() << " cross central membrane " 
       << vtx->numTracksCrossingCentralMembrane() 
       << " rank " << vtx->ranking() << endl;
  int cnt_trk=0;
  int cnt_dcaz=0;
  int cnt_dca=0;
  Int_t n_node = event->trackNodes().size();
  Double_t avg_dip = 0;
  Double_t rms_dip = 0;
  for (int i_node = 0; i_node < n_node; i_node++) {
    StTrackNode *node = event->trackNodes()[i_node];
    StTrack *track = node->track(global);
    if (track &&
	track->flag() >= 0 &&
	track->fitTraits().numberOfFitPoints() >= 15 &&
	!track->topologyMap().trackFtpc() &&
	TMath::Finite(track->length()) ) {
      //cout << "track " << track << endl;
      cnt_trk++;
      double pathlength = track->geometry()->helix().pathLength( vtx->position(), false ); // do not scan periods
      StThreeVectorF dca = track->geometry()->helix().at(pathlength)-vtx->position(); 
      dca_z_h->Fill(dca.z());
      if (fabs(dca.z()) < 3) {
	cnt_dcaz++;
	float dip = track->geometry()->helix().dipAngle();
	avg_dip += dip;
	rms_dip += dip * dip;
      } 
      if (dca.mag() < 3) {
	cnt_dca++;
      } 
    }
  }
  if (cnt_dcaz) {
    avg_dip /= cnt_dcaz;
    rms_dip = sqrt(rms_dip - cnt_dcaz*avg_dip*avg_dip) / cnt_dcaz;
  }
  cout << "Tracks used in vtx " << vtx->numTracksUsedInFinder() << ", num with dcaz cut " << cnt_dcaz << ", dca < 3 " << cnt_dca << endl;
  cout << "avg dip " << avg_dip << ", rms dip " << rms_dip << endl;
  vtx_tuple->Fill(vtx->numTracksUsedInFinder(),vtx->position().z(),vtx->chiSquared(),cnt_dcaz,cnt_dca,vtx->numMatchesWithBEMC(),vtx->numTracksCrossingCentralMembrane(),avg_dip,rms_dip);

  return vtx->chiSquared();
}

void find_vertex(char * fname="high_053/st_physics_6053108_raw_2020002.event.root", Int_t nevents=1000){

  //fname="high_053/st_physics_6053108_raw_2020002.event.root";// daq1
  //fname="low_mb_49/st_physics_adc_6049129_raw_3080002.event.root";

  //      ROOT libs
    gSystem->Load("libPhysics");
    gSystem->Load("libTable");
    gSystem->Load("libGeom");

//      STAR libs  
    gSystem->Load("StarMagField");
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");        // new addition 22jul99
    gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StTriggerDataMaker"); // new starting from April 2003
    gSystem->Load("StBichsel");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");
    gSystem->Load("StEmcUtil"); 
    gSystem->Load("StTofUtil");
    gSystem->Load("StPmdUtil");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("StMagF");

    gSystem->Load("StDbLib.so");
    gSystem->Load("StDbBroker.so");
    gSystem->Load("libStDb_Tables.so");
    gSystem->Load("St_db_Maker.so");


    cout << " loading of shared libraries done" << endl;


  //gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  //loadSharedLibraries();
 
  // Load my makers
 
  gSystem->Load("Sti");
  gSystem->Load("libStEEmcUtil");
  gSystem->Load("StGenericVertexMaker");
  
  TFile *fout = new TFile("vtx_tree.root","RECREATE");
  dca_z_h = new TH1F("dca_z_h","dz_z_h",100,-50,50);
  vtx_tuple = new TNtuple("vtx_tuple","vertex info","num_trk:vtx_z:chisq:n_dcaz:n_dca:n_bemc:n_cross:avg_dip:rms_dip");

  // book some histos
  //gSystem->Load("libStGenericVertexMaker");
  // create chain    
  chain = new StChain("bfc"); 
  //chain->SetDebug();
  
  // Now we add Makers to the chain...

  // StIOMaker - to read files ...
  StIOMaker* ioMaker = new StIOMaker();  
  //ioMaker->SetFile("photon_bemc.event.root"); 
  ioMaker->SetFile(fname); 
  //ioMaker->SetDebug();
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
 
  St_db_Maker* dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");

  //StGenericVertexMaker *myfinder=new StGenericVertexMaker("myvertexfinder");
  
  StGenericVertexMaker *myfinder=new StGenericVertexMaker("myvertexfinder");
  //StGenericVertexMaker *myfinder=new StMinuitVertexFinder("myvertexfinder");
  //StMaker *myfinder=new StMinuitVertexFinder("myvertexfinder");
  myfinder->SetDebug(1);
  myfinder->SetMode(1);  // Select Minuit finder
  myfinder->SetInternalFind();  // Activate finding
  myfinder->Init();
  
  chain->PrintInfo();
  //chain->ls(3);
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
    
  int istat=0,iev=0;

  // Do the event loop    
  while(1) {
    if (iev>=nevents) break;
    chain->Clear();
    cout << "---------------------- Processing Event : " << iev << " ---------------------- " << endl;
   istat = chain->Make();
    iev++;
    if(istat) break;
    cout << "istat " << istat<<endl;

    // if(iev<17) continue;
    if (istat  == kStEOF || istat == kStFatal) break;

    StEvent* mEvent = (StEvent*)chain->GetInputDS("StEvent");
    assert(mEvent);// fix your chain or open the right event file

	//StMinuitVertexFinder* test_finder=(StMinuitVertexFinder*) myfinder->GetGenericFinder();
	//myfinder->GetGenericFinder().printInfo();
	//StMinuitVertexFinder test_finder;//=new StMinuitVertexFinder();
	//test_finder->setPrintLevel(3);
	//StThreeVectorD myvertex;
	//if (myfinder->fit(mEvent)) {
	//myvertex = myfinder->result();
	///test_finder->printInfo();
	  //}
	  //else
	  //      cout << "Error: vertex fit failed, no vertex." << endl;
	  
	  //delete myfinder;
	  
	  //myfinder->Finish();
          Int_t nV=mEvent->numberOfPrimaryVertices();
          if (nV == 0) continue;
          int iv;
	  Float_t best_rank=1e9;
          StPrimaryVertex *best_vtx=0;
	  for(iv=0;iv<nV;iv++) {
	    StPrimaryVertex *V=mEvent->primaryVertex(iv);
	    assert(V);
	    StThreeVectorF &r=V->position();
	    StThreeVectorF &er=V->positionError();
            Float_t rank=eval_print_vertex(mEvent, V);
            if (best_vtx==0 || rank < best_rank) {
              best_vtx = V;
	      best_rank = rank;
	    }
	  }
          if (best_vtx)
            cout << "Best vertex: " << *best_vtx << endl; 
// 	printf("  nPrimTr=%d , VFid=%d:: ntrVF=%d nCtb=%d nBemc=%d nEEmc=%d nTpc=%d sumPt=%.1f rank=%g\n"
// 	       ,V->numberOfDaughters(), V->vertexFinderId() ,V->numTracksUsedInFinder()  ,
// 	       V->numMatchesWithCTB()  ,V-> numMatchesWithBEMC() ,V->numMatchesWithEEMC()  ,
// 	       V->numTracksCrossingCentralMembrane()  ,V->sumOfTrackPt()  ,V->ranking());
	
// 	continue;
// 	int nPrTr=0;
// 	//.... access prim tracks for given vertex
// 	int itr;
// 	for(itr=0; itr<V->numberOfDaughters(); itr++) {
// 	  StTrack *track=V-> daughter(itr);
// 	  if(track==0)  continue;
// 	  if (track->flag() <0 ) continue;
// 	  printf("itr=%d pT=%.1f eta=%.2f nFitP=%d DCA=%.1f\n",itr,
// 		 track->geometry()->momentum().mag(),
// 		 track->geometry()->momentum().pseudoRapidity(),
// 		 track->fitTraits().numberOfFitPoints(),
// 		 track->geometry()->helix().distance(V->position()));
// 	  nPrTr++;
// 	}
	
// 	printf("  counted nPrimTr=%d \n",nPrTr);
//       } // end of loop over vertices
 
  
  } // Event Loop
  //chain->Finish();
  fout->Write();
}


//
// $Id: find_vertex.C,v 3.1 2008/03/19 19:39:08 genevb Exp $
// $Log: find_vertex.C,v $
// Revision 3.1  2008/03/19 19:39:08  genevb
// Introduction of macro
//
//
