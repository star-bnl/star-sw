// 
//   macro: find_vertexR6.C (ROOT 6 Version)
// 

#pragma cling load("libPhysics")
#pragma cling load("libTable")
#pragma cling load("libGeom")
#pragma cling load("libStarMagField")
#pragma cling load("libSt_base")
#pragma cling load("libStChain")
#pragma cling load("libSt_Tables")
#pragma cling load("libStUtilities")
#pragma cling load("libStTreeMaker")
#pragma cling load("libStIOMaker")
#pragma cling load("libStarClassLibrary")
#pragma cling load("libStTriggerDataMaker")
#pragma cling load("libStBichsel")
#pragma cling load("libStEvent")
#pragma cling load("libStTpcDb")
#pragma cling load("libStEventUtilities")
#pragma cling load("libStEmcUtil")
#pragma cling load("libStTofUtil")
#pragma cling load("libStPmdUtil")
#pragma cling load("libStPreEclMaker")
#pragma cling load("libStStrangeMuDstMaker")
#pragma cling load("libStMuDSTMaker")
#pragma cling load("libStMagF")
#pragma cling load("libStDbLib")
#pragma cling load("libStDbBroker")
#pragma cling load("libStDb_Tables")
#pragma cling load("libSt_db_Maker")
#pragma cling load("libSti")
#pragma cling load("libStEEmcUtil")
#pragma cling load("libMinuit")
#pragma cling load("libStGenericVertexMaker")

class StChain;
class StMinuitVertexFinder;
class StGenericVertexFinder;
class StGenericVertexMaker;
class StEvent;
class StPrimaryVertex;

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
      
      cnt_trk++;
      double pathlength = track->geometry()->helix().pathLength( vtx->position(), false );
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

void find_vertexR6(const char * fname="high_053/st_physics_6053108_raw_2020002.event.root", Int_t nevents=1000){

  cout << " loading of shared libraries done via pragmas" << endl;

  TFile *fout = new TFile("vtx_tree.root","RECREATE");
  dca_z_h = new TH1F("dca_z_h","dz_z_h",100,-50,50);
  vtx_tuple = new TNtuple("vtx_tuple","vertex info","num_trk:vtx_z:chisq:n_dcaz:n_dca:n_bemc:n_cross:avg_dip:rms_dip");

  // create chain    
  chain = new StChain("bfc"); 
  
  // StIOMaker - to read files ...
  StIOMaker* ioMaker = new StIOMaker();  
  ioMaker->SetFile(fname); 
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
 
  St_db_Maker* dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","StarDb");
  
  StGenericVertexMaker *myfinder = new StGenericVertexMaker("myvertexfinder");
  myfinder->SetDebug(1);
  myfinder->SetMode(1);  // Select Minuit finder
  myfinder->SetInternalFind();  // Activate finding
  myfinder->Init();
  
  chain->PrintInfo();
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal("initStat", "during Init()");
    
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

    if (istat  == kStEOF || istat == kStFatal) break;

    StEvent* mEvent = (StEvent*)chain->GetInputDS("StEvent");
    assert(mEvent);// fix your chain or open the right event file

    Int_t nV=mEvent->numberOfPrimaryVertices();
    if (nV == 0) continue;
    int iv;
    Float_t best_rank=1e9;
    StPrimaryVertex *best_vtx=0;
    
    for(iv=0;iv<nV;iv++) {
      StPrimaryVertex *V=mEvent->primaryVertex(iv);
      assert(V);
      Float_t rank=eval_print_vertex(mEvent, V);
      if (best_vtx==0 || rank < best_rank) {
        best_vtx = V;
        best_rank = rank;
      }
    }
    if (best_vtx) {
      cout << "Best vertex: " << *best_vtx << endl; 
    }
  } // Event Loop

  fout->Write();
}