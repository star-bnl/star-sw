class StChain;
class StMuEmcCollection;
StChain *chain=0;
const int mxv=4;
TH1F *hA[8], *hT[4], *hF[4];

TFile *hf=0;

int rdMu2PrimTr(
		char* file    = "st_physics_6145042_raw_2040013.MuDst.root",
		Int_t nFiles  = 2, 
		char* inDir   = "out1/",
		int nEve=100) { 

  initHisto();
  // inDir="out0/";
  // file="R6145042.lis";

  inDir="/star/data04/sim/balewski/tmp2/";
  file="st_physics_6111024_raw_1040001.MuDst.root";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;

 
// create chain    
  chain = new StChain("StChain"); 
  
// Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree);
  int nEntries=(int)tree->GetEntries();
  printf("total eve in chain =%d\n",nEntries);
  printf("in=%s%s=\n",inDir,file);
  
  chain->Init();
  chain->ls(3);

  int eventCounter=0;
  int stat=0;

  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    chain->Clear();
    stat = chain->Make();
    if(stat) break; // EOF or input error
    // printf("stat=%d\n", stat);
    eventCounter++;

    // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();

    StEventInfo &info=muEve->eventInfo();
    int nPrimV=muMk->muDst()->numberOfPrimaryVertices();
    StMuTriggerIdCollection &tic=muEve->triggerIdCollection();

    int trigID=96211;
    bool fired=tic.nominal().isTrigger(trigID);
  
    Int_t nPrimTrAll=muMk->muDst()->GetNPrimaryTrack();
    Int_t nGlobTrAll=muMk->muDst()->GetNGlobalTrack();
    
   if(eventCounter%1000==0)  printf("\n\n ====================%d  processing eventID %d nPrimV=%d nPrimTr=%d  nGlobTrAll=%d =============\n", eventCounter,info.id(),nPrimV,nPrimTrAll,nGlobTrAll);
   //   printf("TrigID=%d fired=%d\n",trigID,fired);
    
   // if(nPrimV>1) printf("######\n");

    hA[0]->Fill(nPrimV);
    int iv;
    for(iv=0;iv<nPrimV;iv++) {
      StMuPrimaryVertex* V= muMk->muDst()->primaryVertex(iv);
      assert(V);
      StThreeVectorF &r=V->position();
      StThreeVectorF &er=V->posError();
      // printf("iv=%d   Vz=%.2f +/-%.2f \n",iv,r.z(),er.z()  );
      // count prim tracks for this vert

      float rank1K=V->ranking()/1000.;
      hA[5]->Fill(er.z() );
     ((TH2F*) hA[6])->Fill(er.z() ,rank1K);

      if(nPrimV==1 && iv==0) hA[1]->Fill(rank1K);
      if(nPrimV>1){ 
	if(iv==0)hA[2]->Fill(rank1K);
	if(iv==1)hA[3]->Fill(rank1K);
	if(iv==2)hA[4]->Fill(rank1K);
	if(iv==1)hA[7]->Fill(rank1K/muMk->muDst()->primaryVertex(0)->ranking()*1000.);

      }
      int nPrimTr =0;
      int itr; 
      for(itr=0;itr<nPrimTrAll;itr++) {
	StMuTrack *pr_track=muMk->muDst()->primaryTracks(itr);
	if(pr_track->vertexIndex()!=iv) continue;
	if(pr_track->flag()<=0) continue;
	if(iv<mxv) {
	  if(pr_track->topologyMap().trackTpcOnly()) hT[iv]->Fill(pr_track->nHitsFit());
	  if(pr_track->topologyMap().trackFtpc()) hF[iv]->Fill(pr_track->nHitsFit());
	}
	nPrimTr ++;
      }
      continue;
      printf("  nPrimTr=%d , VFid=%d:: ntrVF=%d nCtb=%d nBemc=%d nEEmc=%d nTpc=%d sumPt=%.1f rank=%g\n"
	     ,nPrimTr, V->vertexFinderId() ,V->nTracksUsed()  ,V->nCTBMatch()  ,V-> nBEMCMatch() ,V->nEEMCMatch()  ,V->nCrossCentralMembrane()  ,V->sumTrackPt()  ,V->ranking());
    } 

    continue;   // do NOT print prim tracks for each vertex  

    for(iv=0;iv<nPrimV;iv++) {
      printf("  Prim tracks belonging to %d prim vertex:\n",iv);      
      int itr; 
      int ntr=0;
      for(itr=0;itr<nPrimTrAll;itr++) {
	StMuTrack *pr_track=muMk->muDst()->primaryTracks(itr);
	if(pr_track->vertexIndex()!=iv) continue;
	if(pr_track->flag()<=0) continue;	
	ntr++;
	cout << "\nPrimary track " << ntr << " momentum " << pr_track->p() << endl;  cout << "\t flag=" << pr_track->flag() << " nHits=" << pr_track->nHits()<< " vertID="<<  pr_track->vertexIndex()<< endl;
	cout << "\t primV("<<iv<<")  primDCA=" << pr_track->dca(iv) << endl;
	if(pr_track->dca(iv).mag()>5) 	cout << "^^^^^ 3D DCA magnitude="<<pr_track->dca(iv).mag()<<endl;
	// cout << "\t first point " << pr_track->firstPoint() << endl;
	// cout << "\t last point " << pr_track->lastPoint() << endl;
      } // end of loop over tracks
    }// end of loop over vertices
    
   continue;     
  }
  hf->Write();
  
}

//===========================================
//===========================================
void initHisto() {
  hf=new TFile("out.hist.root","recreate");
  hA[0]=new TH1F("nV","# of prim vertices per eve; # vertex",10,-0.5,9.5);
  int iv;
  for(iv=0;iv<mxv;iv++) {
    char tt1[100], tt2[300];
    sprintf(tt1,"nPTv%d",iv);
    sprintf(tt2,"nFitPoint for TPC prim tracks for vertex%d",iv);
    hT[iv]=new TH1F(tt1,tt2,50,-0.5,49.5);

    sprintf(tt1,"nPFv%d",iv);
    sprintf(tt2,"nFitPoint for FTPC prim tracks for vertex%d",iv);
    hF[iv]=new TH1F(tt1,tt2,50,-0.5,49.5);
  }

  hA[1]=new TH1F("L11","vertex rank of 1st vertex if only one found; vertex  rank/1000",100,0,20);
  hA[2]=new TH1F("Ln1","vertex rank of 1st vertex if more than one found; vertex  rank/1000",100,0,20);
  hA[3]=new TH1F("Ln2","vertex rank of 2nd vertex if more than one found; vertex  rank/1000",100,0,10);
  hA[4]=new TH1F("Ln3","vertex rank of 3rd vertex if more than one found; vertex  rank/1000",100,0,10);
  hA[5]=new TH1F("zEr","vertex Z-error; error Z (cm)",1000,0,1.);
  TH2F *h2=new TH2F("zErL","Rank vs. vertex Z-error ; error Z (cm); rank/1000",100,0,0.5,100,0,20.);
  hA[6]=(TH1F*) h2;
  hA[7]=new TH1F("Ln2d1","ratio of vertex rank  2nd/1st ; rank2/rank1",100,0,1.);
}


//===============================================
pl(int page=1){
 
  fd=new TFile("out.hist.root");
  assert(fd->IsOpen());
  fd->ls();

  TString tt;


  switch(page){
  case 1:{
    tt="nPTv";
  } break;

  case 2:{
    tt="nPFv";
  } break;

  default:;
  }

    c=new TCanvas();
    c->Divide(2,2);
    int iv;
    for(iv=0;iv<4;iv++){
      TString tt1=tt;
      tt1+=iv;
      printf("=%s=\n",tt1.Data());
  
      h=(TH1F*)fd->Get(tt1); assert(h);
      c->cd(iv+1);
      h->Draw();
    }

}
