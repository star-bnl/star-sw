class StChain;
class StMuEmcCollection;
class StMuTriggerIdCollection;
StChain *chain=0;



int rdMuDst2print(
		  char* file    = "st_physics_12033050_raw_4010001.MuDst.root",
	  int nEve=10,
	  char* inDir   = "./"
	  )
{ 
  Int_t nFiles  = 1;
  
  inDir="/star/data05/scratch/balewski/stW-2012A/data/";
  file="st_W_13078014_raw_4360001.MuDst.root";
 

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  cout << " loading done " << endl;

  
  // create chain    
  chain = new StChain("StChain"); 
  
  // Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  TChain* tree=muMk->chain(); assert(tree);
  int nEntries=(int) tree->GetEntries();
  printf("total eve in muDst chain =%d\n",nEntries);  // return ;
  if(nEntries<0) return;


  chain->Init();
  chain->ls(3);

  h1=new TH1F("nP","# of prim tracks per vertex",100,0,200);
  h2=new TH1F("vR","PPV Vertex rank; rank", 300, -1.2e6, 1.2e6); h2->SetLineColor(kRed);
  h3=new TH1F("vRL","PPV Vertex rank, funny X-axis; X=Log10(rank-1e6)+ offset", 150, -9,21);
  h2->GetXaxis()->SetTitleSize(.043);
  h2->GetXaxis()->SetTitleSize(.043);

  h4=new TH1F("trPhi"," prim tracks phi if PT>1.0 GeV/c; phi (rad)",50,-3.2,3.2);
  h5=new TH1F("zVerTrg"," Z trg-vertex , rank>0; Z (cm)", 50,-200,200);
  h6=new TH1F("zVerPlp"," Z pileup-vertex , rank<0; Z (cm)", 50,-200,200);
  h7=new TH1F("nPrV","# of prim tr used by PPV; rank>0",15,0,30);
  //---------------------------------------------------
  int eventCounter=0;
  int t1=time(0);
  
  for (Int_t iev=0;iev<nEntries; iev++) {
    if(eventCounter>=nEve) break;
    chain->Clear();
    int stat = chain->Make();
    if(stat) break; // EOF or input error
    eventCounter++;

    // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();

    StEventInfo &info=muEve->eventInfo();
    int nPrimV=muMk->muDst()->numberOfPrimaryVertices();

    StMuTriggerIdCollection *tic=&(muEve->triggerIdCollection());
    int trigID=380305; // EHT1*L2EW in 2012
    bool isL2EW=tic->nominal().isTrigger(trigID);

    Int_t nGlobTrAll=muMk->muDst()->GetNGlobalTrack();
  
    TArrayI&  l2Array = muEve->L2Result();

    if(eventCounter%1==0) {
      printf("ieve=%d  eventID %d nPrimV=%d  nGlobTrAll=%d =============\n", eventCounter,info.id(),nPrimV,nGlobTrAll);
      printf("TrigID=%d fired=%d\n",trigID,isL2EW);
    }
    printL0Trig(tic);
    printL2Trig(l2Array); 
    
    int iv;
    if(0)for(iv=0;iv<nPrimV;iv++) {
      StMuPrimaryVertex* V= muMk->muDst()->primaryVertex(iv);
      assert(V);
      muMk->muDst()->setVertexIndex(iv);
      StThreeVectorF &r=V->position();
      StThreeVectorF &er=V->posError();
      printf("iv=%d   Vz=%.2f +/-%.2f \n",iv,r.z(),er.z()  );
      // count prim tracks for this vert
      int nPrimTr =0;
      int itr; 
      Int_t nPrimTrAll=muMk->muDst()->GetNPrimaryTrack();
      for(itr=0;itr<nPrimTrAll;itr++) {
	StMuTrack *pr_track=muMk->muDst()->primaryTracks(itr);
	if(pr_track->flag()<=0) continue;
	nPrimTr ++;
      }
      if(nPrimV>0)h1->Fill(nPrimTr);
      float rank=V->ranking();
      h2->Fill(rank);
      if(rank>1e6)  h3->Fill(log(rank-1e6)+10);
      else if(rank>0)   h3->Fill(log(rank));
      else   h3->Fill(log(rank+1e6)-10);

      if(1)printf("  nPrimTr=%d , Z=%.1f VFid=%d:: ntrVF=%d nCtb=%d nBemc=%d nEEmc=%d nTpc=%d sumPt=%.1f rank=%g\n"
		  ,nPrimTr,r.z(), V->vertexFinderId() ,V->nTracksUsed()  ,V->nCTBMatch()  ,V-> nBEMCMatch() ,V->nEEMCMatch()  ,V->nCrossCentralMembrane()  ,V->sumTrackPt()  ,V->ranking());
      if (rank>0) h5->Fill(r.z());
      if (rank>0) h7->Fill(V->nTracksUsed());
      if (rank<0) h6->Fill(r.z());
    } 
   
    if(0)   // do NOT print prim tracks 
      for(iv=0;iv<nPrimV;iv++) {
	muMk->muDst()->setVertexIndex(iv);
	Int_t nPrimTrAll=muMk->muDst()->GetNPrimaryTrack();
	cout<<"\n  Prim "<<nPrimTrAll<<" tracks belonging to "<<iv<<"-th prim vertex"<<endl;      

	int itr; 
	int ntr=0;
	
	for(itr=0;itr<nPrimTrAll;itr++) {
	  StMuTrack *pr_track=muMk->muDst()->primaryTracks(itr);
	  if(pr_track->flag()<=0) continue;	
	  ntr++;
	  cout << "\nPrimary track " << ntr << " momentum(P vect) " << pr_track->p() << " PT="<<pr_track->pt()<<  " recoCharge="<<pr_track->charge()<<  endl;  cout << "\t flag=" << pr_track->flag() << " nHits=" << pr_track->nHits()<< " vertID="<<  pr_track->vertexIndex()<< endl;
	  cout << "\t primV("<<iv<<")  primDCA=" << pr_track->dca(iv) << endl;
	  if(pr_track->dca(iv).mag()>5) 	cout << "^^^^^ 3D DCA magnitude="<<pr_track->dca(iv).mag()<<endl;
	  // cout << "\t first point " << pr_track->firstPoint() << endl;
	  // cout << "\t last point " << pr_track->lastPoint() << endl;
	  if(pr_track->pt()>1.0) h4->Fill(pr_track->phi());

	} // end of loop over tracks
	
      }// end of loop over vertices
  
    continue; 
    
    StMuEmcCollection* emc = muMk->muDst()->muEmcCollection();
    if (!emc) {
      printf(" No EMC data for this event\n");
      return kStOK;
    }
    //printEEtower(emc);
    // printEEpre(emc);
    // printEEsmd(emc);
    printBEtower(emc);
    //printBEpre(emc);
    //printBEsmd(emc);
  
    
  }
  printf("****************************************** \n");
  //  return;
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*eventCounter/(t2-t1);
  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n\n",eventCounter,nEntries,rate,tMnt);
  c=new TCanvas(); c->Divide(2,2);
  c->cd(1); h5->Fit("gaus"); //h5->Draw();
  c->cd(3); h6->Fit("gaus"); //h5->Draw();
  c->cd(2);  h3->Draw();
  c->cd(4);  h7->Draw();

  return;
  
}


//===========================================
//===========================================
printEEtower( StMuEmcCollection* emc ) {
  int sec,eta,sub,adc;
  StMuEmcHit *hit;
  
  int i, nh;
  
  printf("\Total %d hits in Tower (only ADC>0)\n",emc->getNEndcapTowerADC());
  nh=0;
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    emc->getEndcapTowerADC(i,adc,sec,sub,eta);
      if (adc<=0) continue; // print only non-zero values
    nh++;
    printf("i=%d  Tower %2.2dT%c%2.2d   adc=%4d\n",i,sec,sub+'A'-1,eta,adc );
    //    printf("  Tower isec=%d ieta=%d isub=%d    adc=%4d\n",sec,eta, sub,adc );
    int adcX=1000+ (eta-1) + (sub-1)*12 +(sec-1)*60;
    //    assert(adc==adcX );
}
  printf("  Total %d towers with ADC>0\n",nh);
}


//===========================================
//===========================================
printEEpre( StMuEmcCollection* emc ) {
  int sec,eta,sub,pre,adc;
  StMuEmcHit *hit;
  
  int i, nh;
  nh= emc->getNEndcapPrsHits();
  printf("\nTotal %d hits in pre1+2+post\n",nh);
  for (i=0; i<nh; i++) {
    hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    int ss=sub + 5*(pre-1);
    adc=hit->getAdc();
    printf("i=%d  pre/post(%d) %2.2d%c%c%2.2d : energy=%f  adc=%d\n",i,pre,sec,pre+'P'-1,sub+'A'-1,eta,hit->getEnergy(),adc);
    int adcX=      (eta-1) + (sub-1) *12 +(sec-1)*60 + 1000*pre;
    
    //    assert(adc==adcX  );

  }
}


//===========================================
//===========================================
printEEsmd( StMuEmcCollection* emc ) {
  int sec,strip,adc;
  char uv='U';
  
  for(uv='U'; uv<='V'; uv++) {
    int nh= emc->getNEndcapSmdHits(uv);
    printf("\nTotal %d hits in SMD-%c\n",nh,uv);
    for (int i=0; i<nh; i++) {
      hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      adc=hit->getAdc();
      printf("  SMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d\n",uv,sec,uv,strip,hit->getEnergy(),adc);
       int adcX= 1000 + strip-1  +(sec-1)*300;
       //       assert(adc==adcX  );
    }
  }
}

//===========================================
//===========================================
printBEtower( StMuEmcCollection* emc ) {
  int sec,eta,sub,adc;
  StMuEmcHit *hit;

  int i, nh;

  printf("\Total hits in Tower (only ADC>0)\n");
  nh=0;
  for (i=0; i< 4800; i++) {
    int adc = emc->getTowerADC(i);
    if (adc<=1000) continue; // print only non-zero values
    nh++;
    printf(" Tower id=%d   adc=%4d\n",i,adc );
    //    assert(adc==adcX );
}
  printf("  --> %d towers with ADC>thr\n",nh);
}




//===========================================
//===========================================
printBEpre( StMuEmcCollection* emc ) {

  int i, nh;
  nh = emc->getNPrsHits();
  printf("\nTotal %d hits in pre1\n",nh);

  int n1=0;
  for (i=0; i<nh; i++) {
    StMuEmcHit * hit = emc->getPrsHit(i);
    int adc = hit->getAdc();
    int id=hit->getId();
    if(adc<4) continue;
    n1++;
    printf("BPRS i=%d, id=%d adc = %d\n",i,id,adc);
  } 
  printf("   --> %d BPRS hits with ADC>thr\n",n1);
}

//===========================================
//===========================================
printBEsmd( StMuEmcCollection* emc ) {

  int n1=0,n2=0;
  int nh = emc->getNSmdHits(3);
  printf("\nTotal %d hits in SMDE\n",nh);
  for (int i=0; i<nh; i++) {
    StMuEmcHit * hit = emc->getSmdHit(i,3);
    adc = hit->getAdc();
    int id=hit->getId();
     if(adc<4) continue;
    n1++;
    printf("BSMDE i=%d, id=%d adc = %d\n",i,id,adc);
  }

  int nh = emc->getNSmdHits(4);
  printf("\nTotal %d hits in SMDP\n",nh);
  for (int i=0; i<nh; i++) {
    hit = emc->getSmdHit(i,4);
    adc = hit->getAdc();
    int id=hit->getId(); 
    if(adc<4) continue;
    n2++;
    printf("BSMDP i=%d,id=%d adc = %d\n",i,id,adc);
  }

  printf("   --> %d BSMD-E & %d BSMD-P  hits with ADC>thr\n",n1,n2);
}

//=======================================
printL2Trig( TArrayI& l2Array) {
  
  printf("AccessL2Decision() from regular muDst: L2Ar-size=%d\n",l2Array.GetSize());
  unsigned int *l2res=(unsigned int *)l2Array.GetArray();
  for(int i=0;i<l2Array.GetSize();i++){
    if(l2res[i]==0) continue;
    printf("i=%d val=0x%08x\n",i,l2res[i]);
  }
}


//--------------------------------------
void printL0Trig(StMuTriggerIdCollection *tic){
 
  const StTriggerId &l1=tic->l1();
  vector<unsigned int> idL=l1.triggerIds();
  printf("nTrig=%d, trigID: ",idL.size());
  for(unsigned int i=0;i<idL.size(); i++){
    printf("%d, ",idL[i]);
  }
  printf("\n dump L2Array:");

  //const int EEMCW_off=35; // valid only for 2011 run




}

