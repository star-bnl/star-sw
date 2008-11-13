class StChain;
class StMuEmcCollection;
StChain *chain=0;


int rdMuDst2print(
	  char* file    = "st_physics_adc_9067013_raw_1430001.MuDst.root",
	  int nEve=1,
	  Int_t nFiles  = 1, 
	  char* inDir   = "/star/data05/scratch/balewski/bug1new/")
{ 

  printf("BPRSX %s\n",inDir+20);
  
  // inDir="/star/institutions/iucf/balewski/2006-ppv-eval/test7/";
  //file    = "st_physics_adc_7118049_raw_1070001.MuDst.root",

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
  //---------------------------------------------------
  int eventCounter=0;
  int t1=time(0);
  
  for (Int_t iev=0;iev<nEntries; iev++) {
    if(eventCounter>=nEve) break;
    chain->Clear();
    int stat = chain->Make();
    if(stat) break; // EOF or input error
    //printf("stat=%d\n", stat);
    eventCounter++;
    // if(eventCounter<17) continue;

    // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();

    StEventInfo &info=muEve->eventInfo();
    int nPrimV=muMk->muDst()->numberOfPrimaryVertices();
    StMuTriggerIdCollection &tic=muEve->triggerIdCollection();

    int trigID=96211;
    bool fired=tic.nominal().isTrigger(trigID);
  
    Int_t nGlobTrAll=muMk->muDst()->GetNGlobalTrack();
  
    //assert(tic.nominal().isTrigger(127271)==0);
    
    if(eventCounter%1==0) {
      printf("ieve=%d  eventID %d nPrimV=%d  nGlobTrAll=%d =============\n", eventCounter,info.id(),nPrimV,nGlobTrAll);
      // printf("TrigID=%d fired=%d\n",trigID,fired);
      
      if(nPrimV>1) printf("######\n");
    }
    int iv;
    for(iv=0;iv<nPrimV;iv++) {
      StMuPrimaryVertex* V= muMk->muDst()->primaryVertex(iv);
      assert(V);
      muMk->muDst()->setVertexIndex(iv);
      StThreeVectorF &r=V->position();
      StThreeVectorF &er=V->posError();
      //  printf("iv=%d   Vz=%.2f +/-%.2f \n",iv,r.z(),er.z()  );
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

      if(1)printf("  nPrimTr=%d , Z=%.1f VFid=%d:: ntrVF=%d nCtb=%d nBemc=%d nEEmc=%d nTpc=%d sumPt=%.1f rank=%g\n"
		  ,nPrimTr,r.z(), V->vertexFinderId() ,V->nTracksUsed()  ,V->nCTBMatch()  ,V-> nBEMCMatch() ,V->nEEMCMatch()  ,V->nCrossCentralMembrane()  ,V->sumTrackPt()  ,V->ranking());

    } 

    continue;   // do NOT print prim tracks for each vertex  

    for(iv=0;iv<nPrimV;iv++) {
      printf("  Prim tracks belonging to %d prim vertex:\n",iv);      
      int itr; 
      int ntr=0;
      for(itr=0;itr<nPrimTrAll;itr++) {
	StMuTrack *pr_track=muMk->muDst()->primaryTracks(itr);
	assert((pr_track->vertexIndex()==iv));
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

    StMuEmcCollection* emc = muMk->muDst()->muEmcCollection();
    if (!emc) {
      printf(" No EMC data for this event\n");
      return kStOK;
    }
    // printEEtower(emc);
    // printEEpre(emc);
    // printEEsmd(emc);
    printBPRS(emc);
    
  }
  printf("****************************************** \n");
  //  return;
  int t2=time(0);
  if(t2==t1) t2=t1+1;
  float tMnt=(t2-t1)/60.;
  float rate=1.*eventCounter/(t2-t1);
  printf("sorting done %d of   nEve=%d, CPU rate=%.1f Hz, total time %.1f minute(s) \n\n",eventCounter,nEntries,rate,tMnt);
  h1->Draw();

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
printBPRS( StMuEmcCollection* muEmc ) {
  int nprshits = muEmc->getNPrsHits();
  int prstot = 0;
  int captot = 0;
  printf("BPRSX %d hits====================\n",nprshits);
  for(int j = 0; j < nprshits; j++){
    StMuEmcHit* phit = muEmc->getPrsHit(j);
    int adc = phit->getAdc();
    int cap = phit->getCalType();
    int id= phit->getId();
    // printf("ih=%d softId=%d adc=%.1f cap=%d\n",j,id,adc,cap);
    printf("BPRSX %d %d %d\n",id,adc,cap);
  }
}

