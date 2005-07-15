class StChain;
class StMuEmcCollection;
StChain *chain=0;


int rdMuDst2print(
	  char* file    = "R50530.MuDst.root",
	  Int_t nFiles  = 1, 
	  char* inDir   = "./",
	  int nEve=10)
{ 

  inDir="./"; file="st_physics_6145042_raw_2020009.MuDst.root";
  
  //  inDir="/star/u/mvl/mudst_dev/quick_fix/data/";
  // file="st_physics_5109030_raw_1020001.MuDst.root";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;

 
// create chain    
  chain = new StChain("StChain"); 
  
// Now we add Makers to the chain...   
  muMk = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  
  chain->Init();
  chain->ls(3);

  int eventCounter=0;
  int stat=0;

  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    chain->Clear();
    stat = chain->Make();

    // Access to muDst .......................
    StMuEvent* muEve = muMk->muDst()->event();

    StEventInfo &info=muEve->eventInfo();
      int nPrimV=muMk->muDst()->numberOfPrimaryVertices();
    //int nPrimV=muMk->muDst()->GetNPrimaryVertex();

    printf("\n\n ====================%d  processing eventID %d nPrimV=%d ==============\n", eventCounter++,info.id(),nPrimV);
   if(nPrimV>1) printf("######\n");
   int iv;
   for(iv=0;iv<nPrimV;iv++) {
     StMuPrimaryVertex* V= muMk->muDst()->primaryVertex(iv);
 	assert(V);
	StThreeVectorF &r=V->position();
	StThreeVectorF &er=V->posError();
	printf("iv=%d   Vz=%.2f +/-%.2f \n",iv,r.z(),er.z()  );
	int nPrim =V->nDaughters();   // get number of primary tracks 
	printf("  nPrimTr=%d , VFid=%d:: ntrVF=%d nCtb=%d nBemc=%d nEEmc=%d nTpc=%d sumPt=%.1f rank=%g\n",nPrim);

#if 0

	printf("  nPrimTr=%d , VFid=%d:: ntrVF=%d nCtb=%d nBemc=%d nEEmc=%d nTpc=%d sumPt=%.1f rank=%g\n"
	   ,V->numberOfDaughters(), V->vertexFinderId() ,V->numTracksUsedInFinder()  ,V->numMatchesWithCTB()  ,V-> numMatchesWithBEMC() ,V->numMatchesWithEEMC()  ,V->numTracksCrossingCentralMembran()  ,V->sumOfTrackPt()  ,V->ranking());
#endif
      }
    continue;

    StMuEmcCollection* emc = muMk->muDst()->muEmcCollection();
    if (!emc) {
      printf(" No EMC data for this event\n");
      return kStOK;
    }
    // printEEtower(emc);
    // printEEpre(emc);
    // printEEsmd(emc);
    
  }
  
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
