class StChain;
class StMuEmcCollection;
StChain *chain=0;


int rdMuDst2print(
	  char* file    = "cc.MuDst.root",
	  Int_t nFiles  = 1, 
	  char* inDir   = "./",
	  int nEve=5)
{ 


  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;

 
// create chain    
  chain = new StChain("StChain"); 
  
// Now we add Makers to the chain...   
  muDstMaker = new StMuDstMaker(0,0,inDir,file,"MuDst.root",nFiles);
  
  chain->Init();
  chain->ls(3);

  int eventCounter=0;
  int stat=0;

  //---------------------------------------------------
  while ( stat==0 ) {// loop over events
    if(eventCounter>=nEve) break;
    chain->Clear();
    stat = chain->Make();
    printf("\n\n ==================== processing event# %d ==============\n", eventCounter++);
 
    StMuEmcCollection* emc = muDstMaker->muDst()->emcCollection();
    if (!emc) {
      printf(" No EMC data for this event\n");
      return kStOK;
    }
    printEEtower(emc);
    printEEpre(emc);
    printEEsmd(emc);
    
  }
  
}

//===========================================
//===========================================
printEEtower( StMuEmcCollection* emc ) {
  int isec,ieta,isub,istrip,adc,ipre;
  StMuEmcHit *hit;
  
  int i, nh;
  
  printf("\Total %d hits in Tower\n",emc->getNEndcapTowerADC());
  nh=0;
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    emc->getEndcapTowerADC(i,adc,isec,isub,ieta);
    if (adc<=0) continue; // print only non-zero values
    nh++;
    printf("  Tower %2.2dT%c%2.2d   adc=%4d\n",isec+1,isub+'A',ieta+1,adc );
  }
  printf("  Total %d towers with ADC>0\n",nh);
}


//===========================================
//===========================================
printEEpre( StMuEmcCollection* emc ) {
  int isec,ieta,isub,istrip,adc,ipre;
  StMuEmcHit *hit;
  
  int i, nh;
  nh= emc->getNEndcapPrsHits();
  printf("\nTotal %d hits in pre1+2+post\n",nh);
  for (i=0; i<nh; i++) {
    hit=emc->getEndcapPrsHit(i,isec,isub,ieta,ipre);
    printf("  pre/post(%d) %2.2d%c%c%2.2d : energy=%f  adc=%d\n",ipre+1,isec+1,ipre+'P',isub+'A',ieta+1,hit->getEnergy(),hit->getAdc());
  }
}


//===========================================
//===========================================
printEEsmd( StMuEmcCollection* emc ) {
  int isec,ieta,isub,istrip,adc,ipre; char uv='U';
  
  for(uv='U'; uv<='V'; uv++) {
    int nh= emc->getNEndcapSmdHits(uv);
    printf("\nTotal %d hits in SMD-%c\n",nh,uv);
    for (int i=0; i<nh; i++) {
      hit=emc->getEndcapSmdHit(uv,i,isec,istrip);
      printf("  SMD-%c  %2.2d%c%3.3d : energy=%f  adc=%d\n",uv,isec+1,uv,istrip+1,hit->getEnergy(),hit->getAdc());
      
    }
  }
}
