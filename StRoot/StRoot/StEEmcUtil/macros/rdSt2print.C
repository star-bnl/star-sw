// *****************************************************************************
class StEmcDetector;
class StChain;
class  StEmcRawData;
class EEfeeDataBlock;
class Collection;
StChain *chain=0;

void rdSt2print(Int_t nevents=100){

  char * fname="/star/data09/reco/ppProductionTrans/FullField/dev/2006/129/7129060/st_physics_7129060_raw_1030002.event.root";

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  cout << " loading done " << endl;
     
  // Load my makers

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
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  ioMaker->SetBranch("eventBranch",0,"r");   //activate Event Branch
  ioMaker->SetIOMode("r"); 
 
  // StMcEventMaker
  // StMcEventMaker *mcEventMaker = new StMcEventMaker();

  // My Makers 
  //  StEmcTrigSimuMaker *myMk2=new StEmcTrigSimuMaker("eemcTrigMaker");

  // Now execute the chain Init functions
  chain->PrintInfo();
  chain->ls(3);
  Int_t initStat = chain->Init(); // This should call the Init() method in ALL makers
  if (initStat) chain->Fatal(initStat, "during Init()");
    
  int istat=0,iev=1;

  // Do the event loop    
  while(iev<=nevents && istat!=2) {
    chain->Clear();
    cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
    istat = chain->Make(iev); // This should call the Make() method in ALL makers
    iev++; 
    if (istat  == kStEOF || istat == kStFatal) break;
    
    StEvent* mEvent = (StEvent*)chain->GetInputDS("StEvent");
    assert(mEvent);// fix your chain or open the right event file
    
    int nV=mEvent->numberOfPrimaryVertices();
    if(nV==0) continue;
    int iv;
    printf("eveID=%d  nPrimVert=%d\n", mEvent->id(),nV);
    for(iv=0;iv<nV;iv++) {
      StPrimaryVertex *V=mEvent->primaryVertex(iv);
      assert(V);
      StThreeVectorF &r=V->position();
      StThreeVectorF &er=V->positionError();
      printf("iv=%d   Vz=%.2f +/-%.2f \n",iv,r.z(),er.z()  );
      printf("  nDaugh=%d , VFid=%d:: ntr=%d nCtb=%d nBemc=%d nEEmc=%d nTpc=%d sumPt=%.1f rank=%g xchi2=%g\n"
	     ,V->numberOfDaughters(), V->vertexFinderId() ,V->numTracksUsedInFinder()  ,V->numMatchesWithCTB()  ,V-> numMatchesWithBEMC() ,V->numMatchesWithEEMC()  ,V->numTracksCrossingCentralMembrane()  ,V->sumOfTrackPt()  ,V->ranking(), V->chiSquared());
    }
    
    
    StEmcCollection* emcC =(StEmcCollection*)mEvent->emcCollection(); assert(emcC);
    
    //................... print Endcap hits in StEvent
    // printETOW(emcC->detector(13));
    //    printEPRE(emcC->detector(14));
    //printESMD(emcC->detector(15));
    //     printESMD(emcC->detector(16));
    
    // printRaw(emcC->eemcRawData());
    
    //................... print Barrel hits in StEvent
    printRawBEMC(emcC->bemcRawData());
    
  } // Event Loop
  chain->Finish();
  //    delete myMk2;
   
    
}

// ****************************************************************************/

void printRawBEMC(StEmcRawData *raw) {


  if(!raw) return;

/*
data banks
0 - tower
1-8 - SMD
9-12- PSD
*/

  int NBANK = 13;



//for the headers
 for(int i = 0; i<NBANK;i++) {
     int size = raw->sizeHeader(i);
     printf("\n======\nBANK=%d headerSize=%d  dataSize=%d\n",i,size,raw->sizeData(i));
 
     if(raw->header(i))  {
       for(int j = 0;j<size;j++)  {
	 if(j%16==0) printf("\n");
	 printf("0x%04x ",raw->header(i,j));
       }
     }
     int tot=0;
#if 1
   if(raw->data(i))   {
     int size = raw->sizeData(i);
     printf("\nBANK=%d data size=%d",i,size);
     for(int j = 0;j<size;j++) {
       if(j%16==0) printf("\n");
       printf("0x%04x ",raw->data(i,j));
       tot++;
     }
   } 
#endif
   printf("\n tot=%d\n",tot);
 }
}



//=============================================
//=============================================
//=============================================

printRaw(  StEmcRawData* raw) {

  printf("printRaw(%p)\n",raw);

    assert(raw);
    int icr;
    printf("nBlocks=%d\n",raw->getNBlocks());
    EEfeeDataBlock block;
    for(icr=0; icr<raw->getNBlocks();icr++) {
      if(raw->sizeData(icr)<=0) continue;
      
      const  UShort_t* head=raw->header(icr);
      const  UShort_t* data=raw->data(icr);
      assert(head);
      printf("icr=%d, size: head=%d data=%d\n",icr,raw->sizeHeader(icr),raw->sizeData(icr));
      
      int i;
      block.clear();
      block.setHead(raw->header(icr));
      block.setDataArray(raw->data(icr),raw->sizeData(icr));
      if(icr>=6) continue; // just towers
      block.print(0);
      
    }
    
}
//=============================================
//=============================================
//=============================================

printETOW( StEmcDetector* det) {

  printf("printTw(%p)\n",det);
  assert(det);
  printf("towers nHit=%d nMod=%d\n",det->numberOfHits(),det->numberOfModules());
  int nPos=0;
  for(int mod=1;mod<=det->numberOfModules();mod++) {
   StEmcModule*     module=det->module(mod);
   printf("ETOW sector=%d nHit=%d\n",mod,module->numberOfHits());
   StSPtrVecEmcRawHit&     hit=  module->hits();
   int ih;
   for(ih=0;ih<hit.size();ih++){
     StEmcRawHit *x=hit[ih];
     int sec=x->module();
     char sub='A'+x->sub()-1;
     int eta=x->eta();
     int adc=x->adc();
     // if(adc>0) continue;
     printf("ih=%d %02dT%c%02d -->adc=%d ener=%f\n",ih,sec,sub,eta,adc, x->energy());
     if(adc>0) nPos++;
     int adcX=1000+ (eta-1) + (sub-'A')*12 +(sec-1)*60;
     //     assert(adc==adcX  );

     // printf("ih=%d, mod=%d eta=%d sub=%d adc=%d\n",ih,x->module(),x->eta(),x->sub(),x->adc());
   }
   printf("nPos=%d\n",nPos);
 }
  printf("total nPos=%d\n",nPos);
   printf("nPos=%d of %d \n",nPos,det->numberOfHits());

}


//=============================================
//=============================================
//=============================================

printEPRE( StEmcDetector* det) {

  printf("printPre/post(%p)\n",det);
  assert(det);
  printf("pre/post nHit=%d nMod=%d\n",det->numberOfHits(),det->numberOfModules());
  int nPos=0;
  for(int imod=1;imod<=det->numberOfModules();imod++) {
   StEmcModule*     module=det->module(imod);
   printf("EPRE sect=%d nHit=%d\n",imod, module->numberOfHits());
   StSPtrVecEmcRawHit&     hit=  module->hits();
   int ih;
   for(ih=0;ih<hit.size();ih++){
     StEmcRawHit *x=hit[ih];
     int sec=x->module();
     int ss=x->sub()-1;
     char sub='A'+ss%5;
     char preL='P'+ss/5;
     int eta=x->eta();
     int adc=x->adc();
      printf("ih=%d %02d%c%c%02d ss=%d -->adc=%d ener=%f ss=%d\n",ih,sec,preL,sub,eta,ss,adc, x->energy(),ss);
     if(adc>0) nPos++;
     int adcX=      (eta-1) + (sub-'A')*12 +(sec-1)*60 + 1000*(preL-'P'+1);
     //     assert(adc==adcX  );
   }
   printf("nPos=%d\n",nPos);
 }
  //  printf("total nPos=%d\n",nPos);
   printf("nPos=%d of %d \n",nPos,det->numberOfHits());

}



//=============================================
//=============================================
//=============================================

printESMD( StEmcDetector* det) {

  printf("printSMD/post(%p)\n",det);
  assert(det);
  printf("U-SMD nHit=%d nMod=%d\n",det->numberOfHits(),det->numberOfModules());
  int nPos=0;
  for(int imod=1;imod<=det->numberOfModules();imod++) {
   StEmcModule*     module=det->module(imod);
   printf("ESMD sector=%d nHit=%d\n",imod, module->numberOfHits());
   StSPtrVecEmcRawHit&     hit=  module->hits();
   int ih;
   for(ih=0;ih<hit.size();ih++){
     StEmcRawHit *x=hit[ih];
     int sec=x->module();
     int strip=x->eta();
     int adc=x->adc();
       printf("ih=%d %02dU%03d -->adc=%d ener=%f\n",ih,sec,strip,adc, x->energy());
     if(adc>0) nPos++;
     int adcX= 1000+(strip   -1) +(sec-1)*300;
     //     assert(adc==adcX  );
   }
   printf(" nPos=%d\n",nPos);
 }
  printf("nPos=%d of %d \n",nPos,det->numberOfHits());


}






