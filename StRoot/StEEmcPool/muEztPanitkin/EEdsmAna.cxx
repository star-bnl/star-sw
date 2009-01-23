#include <iostream>
#include <assert.h>

#include <TObjArray.h>
#include <TH2.h>
#include <TFile.h>

#include "EEdsmAna.h"
#if 0 // disable whole code, needs re-work to match with new EEdsm.so after Xin added  detailed Endcap triger simu code 

#include "daqFormats.h"
#include "trgReader.h"

  

#define EEmapTP_USE // trick instattiates data only in the cxx
#include "StEEmcUtil/EEdsm/EEmapTP.h" 
#undef EEmapTP_USE

#include "StEEmcUtil/EEdsm/EEdsm0.h"
#include "StEEmcUtil/EEdsm/EEdsm1.h"
#include "StEEmcUtil/EEdsm/EEdsm2.h"
#include "StEEmcUtil/EEdsm/EEdsm3.h"


ClassImp(EEdsmAna)

//--------------------------------------------------
//--------------------------------------------------
EEdsmAna ::  EEdsmAna( TObjArray *L, TString nm ) {
  HList=L;
  myName=nm;
  nTot=0;
  mYear=2006;

  //=====================  DSM 0 ===============
  Nee0=9; 
  ee0=new EEdsm0[Nee0];
  Nee0out=12;
  ee0outTPadc=new int [Nee0out];
  ee0outHTadc=new int [Nee0out];
  
  // set pedestals initially to 0
  ped0=new int *[Nee0]; // WARN, may be old from 2005 ???, probably not used in the code
  int ibr;
  for(ibr=0;ibr<Nee0; ibr++) {
    ped0[ibr]=new int [ee0[ibr].getNc()];
    int ch;
    for(ch=0;ch<ee0[ibr].getNc();ch++) 
      ped0[ibr][ch]=0;
  }

  //=====================  DSM 1 ===============
  Nee1=2; 
  ee1=new EEdsm1[Nee1];
  int i=0;
  for(i=0; i<Nee1; i++) ee1[i].setYear(mYear);
  ee1[0].setType(1);
  ee1[1].setType(2);
  ee1out3JPadc=new int [Nee1];
  
  //=====================  DSM 2 ===============
  ee2=new EEdsm2;  // Endcap
  ee2->setYear(mYear);
  
  be2=new EEdsm2[Nbe2];
  for(i=0; i<Nbe2; i++)   // Barrel
    be2[i].setYear(mYear);

  //=====================  DSM 3 ===============
  ee3=new EEdsm3;
  ee3->setYear(mYear);

  clear();
}

//--------------------------------------------------
//--------------------------------------------------
EEdsmAna ::  ~EEdsmAna() { }

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna :: clear() {
  
  int i;  
  for (i=0;i<Nee0;i++) ee0[i].clear();
  for (i=0;i<Nee0out;i++) { //# boards !=  # outputs
    ee0outTPadc[i]=0;
    ee0outHTadc[i]=0;
  }

  for (i=0;i<Nee1;i++) { // # boards ==# outputs
    ee1[i].clear();
    ee1out3JPadc[i]=0;
    // ee1outHT[i]=0;
  }

  memset(ee1outTPthrMax, 0, sizeof(ee1outTPthrMax));
  memset(ee1outHTTPthrMax, 0, sizeof(ee1outHTTPthrMax));

  for (i=0;i<EEnJetPatch;i++) { 
    ee1outJPadc[i]=0;
    ee1outHT[i]=0;
    //    AdjJPsum[i]=0;
    //    JPtrig[i]=0;
  }

  ee2->clear();
  ee2outHT=0;

  for (i=0;i<Nbe2;i++) 
    be2[i].clear();

  ee3->clear();
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::initHisto(){
  //  assert(fd);
  //assert(fd->IsOpen());
  //TDirectory *dir=  fd->mkdir(myName);

  int iJ;
  char tt1[100], tt2[100];

  //printf("EEdsmAna initHisto");
  
  // .................level-0 input, 2D, HT ..............
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    sprintf(tt1,"dsm0inJP%d_HT",iJ+1);
    sprintf(tt2,"HT input, DSM layer-0, Jet Patch %d (Steve), trig=%s",iJ+1,myName.Data());
    TH2F*h2=new TH2F(tt1,tt2,64,-0.5,63.5,EEnTPinJP,0.5,EEnTPinJP+0.5);
    h2->GetXaxis()->SetTitle("input 0-DSM ADC value");
    h2->GetYaxis()->SetTitle("Trig Patch  ID");
  
    H0inHT[iJ]=h2;
    HList->Add(h2); 
  }

  sprintf(tt2,"HT input, DSM layer-0, all trig Patches, trig=%s; Hanks ID=10*brd+inpID",myName.Data());
  H0inHTall=new TH2F("dsm0inJPall_HT",tt2,90,-0.5,89.5,64,-0.5,63.5);
  
  HList->Add( H0inHTall);

  // ........level-0 input, 2D, TP sum
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    char tt1[100], tt2[100];
    sprintf(tt1,"dsm0inJP%d_TP",iJ+1);
    sprintf(tt2,"TP input, DSM layer-0, Jet Patch %d (Steve), trig=%s",iJ+1,myName.Data());
    TH2F*h2=new TH2F(tt1,tt2,64,-0.5,63.5,EEnTPinJP,0.5,EEnTPinJP+0.5);
    h2->GetXaxis()->SetTitle("input to level 0");
    h2->GetYaxis()->SetTitle("Trig Patch  ID");
    
    H0inTP[iJ]=h2;
    HList->Add(h2);
  }
  
  sprintf(tt2,"TP input, DSM layer-0, all trig Patches, trig=%s",myName.Data());
  H0inTPall=new TH2F("dsm0inJPall_TP",tt2,90,-0.5,89.5,64,-0.5,63.5);

  HList->Add( H0inTPall);
  

  // .................level-1 input, 2D
  //================================ TP 

  for(iJ=0;iJ<EEnHalfJetPatch;iJ++) {
    sprintf(tt1,"dsm1HJP%d_TP",iJ+1);
    sprintf(tt2,"TP input vs. emulated, DSM layer-1, brd=%d  ch=%d, trig=%s",iJ/6,iJ%6,myName.Data());
    //printf("iJ=%d tit=%s\n",iJ,tt1);
    TH2F*h2=new TH2F(tt1,tt2,40,0.,400,40,0,400);
    h2->GetXaxis()->SetTitle("emulated TP from level-0");
    h2->GetYaxis()->SetTitle("input to level-1");
    H1inTPvEmu[iJ]=h2;
    HList->Add(h2); 
  }


  //================================ HT 
  for(iJ=0;iJ<EEnHalfJetPatch;iJ++) {
    sprintf(tt1,"dsm1HJP%d_HT",iJ+1);
    sprintf(tt2,"HT input vs.emu, DSM-1, Half Patch %d (Falk), trig=%s",iJ+1,myName.Data());
    // printf("iJ=%d tit=%s\n",iJ,tt1);
    TH2F*h2=new TH2F(tt1,tt2,64,-0.5,63.5,4,-0.5,3.5);
    h2->GetXaxis()->SetTitle("emulated HT from level-0");
    h2->GetYaxis()->SetTitle("input to level-1");
    H1inHTvEmu[iJ]=h2;
    HList->Add(h2); 
  }

  // .................level-2 input

  //================================ HT 
  for(iJ=0;iJ<EEnHalf;iJ++) {
    sprintf(tt1,"dsm2Half%d_HTTP",iJ+1);
    sprintf(tt2,"TP (+) HTTP thres, JP_Falk(%d+%d+%d), trig=%s",3*iJ,3*iJ+1,3*iJ+2,myName.Data());
    //printf("iJ=%d tit=%s\n",iJ,tt1);
    TH2F*h2=new TH2F(tt1,tt2,16,-0.5,15.5,4,-0.5,3.5);
    h2->GetXaxis()->SetTitle("emu layer1 out: maxTPthr*4 + maxHTTPthr");
    h2->GetYaxis()->SetTitle("layer2 inp: TPthr*2 + HTTP");
    H2inHTTP[iJ]=h2;
    HList->Add(h2); 
  }

  //================================ half Etot sum
  for(iJ=0;iJ<EEnHalf;iJ++) {
    sprintf(tt1,"dsm2Half%d_Etot",iJ+1);
    sprintf(tt2,"Etot for JP_Falk(%d+%d+%d), trig=%s; level2 input",3*iJ,3*iJ+1,3*iJ+2,myName.Data());
    TH1F*h1=new TH1F(tt1,tt2,32,-0.5,31.5);
    H1inEtot[iJ]=h1;
    HList->Add(h1); 
  }


  // Total energy histos
  for(iJ=0;iJ<mxEtotBit;iJ++) {
    sprintf(tt1,"dsm2E_etot%d",iJ);
    sprintf(tt2,"EEMC ETOT BIT=%d, trig=%s;  EEMC ETOT emul sum over 1 brd @ level2",iJ,myName.Data());
    //printf("%s=%s=\n",tt1,tt2);
    TH1F *h=new TH1F(tt1,tt2,48,15.5,63.5);
    h->SetLineColor(kBlue);
    if(iJ==1) h->SetLineColor(kRed);
    HEetot[iJ]=h;
    HList->Add( h);


    sprintf(tt1,"dsm2B_etot%d",iJ);
    sprintf(tt2,"BEMC ETOT BIT=%d, trig=%s;  BEMC ETOT emul sum over 3 brds @ level2",iJ,myName.Data());
    //printf("%s=%s=\n",tt1,tt2);
    h=new TH1F(tt1,tt2,190,65.5,255.5);
   h->SetLineColor(kBlue);
    if(iJ==1) h->SetLineColor(kRed);
    HBetot[iJ]=h;
    HList->Add( h);

    sprintf(tt1,"dsm2BE_etot%d",iJ);
    sprintf(tt2,"B+EEMC ETOT BIT=%d, trig=%s;  B+EEMC ETOT emul sum over 4 brds @ level2",iJ,myName.Data());
    // printf("%s=%s=\n",tt1,tt2);
    h=new TH1F(tt1,tt2,190,65.5,255.5);
    h->SetLineColor(kBlue);
    if(iJ==1) h->SetLineColor(kRed);
    HBEetot[iJ]=h;
    HList->Add( h);
  } 

  // .................level-3 input
  //================================ HT 
  sprintf(tt2,"HTTP  layer=3, trig=%s",myName.Data());
  TH2F*h2=new TH2F("dsm3_HTTP",tt2,4,-0.5,3.5,4,-0.5,3.5);
  h2->GetXaxis()->SetTitle("emu layer2 out:  orTPbit*2 + orHTTPbit");
  h2->GetYaxis()->SetTitle("layer3 inp: TPbit*2 + HTTPbit");
  H3inHTTP=h2;
  HList->Add(h2); 
 
  //..................Jet Patch sums
  //=================================summed patch spectra
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    int steve_jp=((iJ+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    sprintf(tt1,"JP%d_sum",steve_jp);
    sprintf(tt2,"Emulated Sum Jet Patch %d (DSMin %d)",((iJ+2)%EEnJetPatch)+1,iJ);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH1F*h1=new TH1F(tt1,tt2,300,-.5,299.5);
    h1->GetXaxis()->SetTitle("Jet Patch Emu Sum");
    h1->GetYaxis()->SetTitle("Freq");
    H4jpSums[iJ]=h1;
    HList->Add(h1);
  }

  //=================================Jet patch over threshold
  mJPthr[0]=16;
  mJPthr[1]=24;
  mJPthr[2]=30;
  mJPthr[3]=40;
  for(iJ=0;iJ<EEnThresh;iJ++) {
    sprintf(tt1,"JPsumTh%d",iJ);
    if (iJ == 0) sprintf(tt2,"Emu Jet Patch sum > %d",mJPthr[0]);
    else if( iJ==1 || iJ==2) 
      sprintf(tt2,"Emu Jet Patch sum in [%d,%d]",mJPthr[iJ],mJPthr[iJ+1]-1);
    else  
      sprintf(tt2,"Emu Jet Patch sum > %d",mJPthr[iJ]);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH1F*h1=new TH1F(tt1,tt2,6,0.5,6.5);
    h1->GetXaxis()->SetTitle("Steve's Jet Patch ID");
    h1->GetYaxis()->SetTitle("Freq"); 
    h1->SetFillColor(kBlue);
    H4jpFreq[iJ]=h1;
    HList->Add(h1);
  }

  //=================================summed adjacent patch spectra
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    int steve_jp=((iJ+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    int steve_jp2=((iJ+3)%EEnJetPatch)+1;
    sprintf(tt1,"JP%d%d_sum",steve_jp,steve_jp2);
    sprintf(tt2,"Emulated Sum Jet Patches %d+%d",steve_jp,steve_jp2);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH1F*h1=new TH1F(tt1,tt2,150,-.5,149.5);
    h1->GetXaxis()->SetTitle("Jet Patch Emu Sum");
    h1->GetYaxis()->SetTitle("Freq");
    H4adjpSums[iJ]=h1;
    HList->Add(h1);
  }

 //=================================adjacent patch correlation
  for(iJ=0;iJ<EEnJetPatch;iJ++) {
    int steve_jp=((iJ+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    int steve_jp2=((iJ+3)%EEnJetPatch)+1;
    sprintf(tt1,"JP%d%d_cor",steve_jp,steve_jp2);
    sprintf(tt2,"Emulated Sum Jet Patches %d vs %d",steve_jp,steve_jp2);
    //printf("iJ=%d name=%s tit=%s\n",iJ,tt1,tt2);
    TH2F*h2=new TH2F(tt1,tt2,60,9.5,69.5,60,9.5,69.5);
    sprintf(tt1,"Jet Patch %d Sum",steve_jp);
    h2->GetXaxis()->SetTitle(tt1);
    sprintf(tt1,"Jet Patch %d Sum",steve_jp2);
    h2->GetYaxis()->SetTitle(tt1);
    H4adjPcor[iJ]=h2;
    HList->Add(h2);
  }

  { //====================freq summed adjacent patch spectra over thresh
    TH1F*h1=new TH1F("JPadjTh","Adjacent Jet patches both over thr0",8,-.5,7.5);
    h1->GetXaxis()->SetTitle("DSM Adjacent Jet Patch Emu Sum (FEE crate add 2)");
    h1->GetYaxis()->SetTitle("Freq");
    H4adjpFreq=h1;
    HList->Add(h1);
  }

  // added in 2005 
  H5jpPed =new TH2F("JPpedZoom","Zoom in of 1x1 JP pedestals; Steve's JP ID",EEnJetPatch,0.5,EEnJetPatch+0.5,41,4.5,45.5); //2005: Y:40,34.5,74.5
  HList->Add(H5jpPed); 

  sprintf(tt2,"JP DSM sum > %d; Steve's JP ID", mJPthr[1]);
  H5jpFreq =new TH1F("JPtotFreq",tt2,EEnJetPatch,0.5,EEnJetPatch+0.5); 
  H5jpFreq->SetFillColor(kGreen);
  HList->Add(H5jpFreq);

  H5jpHot =new TH1F("JPpedHot","Hot towers ; Steve's JP ID",EEnJetPatch,0.5,EEnJetPatch+0.5); 
  H5jpHot->SetFillColor(kRed); // this histo is updated in presenter - a bit tricky
  HList->Add(H5jpHot); // is _not_ filled by sorter

  printf("EEdsmAna(%s)::initHisto() done  \n", myName.Data());
 
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::sort( const unsigned char * dsm0inp, 
		      const unsigned short int  * dsm1inp,
		      const unsigned short int  * dsm2inp,   
		      const unsigned short int  * dsm3inp){
  
  nTot++;
  // printf("EEdsmAna:: sort()  ,nTot=%d\n",nTot);

  readDsm0(dsm0inp);
  // print();
  readDsm1(dsm1inp);
  if(dsm2inp) readDsm2(dsm2inp);
  if(dsm3inp) readDsm3(dsm3inp);

  emulDsm0();    
  emulDsm1();    
  emulDsm2();    
  
  histoDsm0();
  histoDsm1();
  histoDsm2();
  histoDsm3();

  return;

  printf("==================================\n");
  printf("==================================\n");
  printAllEndcap();
  //printAllBarrel();
  //  assert(2==3);

#if 0
  // test for Steve:
  int iJ;
  for(iJ=0;iJ<EEnHalf;iJ++) {
    if(ee1outTPthrMax[iJ]>=ee1outHTTPthrMax[iJ]) continue;
    printf("NEW_EVE==================================\n ee1outTPthrMax[%d]=%d <ee1outHTTPthrMax[%d]=%d\n",iJ,ee1outTPthrMax[iJ],iJ,ee1outHTTPthrMax[iJ]);
    goto dump;
  }
#endif

}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::emulDsm0(){
  
  int i,j;
  // printf("emulDsm0 (%s) \n      ",myName.Data());
  for(i=0;i<EEnTPphi; i++) {
     for(j=0;j< EEnTPeta;j++) {
       EEmapTP *y=&eeMapTP[i][j];
       int ibr=y->brdIn-1;
       int ch=y->chIn;
       int chOut=y->chOut;
       EEdsm0 * br=ee0+ibr;
       // printf(" ibr=%d ch=%d chOut=%d  val=%d sum=%d\n",ibr,ch,chOut,ee0[ibr].getTP(ch), ee0out[chOut]);
       
       // ........ TP ..........
       ee0outTPadc[chOut]+=br->getTP(ch);

       //.......... HT ..............
       int val= br->getHT(ch);
       if(ee0outHTadc[chOut]< val)ee0outHTadc[chOut]=val;
     }
  }
  
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::emulDsm1(){
  
  int i,j;
  for (i=0;i<Nee1;i++) {
    EEdsm1 *br=ee1+i;           //Select data for board 1 or 2
    int maxTPthr=0;
    int maxHTTPthr=0;
    for(j=0;j<br->getNc(); j++) {
      int ix=j+i*br->getNc();
      int ijp=ix/2;
      ee1outJPadc[ijp]+=br->getTPsum(j);  /*Note jet patches are numbered
					    in DSM input order.  So ijp=0
					    is FEE crate 3.  */
      ee1out3JPadc[i]+=br->getTPsum(j);
      int val= br->getHTthr(j);
      if(ee1outHT[ijp]< val)ee1outHT[ijp]=val;
      if(maxTPthr< br->getTPthr(j)) maxTPthr= br->getTPthr(j);
      if(maxHTTPthr< br->getHTTPthr(j)) maxHTTPthr= br->getHTTPthr(j);
    }
    ee1outTPthrMax[i]  =maxTPthr;
    ee1outHTTPthrMax[i]=maxHTTPthr;
  }

  //Add some further processing to check jet patch sums for passing thresholds
  //and form adjacent patch sums.
  for (i=0;i<EEnJetPatch;i++) {
    AdjJPsum[i]=ee1outJPadc[i]+ee1outJPadc[(i+1)%EEnJetPatch]; //sum adj patches
    //printf("patch=%d  adc1=%d adc2=%d sum=%d  \n",i,ee1outJPadc[i],ee1outJPadc[(i+1)%EEnJetPatch],AdjJPsum[i]);
  }
  //printf("done processing jet patch sums \n");
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::emulDsm2(){
  
  ee2outHT=ee2->get3JPHTthr(0);
  if(ee2outHT<ee2->get3JPHTthr(1))ee2outHT=ee2->get3JPHTthr(1);

}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm0(const unsigned char *EEMC){
  
  /*Gets level 0 DSM inputs and reorders them.  They
    are stored 7 -> 0 followed by 15 -> 8.  Output is
    ordered as physically input to boards, 0 -> 15.
  */

  int ibr; // index, boards ID= ibr+1

  //--------------------- initialize input to DSM level-0
  for(ibr=0;ibr<Nee0; ibr++) {
    int k=16*ibr; // begin of data for given board
    int i;
    // fill in lower 8 words [ 0,1...,7] 
    for(i=0;i<8;i++) ee0[ibr].setBite(7-i,EEMC[k+i]);
    
    // fill in higher 8 words [8,9,....,15]
    for(i=8;i<16;i++) ee0[ibr].setBite(23-i,EEMC[k+i]);
  }
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm1(const  unsigned short *EEMC_l1){

  /* Gets level 1 DSM inputs (output of level 0) and
     reorders them.  They are stored 3 ->0 followed by
     7 -> 4 followed by 11 -> 8 and 15 -> 12.  Only 0-5
     and 8-13 are used, the first group for the first 
     board.  Output is ordered as physically input to
     the board.  For first board the order is crate 3,
     4 then 5, and for second board crate 6, 1 then 2.
  */

  int i;
  for(i=0;i<Nee1;i++) {
    ee1[i].setWord(0,EEMC_l1[3+i*8]);
    ee1[i].setWord(1,EEMC_l1[2+i*8]);
    ee1[i].setWord(2,EEMC_l1[1+i*8]);
    ee1[i].setWord(3,EEMC_l1[0+i*8]);
    ee1[i].setWord(4,EEMC_l1[7+i*8]);
    ee1[i].setWord(5,EEMC_l1[6+i*8]);
  }
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm2( const unsigned short *EMC){

  //Endcap
  ee2->setWord(0,EMC[5]);
  ee2->setWord(1,EMC[4]);

  //Barrel
  be2[0].setWord(0,EMC[3]);
  be2[0].setWord(1,EMC[2]);
  be2[1].setWord(0,EMC[1]);
  be2[1].setWord(1,EMC[0]);
  be2[2].setWord(0,EMC[7]);
  be2[2].setWord(1,EMC[6]);
  
}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::readDsm3( const unsigned short *lastDSM){
  ee3->setWord(0,lastDSM[0]);

}


//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm0(){
  int i,j;
  for(i=0;i<EEnTPphi; i++) {
    for(j=0;j< EEnTPeta;j++) {
      EEmapTP *y=&eeMapTP[i][j];
      int ibr=y->brdIn-1;
      int ch=y->chIn;
      int iJP=y->JPid-1;
      assert(iJP>=0 && iJP<EEnJetPatch);
      H0inHT[iJP]->Fill( ee0[ibr].getHT(ch),y->TPid);
      H0inTP[iJP]->Fill( ee0[ibr].getTP(ch),y->TPid);

      H0inHTall->Fill( 10*ibr+ch,ee0[ibr].getHT(ch));
      H0inTPall->Fill( 10*ibr+ch,ee0[ibr].getTP(ch));

      // printf("fill JP=%d TP=%d ADC=%d\n", y->JPid,y->TPid, ee0[ibr].getTP(ch));
    }
  }

}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm1(){

  int i,j;
  for (i=0;i<Nee1;i++) {
    EEdsm1 *br=ee1+i;
    for(j=0;j<br->getNc(); j++) {
      int ix=j+i*br->getNc();
      // .... TP ..................
      H1inTPvEmu[ix]->Fill(ee0outTPadc[ix],br->getTPsum(j));
      //..... HT ............
      H1inHTvEmu[ix]->Fill(ee0outHTadc[ix],br->getHTthr(j));
    }
  }

  /* 2006
     6) For the upper 4 plots ( H4jpFreq[i]), change the JP sum cuts to:
     JP sum > 16
     JP sum in range [24,29]
     JP sum in range [30,39]
     JP sum > 40
  */


  //printf("done with dsm1 histos on to jet patch histos \n");
  for (i=0;i<EEnJetPatch;i++) {     //histos for jet patch sums
    int steve_jp=((i+2)%EEnJetPatch)+1;  //stored by dsm input number 0-5 Want by Steves jp #
    H4jpSums[i]->Fill(ee1outJPadc[i]);
    int JPene=ee1outJPadc[i];
    if(JPene>mJPthr[0])  H4jpFreq[0]->Fill(steve_jp);
    if(JPene>mJPthr[3])  H4jpFreq[3]->Fill(steve_jp);
    else if(JPene>mJPthr[2])   H4jpFreq[2]->Fill(steve_jp);
    else if(JPene>mJPthr[1])   H4jpFreq[1]->Fill(steve_jp);
    
    H5jpPed ->Fill(steve_jp,ee1outJPadc[i]);
    if(ee1outJPadc[i]>mJPthr[1])  H5jpFreq ->Fill(steve_jp); 
    j=(i+1)%EEnJetPatch;
    //printf("histog i=%d j=%d  \n",i,j);
    H4adjPcor[i]->Fill(ee1outJPadc[i],ee1outJPadc[j]);
    H4adjpSums[i]->Fill(AdjJPsum[i]);

    if(ee1outJPadc[i]>mJPthr[1] && ee1outJPadc[(i+1)%EEnJetPatch] >mJPthr[1] )
      H4adjpFreq->Fill(steve_jp);
  }
}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm2(){

  //------------ HT ---------------
  int iJ;
  for(iJ=0;iJ<EEnHalf;iJ++) {
    int max=0;
    int i;
    // .... HT ..................
    for(i=3*iJ; i<3*iJ+3; i++) 
      if(max<ee1outHT[i]) max=ee1outHT[i];
    //tmp H2inHTvEmu[iJ]->Fill(max,ee2->get3JPHTthr(iJ));
  }

  //------------ TP & HTTP since 2006 ---------------
  /*
    4) The upper 2 plots need to be completely redefined. The result will be interpretable by me 
    and probably by few others until I train them, but it's not easy to do better unless we have 
    access to the register values that tell DSM exactly which thresholds we are choosing from TP
    and HT x TP bits. We want to change both histograms to be 16 channels in x by 4 channels in y. 
    The left-hand plot should still deal with the layer 1 DSM that Falk labels for JP
    0+1+2, and the right-hand plot for JP3+4+5.

    To form the 4-bit word for x in each of the above histograms, do the following:
    a) loop over the 6 inputs to the layer 1 DSM and extract bits 12-13; place the highest of 
    these 6 2-bit values into the upper 2 bits of the address for x.
    b) repeat (a), but now for bits 14-15 of the 6 inputs; place the highest of these 6 2-bit 
    values into the lower 2 bits of the 4-bit address for x.
    x thus measures: (TP bits for highest threshold passed)*4 + (HTxTP bits for highest threshold 
    passed)

    For the y-value in these histograms, form a 2-bit word with bit 1=bit 9 of the corresponding
    layer 2 DSM input word bit 0=bit 7 of the corresponding layer 2 DSM input word (all bits 
    numbered starting from 0).
    y thus measures: (selected TP threshold passed?Y or N)*2
    +(selected HT x TP threshold passed?Y or N)
    
  */

  for(iJ=0;iJ<EEnHalf;iJ++) {
    int x=ee1outTPthrMax[iJ]*4 + ee1outHTTPthrMax[iJ];
    int y=ee2->getTPthr(iJ)*2 + ee2->getHTTPthr(iJ);
    H2inHTTP[iJ]->Fill(x,y);
 }

  /* in 2006+
    2 1D plots, one for each layer 1 DSM (i.e., Falk JP0+1+2 and Falk JP3+4+5).
    The x-axis should be 32 channels and represent the value extracted from bits 0-4 of
    the corresponding EEMC input to the layer 2 DSM. The plots should be labeled:
    ETOT FOR JP0+1+2 and ETOT FOR JP3+4+5.
  */

  for(iJ=0;iJ<EEnHalf;iJ++) H1inEtot[iJ]->Fill(ee2->get3JPsum(iJ));


  //Total energy histos
  /*
    64 channels x by 2 channels y :
    x-axis (label EEMC ETOT emulated) ; value = sum over
    2 EEMC inputs to layer 2 DSM the values in bits 0-4 for each input.
    
    y-axis (label EEMC ETOT BIT): value = bit 11 of layer
    2 DSM output (or of TCU input).
  */
  ushort sumE=ee2->get3JPsum(0)+ee2->get3JPsum(1);
  HEetot[ee3->getEtbitE()]->Fill(sumE);
  
  //BARREL histos 2006
  /*
    256 channels x by 2 channels y:
    x-axis (label BEMC ETOT emulated) ; value = sum over
    6 BEMC inputs to layer 2 DSM the values in bits 0-4 for each input.
    
    y-axis (label BEMC ETOT BIT): value = bit 4 of layer
    2 DSM output (or of TCU input).
  */

   ushort  sumB=0;
  int i;
  for(i=0; i<Nbe2; i++) {
    sumB+=be2[i].get3JPsum(0);
    sumB+=be2[i].get3JPsum(1);
  }
  HBetot[ee3->getEtbitB()]->Fill(sumB);

  /*
    256 channels x by 2 channels y:
    x-axis (label B+EEMC ETOT emulated) ; value = sum over
    6 BEMC+2 EEMC inputs to layer 2 DSM the values in bits 0-4 for each input.
    
    y-axis (label B+EEMC ETOT BIT): value = bit 15 of layer
    2 DSM output (or of TCU input).
  */

  HBEetot[ee3->getEtbitBE()]->Fill(sumB+sumE);  
}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna::histoDsm3(){

/*
 The plot should be replaced with a 4 x 4 histo:
x-axis = 2-bit word constructed from:
bit 0=OR of bit 7 on two EEMC inputs to layer 2 DSM bit 1=OR of bit 9 on two EEMC inputs to layer 2 DSM y-axis = 2-bit word constructed from:
bit 0=bit 12 of layer 2 DSM output (or TCU input) bit 1=bit 14 of layer 2 DSM output (or TCU input)

This histogram should have the "normal" appearance of a diagonal band; the x-axis is the emulated TP and HT x TP inputs to layer 2, and the y-axis is the actual output from layer 2.

 */

  bool x_b0=ee1outTPthrMax[0] ||ee1outTPthrMax[1];
  bool x_b1=ee1outHTTPthrMax[0] ||ee1outHTTPthrMax[1];
  int x=x_b1*2 + x_b0; 
  int y=ee3->getHTTPbit()*2 +ee3->getTPbit();
  H3inHTTP->Fill(x,y);
}

//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::printDsm0map(){

  printf("TP Map level-0 \n      ");

  int i,j;
  for(j=0;j< EEnTPeta;j++)  
    printf("        iEta=%d        ",j);

  printf("\n              ");
  for(j=0;j< EEnTPeta;j++)  
    printf("JP/TP brd:ch->outCh   ");


  for(i=0;i<EEnTPphi; i++) {
    int phi=111-i*12;
    if(phi<0) phi+=360;
    printf("\nPhi/deg=%3d   ",phi);
    for(j=0;j< EEnTPeta;j++) {
       EEmapTP *y=&eeMapTP[i][j];
      printf("%d/%2d %2d:%2d -> %2d      ",y->JPid,y->TPid,y->brdIn,y->chIn,y->chOut);
    }
  }
  printf("\n");
}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna :: printAllEndcap( int k) {
  printf("\n\nEEdsmAna(%s)::print(eve=%d) , year=%d \n",myName.Data(),nTot,mYear);

  int i;
  for(i=0;i<Nee0;i++) {
    printf("\n----------- level-0 Board %2d ",i+1); 
    ee0[i].print();
  }
 
  printf("\n----------- level-0 emulated output \n ch =");
  for(i=Nee0out-1;i>=0; i--) printf("  %4d ",i);
  printf("\n TP =");
  for(i=Nee0out-1;i>=0; i--) printf("  %4d ",ee0outTPadc[i]);
  printf("\nHTadc =");
  for(i=Nee0out-1;i>=0; i--) printf("  %4d ",ee0outHTadc[i]);

  printf("\n\n----------- level-1 -----------------\n "); 

  for(i=0;i<Nee1;i++) {
    printf("\n----------- level-1 Board %2d , ",i+1); 
    ee1[i].print();
    printf("emul out 3x.9 JP_Falk(%d+%d+%d) energy/dec: 13bit=%d  8bit=%d  mxTPthr=%d mxHTTPthr=%d\n",3*i,3*i+1,3*i+2,ee1out3JPadc[i],ee1out3JPadc[i]>>5, ee1outTPthrMax[i], ee1outHTTPthrMax[i]);
  }

  printf("\n----------- level-1 emulated output \n JP_Falk =");
  int Njp=EEnJetPatch;
  for(i=Njp-1;i>=0; i--) printf("  %4d ",i);
  printf("\n JP_Steve=");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",(i+2)%6+1);
  printf("\n JPsum   =");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",ee1outJPadc[i]);
  printf("\n HTthr   =");
  for(i=Njp-1;i>=0; i--) printf("  %4d ",ee1outHT[i]);
  if(mYear<2006) {
    printf("\n AdjJPsum=");
    for(i=Njp-1;i>=0; i--) printf("  %4d ",AdjJPsum[i]);
  }
  printf("\n");
  printf("emul: orTPbit=%d  orHTTPbit=%d \n",ee1outTPthrMax[0]||ee1outTPthrMax[1], ee1outHTTPthrMax[0] ||ee1outHTTPthrMax[1]);

  printf("\n\n----------- level-2 ----------------- \n"); 
  ee2->print();


  printf("\n\n----------- level-3 ( aka TCU) ----------- \n"); 
  ee3->print();

}




//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna :: printAllBarrel( int k) {
  printf("\n\nBEdsmAna(%s)::print(eve=%d) , year=%d \n",myName.Data(),nTot,mYear);

  int i;
  for(i=0;i<Nbe2;i++) {
    printf("\n----------- level-2 BARREL Board %2d ",i+1); 
    be2[i].print();
  }
  printf("\n\n");
}



//--------------------------------------------------
//--------------------------------------------------
void EEdsmAna ::usePed( TString fName){
  //......................... load ped for L-0
  printf("EEdsmAna(%s) reads pedestals from %s\n",myName.Data(),fName.Data());

  FILE *fd=fopen(fName.Data(),"r");
  assert(fd);
  int ibr;
  for(ibr=0;ibr<Nee0; ibr++) {
    int ch;
    for(ch=0;ch<ee0[ibr].getNc();ch++) {
      int a,b,c;
      int ret=fscanf(fd,"%d %d %d",&a,&b,&c);
      assert(ret==3);
      assert(a==ibr+1);
      assert(b==ch);
      ped0[ibr][ch]=c;
    }
  } 
}
#else 
  void EEdsmAna::readDsm0( const unsigned char *){}
  void EEdsmAna::readDsm1( const unsigned short *){}
  void EEdsmAna::readDsm2( const unsigned short *){}
  void EEdsmAna::readDsm3( const unsigned short *){}
  void EEdsmAna::emulDsm0(){}
  void EEdsmAna::emulDsm1(){}
  void EEdsmAna::emulDsm2(){}
  void EEdsmAna::histoDsm0(){}
  void EEdsmAna::histoDsm1(){}
  void EEdsmAna::histoDsm2(){}
  void EEdsmAna::histoDsm3(){}
 EEdsmAna::EEdsmAna( TObjArray *L,TString n){}
 void  EEdsmAna::printDsm0map(){}

 EEdsmAna::~EEdsmAna(){}
 void  EEdsmAna::printAllEndcap(int k){}
 void  EEdsmAna::printAllBarrel(int k){}
 void  EEdsmAna::initHisto(){}
 void  EEdsmAna::clear(){}
 void  EEdsmAna::sort(const unsigned char * dsm0inp, 
	    const unsigned short int  * dsm1inp ,
	    const unsigned short int  * dsm2inp, 
	    const unsigned short int  * dsm3inp){}
       void  usePed( TString n){}

#endif
