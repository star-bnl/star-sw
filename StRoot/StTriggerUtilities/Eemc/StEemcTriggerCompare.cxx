#include <TH2.h>
#include <vector>
#include "StEemcTriggerSimu.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h" // for triggerID

#include "EEfeeTPTree.h" 
#include "EEfeeTP.h" 
#include "EEdsm0Tree.h"
#include "EEdsm1Tree.h"
#include "EMCdsm2Tree.h"
#include "EEdsm3.h"


//==================================================
//==================================================
void 
StEemcTriggerSimu::compareADCfee_TRG0(){
  
  // compare DSM0 inputs:  ADCemul vs. TRGreal
  int i;
  for(i=0;i<EEfeeTPTree::mxTP;i++) {
    EEfeeTP *feeTP=feeTPTreeADC->TP(i);

    
    int trgHT=dsm0TreeTRG->getInpHT6bit(i);
    int trgTPsum=dsm0TreeTRG->getInpTP6bit(i);

    int delHT=feeTP->getOutHT()-trgHT;
    int delTPsum=feeTP->getOutTPsum()-trgTPsum;
    bool isBad=false;

    //.... HT ..............
    hA[10]->Fill(i,trgHT);
    hA[11]->Fill(i,feeTP->getOutHT());
    if(delHT!=0) {
      hA[12]->Fill(i,delHT);
      hA[13]->Fill(i);
      isBad=true;
      // printf("\nBAD HT6bit eve=%d HanksCh=%d  emuAdc=%d inpTRG=%d del=%d\n ", nInpEve, i, feeTP->getHT(), trgHT, delHT);
      
    }

    //.... TPsum ..............  
    hA[20]->Fill(i,trgTPsum);
    hA[21]->Fill(i,feeTP->getOutTPsum());
    if(delTPsum!=0) {
      hA[22]->Fill(i,delTPsum);
      hA[23]->Fill(i);
      isBad=true;
      //  printf("\nBAD TP6bit eve=%d HanksCh=%d  emuAdc=%d inpTRG=%d del=%d\n ", nInpEve, i, feeTP->getTP(), trgTP, delTP);
    }
  
    //  if(isBad ) feeTP->print();
    
  }

}


//==================================================
//==================================================
void 
StEemcTriggerSimu::compareTRG0_TRG1(){
  
  int ch;
  //  printf("eveID=%d ............ verify dsm0Tree math for TRG data only............\n",eveId);

  bool isGood=true;
  for(ch=0;ch<12;ch++) {
    
    int inpTrgTPsum=dsm1TreeTRG->getInpTPsum(ch);
    int emuTrgTPsum=dsm0TreeTRG->getOutTPsum(ch);
    
    hA[150]->Fill(ch, inpTrgTPsum);
    hA[151]->Fill(ch, emuTrgTPsum);

    if(inpTrgTPsum!=emuTrgTPsum) {// error found
      hA[152]->Fill(ch, emuTrgTPsum-inpTrgTPsum); 
      hA[157]->Fill(ch, emuTrgTPsum-inpTrgTPsum);
      hA[153]->Fill(ch);
      if(!mMCflag) printf("BaD TPsum(ch=%d)  out0=%d  inp1=%d   del=%d\n", ch, emuTrgTPsum, inpTrgTPsum, emuTrgTPsum-inpTrgTPsum );
      isGood=false;
    }
  
    int inpTrgHT=dsm1TreeTRG->getInpHT2bit(ch);
    int emuTrgHT=dsm0TreeTRG->getOutHT2bit(ch);

    hA[160]->Fill(ch, inpTrgHT);
    hA[161]->Fill(ch, emuTrgHT);

    if(inpTrgHT!=emuTrgHT) {// error found
      hA[162]->Fill(ch, emuTrgHT-inpTrgHT);
      hA[163]->Fill(ch);
      isGood=false;
      if(!mMCflag) printf("BaD HT2bit(ch=%d)  out0=%d  inp1=%d   del=%d\n",ch, emuTrgHT, inpTrgHT, emuTrgHT-inpTrgHT );
    }
    
    int inpTrgHTTP=dsm1TreeTRG->getInpHTTP2bit(ch);
    int emuTrgHTTP=dsm0TreeTRG->getOutHTTP2bit(ch);

    hA[170]->Fill(ch, inpTrgHTTP);
    hA[171]->Fill(ch, emuTrgHTTP);

    if(inpTrgHTTP!=emuTrgHTTP) {// error found
      hA[172]->Fill(ch, emuTrgHTTP-inpTrgHTTP);
      hA[173]->Fill(ch);
      isGood=false;
      if(!mMCflag) printf("BaD HTTP2bit(ch=%d)  out0=%d  inp1=%d   del=%d\n", ch, emuTrgHTTP, inpTrgHTTP, emuTrgHTTP-inpTrgHTTP );
    }

    
    int inpTrgTP=dsm1TreeTRG->getInpTP2bit(ch);
    int emuTrgTP=dsm0TreeTRG->getOutTP2bit(ch);

    hA[180]->Fill(ch, inpTrgTP);
    hA[181]->Fill(ch, emuTrgTP);

    if(inpTrgTP!=emuTrgTP) {// error found
      hA[182]->Fill(ch, emuTrgTP-inpTrgTP);
      hA[183]->Fill(ch);
      isGood=false;
      if(!mMCflag) printf("BaD TP2bit(ch=%d)  out0=%d  inp1=%d   del=%d\n", ch, emuTrgTP, inpTrgTP, emuTrgTP-inpTrgTP );
    }

    int inpTrg16bit=dsm1TreeTRG->getInp16bit(ch);
    int emuTrg16bit=dsm0TreeTRG->getOut16bit(ch);

    hA[190]->Fill(ch, inpTrg16bit);
    hA[191]->Fill(ch, emuTrg16bit);

    if(inpTrg16bit!=emuTrg16bit) {// error found
      hA[192]->Fill(ch, emuTrg16bit-inpTrg16bit);
      hA[193]->Fill(ch);
      isGood=false;
      if(!mMCflag) printf("BaD TP2bit(ch=%d)  out0=%d  inp1=%d   del=%d\n", ch, emuTrgTP, inpTrgTP, emuTrgTP-inpTrgTP );
    }
    
  }
  //loop over chan finished

  if(!isGood) {    
    dsm0TreeTRG->print();
    dsm1TreeTRG->print();
  }

}


//==================================================
//==================================================
void 
StEemcTriggerSimu::compareTRG1_TRG2(){

  //printf("............ verify dsm1Tree math for TRG data only............\n");
  // inputs to DSM2 board=0 (only Endcap)

  int ibr=0; //only Endcap
  int ch;
  int outJPdiff[2],outHTdiff[2],outHTTPdiff[2],outTPdiff[2];
  int inpJPdiff[2],inpHTdiff[2],inpHTTPdiff[2],inpTPdiff[2];

  for(ch=0;ch<2;ch++) { // loop over DSM1 boards

    int outE=dsm1TreeTRG->getOutEsum5bit(ch);
    int inpE=dsm2TreeTRG->getInpEsum5bit(ibr,ch);

  
    //printf("!!! Esum5bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outE,inpE,outE-inpE);

    // hA[500]->Fill(ch, inpE); // filled in compareADC1_TRG2()
    hA[501]->Fill(ch, outE);

    if(outE!=inpE) {
      dsm1TreeTRG->print();//tmp

      hA[502]->Fill(ch, outE-inpE);
      hA[503]->Fill(ch);

     if(!mMCflag)  printf("BaD 3JP Esum5bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outE,inpE,outE-inpE);
      // assert(400==i);
    }

    int outJP=dsm1TreeTRG->getOutJP2bit(ch);
    int inpJP=dsm2TreeTRG->getInpJP2bit(ibr,ch);

    outJPdiff[ch]=outJP;
    inpJPdiff[ch]=inpJP;
      

    //printf("!!! JP2bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outJP,inpJP,outJP-inpJP);

    hA[510]->Fill(ch, inpJP);
    hA[511]->Fill(ch, outJP);

    if(outJP!=inpJP) {

      dsm1TreeTRG->print();//tmp

      hA[512]->Fill(ch, outJP-inpJP);
      hA[513]->Fill(ch);

      if(!mMCflag) printf("BaD 3JP JP2bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outJP,inpJP,outJP-inpJP);
      // assert(500==i);
    } 
    
    int outHT=dsm1TreeTRG->getOutHT2bit(ch);
    int inpHT=dsm2TreeTRG->getInpHT2bit(ibr,ch);
    
    outHTdiff[ch]=outHT;
    inpHTdiff[ch]=inpHT;

    //printf("!!! HT2bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outHT,inpHT,outHT-inpHT);
    
    hA[510]->Fill(ch+3, inpHT);
    hA[511]->Fill(ch+3, outHT);

    if(outHT!=inpHT) {
      dsm1TreeTRG->print();//tmp

      hA[512]->Fill(ch+3, outHT-inpHT);
      hA[513]->Fill(ch+3);

      if(!mMCflag)  printf("BaD 3JP HT2bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outHT,inpHT,outHT-inpHT);
      // assert(100==i);
    }  

    int outHTTP=dsm1TreeTRG->getOutHTTP1bit(ch);
    int inpHTTP=dsm2TreeTRG->getInpHTTP1bit(ibr,ch);
    
    outHTTPdiff[ch]=outHTTP;
    inpHTTPdiff[ch]=inpHTTP;
    
    //printf("!!! HTTP1bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outHTTP,inpHTTP,outHTTP-inpHTTP);

    hA[510]->Fill(ch+6, inpHTTP);
    hA[511]->Fill(ch+6, outHTTP);

    if(outHTTP!=inpHTTP) {
      
      dsm1TreeTRG->print();//tmp

      hA[512]->Fill(ch+6, outHTTP-inpHTTP);
      hA[513]->Fill(ch+6);

     if(!mMCflag)  printf("BaD 3JP HTTP1bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outHTTP,inpHTTP,outHTTP-inpHTTP);
      // assert(300==i);
    } 
    
    int outTP=dsm1TreeTRG->getOutTP1bit(ch);
    int inpTP=dsm2TreeTRG->getInpTP1bit(ibr,ch);

    outTPdiff[ch]=outTP;
    inpTPdiff[ch]=inpTP;

    // printf("!!! TP1bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outTP,inpTP,outTP-inpTP);
 
    hA[510]->Fill(ch+9, inpTP);
    hA[511]->Fill(ch+9, outTP);

    if(outTP!=inpTP) {

      dsm1TreeTRG->print();//tmp

      hA[512]->Fill(ch+9, outTP-inpTP);
      hA[513]->Fill(ch+9);

      if(!mMCflag)  printf("BaD 3JP TP1bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outTP,inpTP,outTP-inpTP);
      //assert(200==i);
    } 

  }

  hA[520]->Fill(1,inpJPdiff[1]-inpJPdiff[0]);
  hA[520]->Fill(4,inpHTdiff[1]-inpHTdiff[0]);
  hA[520]->Fill(7,inpHTTPdiff[1]-inpHTTPdiff[0]);
  hA[520]->Fill(10,inpTPdiff[1]-inpTPdiff[0]);

  hA[521]->Fill(1,outJPdiff[1]-outJPdiff[0]);
  hA[521]->Fill(4,outHTdiff[1]-outHTdiff[0]);
  hA[521]->Fill(7,outHTTPdiff[1]-outHTTPdiff[0]);
  hA[521]->Fill(10,outTPdiff[1]-outTPdiff[0]);


}


//==================================================
//==================================================
void 
StEemcTriggerSimu::compareTRG2_TRG3(){

  int outEndcapJP2bit = dsm2TreeTRG->getOutEndcapJP2bit();
  int inpEndcapJP2bit = dsm3TRG->getEndcapJPthr2bit(); 
  int chan=0;
  
  hA[600]->Fill(chan,inpEndcapJP2bit);
  hA[601]->Fill(chan,outEndcapJP2bit);

  if( outEndcapJP2bit != inpEndcapJP2bit){
    printf("!!! BAD EndcapJP2bit out2 = %d, inp3 = %d, diff=%d\n", outEndcapJP2bit, inpEndcapJP2bit, outEndcapJP2bit-inpEndcapJP2bit);
    hA[602]->Fill(chan,inpEndcapJP2bit-outEndcapJP2bit);
    hA[603]->Fill(chan);
  }
 
  int outEndcapHT2bit = dsm2TreeTRG->getOutEndcapHT2bit();
  int inpEndcapHT2bit = dsm3TRG->getEndcapHTthr2bit();
  chan=1;
  
  hA[600]->Fill(chan,inpEndcapHT2bit);
  hA[601]->Fill(chan,outEndcapHT2bit);

  if( outEndcapHT2bit != inpEndcapHT2bit){
    printf("!!! BAD EndcapHT2bit out2 = %d, inp3 = %d, diff=%d\n", outEndcapHT2bit, inpEndcapHT2bit, outEndcapHT2bit-inpEndcapHT2bit);
    hA[602]->Fill(chan,inpEndcapHT2bit-outEndcapHT2bit);
    hA[603]->Fill(chan);
  }

  int outEndcapSum1bit = dsm2TreeTRG->getOutEndcapSum1bit();
  int inpEndcapSum1bit = dsm3TRG->getEndcapEsumthr1bit();
  chan=2;

  hA[600]->Fill(chan,inpEndcapSum1bit);
  hA[601]->Fill(chan,outEndcapSum1bit);
  

  if( outEndcapSum1bit != inpEndcapSum1bit){
    printf("!!! BAD EndcapSum1bit out2 = %d, inp3 = %d, diff=%d\n", outEndcapSum1bit, inpEndcapSum1bit, outEndcapSum1bit-inpEndcapSum1bit);
    hA[602]->Fill(chan,inpEndcapSum1bit-outEndcapSum1bit);
    hA[603]->Fill(chan);
  }

  int outEndcapHTTP1bit = dsm2TreeTRG->getOutEndcapHTTP1bit();
  int inpEndcapHTTP1bit = dsm3TRG->getEndcapHTTPthr1bit();
  chan=3;

  hA[600]->Fill(chan,inpEndcapHTTP1bit);
  hA[601]->Fill(chan,outEndcapHTTP1bit);

  if( outEndcapHTTP1bit != inpEndcapHTTP1bit){
    printf("!!! BAD EndcapHTTP1bit out2 = %d, inp3 = %d, diff=%d\n", outEndcapHTTP1bit, inpEndcapHTTP1bit, outEndcapHTTP1bit-inpEndcapHTTP1bit);
    hA[602]->Fill(chan,inpEndcapHTTP1bit-outEndcapHTTP1bit);
    hA[603]->Fill(chan);
  }

  int outEndcapTP1bit = dsm2TreeTRG->getOutEndcapTP1bit();
  int inpEndcapTP1bit = dsm3TRG->getEndcapTPthr1bit();
  chan=4;

  hA[600]->Fill(chan,inpEndcapTP1bit);
  hA[601]->Fill(chan,outEndcapTP1bit);
  
  if( outEndcapTP1bit != inpEndcapTP1bit){
    printf("!!! BAD EndcapTP1bit out2 = %d, inp3 = %d, diff=%d\n", outEndcapTP1bit, inpEndcapTP1bit, outEndcapTP1bit-inpEndcapTP1bit);
    hA[602]->Fill(chan,inpEndcapTP1bit-outEndcapTP1bit);
    hA[603]->Fill(chan);
  }

 
  int outBarreJP2bit = dsm2TreeTRG->getOutBarreJP2bit();
  int inpBarreJP2bit = dsm3TRG->getBarreJPthr2bit();
  int ch=0;

  hA[610]->Fill(ch,inpBarreJP2bit);
  hA[611]->Fill(ch,outBarreJP2bit);

  if( outBarreJP2bit != inpBarreJP2bit){
    printf("!!! BAD BarreJP2bit out2 = %d, inp3 = %d, diff=%d\n", outBarreJP2bit, inpBarreJP2bit, outBarreJP2bit-inpBarreJP2bit);
    hA[612]->Fill(ch,inpBarreJP2bit-outBarreJP2bit);
    hA[613]->Fill(ch);
  }

  int outBarreHT2bit = dsm2TreeTRG->getOutBarreHT2bit();
  int inpBarreHT2bit = dsm3TRG->getBarreHTthr2bit(); 
  ch=1;

  hA[610]->Fill(ch,inpBarreHT2bit);
  hA[611]->Fill(ch,outBarreHT2bit);

  if( outBarreHT2bit != inpBarreHT2bit){
    printf("!!! BAD BarreHT2bit out2 = %d, inp3 = %d, diff=%d\n", outBarreHT2bit, inpBarreHT2bit, outBarreHT2bit-inpBarreHT2bit);
    hA[612]->Fill(ch,inpBarreHT2bit-outBarreHT2bit);
    hA[613]->Fill(ch);
  }

  int outBarreSum1bit = dsm2TreeTRG->getOutBarreSum1bit();
  int inpBarreSum1bit = dsm3TRG->getBarreEsumThr1bit();
  ch=2;

  hA[610]->Fill(ch,inpBarreSum1bit);
  hA[611]->Fill(ch,outBarreSum1bit);

  if( outBarreSum1bit != inpBarreSum1bit){
    printf("!!! BAD BarreSum1bit out2 = %d, inp3 = %d, diff=%d\n", outBarreSum1bit, inpBarreSum1bit, outBarreSum1bit-inpBarreSum1bit);
    hA[612]->Fill(ch,inpBarreSum1bit-outBarreSum1bit);
    hA[613]->Fill(ch);
  }

  int outBarreHTTP1bit = dsm2TreeTRG->getOutBarreHTTP1bit();
  int inpBarreHTTP1bit = dsm3TRG->getBarreHTTPthr1bit();
  ch=3;

  hA[610]->Fill(ch,inpBarreHTTP1bit);
  hA[611]->Fill(ch,outBarreHTTP1bit);

  if( outBarreHTTP1bit != inpBarreHTTP1bit){
    printf("!!! BAD BarreHTTP1bit out2 = %d, inp3 = %d, diff=%d\n", outBarreHTTP1bit, inpBarreHTTP1bit, outBarreHTTP1bit-inpBarreHTTP1bit);
    hA[612]->Fill(ch,inpBarreHTTP1bit-outBarreHTTP1bit);
    hA[613]->Fill(ch);
  }

  int outBarreTP1bit = dsm2TreeTRG->getOutBarreTP1bit();
  int inpBarreTP1bit = dsm3TRG->getBarreTPthr1bit(); 
  ch=4;

  hA[610]->Fill(ch,inpBarreTP1bit);
  hA[611]->Fill(ch,outBarreTP1bit);

  if( outBarreTP1bit != inpBarreTP1bit){
    printf("!!! BAD BarreTP1bit out2 = %d, inp3 = %d, diff=%d\n", outBarreTP1bit, inpBarreTP1bit, outBarreTP1bit-inpBarreTP1bit);
    hA[612]->Fill(ch,inpBarreTP1bit-outBarreTP1bit);
    hA[613]->Fill(ch);
  }
#if 0
  int outBarreJPSi1bit = dsm2TreeTRG->getOutBarreJPSi1bit();
  int inpBarreJPSi1bit = dsm3TRG->getJpsi1bit();

  if( outBarreJPSi1bit != inpBarreJPSi1bit){
    printf("!!! BAD BarreJPSi1bit out2 = %d, inp3 = %d, diff=%d\n", outBarreJPSi1bit, inpBarreJPSi1bit, outBarreJPSi1bit-inpBarreJPSi1bit);
  }

#endif
  int outEtot1bit = dsm2TreeTRG->getOutEtot1bit();
  int inpEtot1bit = dsm3TRG->getEtotThr1bit();
  int cha=0;

  hA[620]->Fill(cha,inpEtot1bit);
  hA[621]->Fill(cha,outEtot1bit);

  if( outEtot1bit != outEtot1bit){
    printf("!!! BAD Etot1bit out2 = %d, inp3 = %d, diff=%d\n", outEtot1bit, inpEtot1bit, outEtot1bit-inpEtot1bit);
    hA[622]->Fill(cha,inpEtot1bit-outEtot1bit);
    hA[623]->Fill(cha);
  }

  int outBarreSum = dsm2TreeTRG->getIntBarreSum();
  if ( (outBarreSum<76) && (outBarreSum>64) ){
    hA[690]->Fill(cha, outBarreSum);
  }
}

//==================================================
//==================================================
void 
StEemcTriggerSimu::compareADC0_TRG1(){
 
  int ch;
 
  bool isGood=true;
  for(ch=0;ch<12;ch++) {

    int inpTrgTPsum=dsm1TreeTRG->getInpTPsum(ch);
    int emuTrgTPsum=dsm0TreeADC->getOutTPsum(ch);

    //hA[150]->Fill(ch, inpTrgTPsum);
    hA[154]->Fill(ch, emuTrgTPsum);

    if(inpTrgTPsum!=emuTrgTPsum) {// error found
      hA[155]->Fill(ch, emuTrgTPsum-inpTrgTPsum); 
      hA[158]->Fill(ch, emuTrgTPsum-inpTrgTPsum); 
      hA[156]->Fill(ch);

     if(!mMCflag)  printf("BaD TPsum(ch=%d)  emu-out0=%d  inp1=%d   del=%d\n", ch, emuTrgTPsum, inpTrgTPsum, emuTrgTPsum-inpTrgTPsum );
      isGood=false;
    }

    int inpTrgHT=dsm1TreeTRG->getInpHT2bit(ch);
    int emuTrgHT=dsm0TreeADC->getOutHT2bit(ch);

    //hA[160]->Fill(ch, inpTrgHT);
    hA[164]->Fill(ch, emuTrgHT);

    if(inpTrgHT!=emuTrgHT) {// error found
      hA[165]->Fill(ch, emuTrgHT-inpTrgHT);
      hA[166]->Fill(ch);

      isGood=false;
     if(!mMCflag)  printf("BaD HT2bit(ch=%d)  emu-out0=%d  inp1=%d   del=%d\n",ch, emuTrgHT, inpTrgHT, emuTrgHT-inpTrgHT );  
    }

    int inpTrgHTTP=dsm1TreeTRG->getInpHTTP2bit(ch);
    int emuTrgHTTP=dsm0TreeADC->getOutHTTP2bit(ch);

    //hA[170]->Fill(ch, inpTrgHTTP);
    hA[174]->Fill(ch, emuTrgHTTP);

    if(inpTrgHTTP!=emuTrgHTTP) {// error found
      hA[175]->Fill(ch, emuTrgHTTP-inpTrgHTTP);
      hA[176]->Fill(ch);

      isGood=false;
     if(!mMCflag)  printf("BaD HTTP2bit(ch=%d)  emu-out0=%d  inp1=%d   del=%d\n", ch, emuTrgHTTP, inpTrgHTTP, emuTrgHTTP-inpTrgHTTP );
    }

    int inpTrgTP=dsm1TreeTRG->getInpTP2bit(ch);
    int emuTrgTP=dsm0TreeADC->getOutTP2bit(ch);

    //hA[180]->Fill(ch, inpTrgTP);
    hA[184]->Fill(ch, emuTrgTP);

    if(inpTrgTP!=emuTrgTP) {// error found
      hA[185]->Fill(ch, emuTrgTP-inpTrgTP);
      hA[186]->Fill(ch);
      
      isGood=false;
      if(!mMCflag) printf("BaD TP2bit(ch=%d)  emu-out0=%d  inp1=%d   del=%d\n", ch, emuTrgTP, inpTrgTP, emuTrgTP-inpTrgTP );
    }

  }

  if(!isGood) {    
    //dsm0TreeADC->print();
    //dsm1TreeTRG->print();
    printf("!!!\n");
  }

}

//==================================================
//==================================================
void 
StEemcTriggerSimu::compareADC1_TRG2(){
  
  int ibr=0; //only Endcap
  int ch;
  int outJPdiff[2],outHTdiff[2],outHTTPdiff[2],outTPdiff[2];
  int inpJPdiff[2],inpHTdiff[2],inpHTTPdiff[2],inpTPdiff[2];
  
  for(ch=0;ch<2;ch++) { // loop over DSM1 boards
    
    int outJP=dsm1TreeADC->getOutJP2bit(ch);
    int inpJP=dsm2TreeTRG->getInpJP2bit(ibr,ch);
    
    outJPdiff[ch]=outJP;
    inpJPdiff[ch]=inpJP;
      
    
    //printf("!!! JP2bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outJP,inpJP,outJP-inpJP);
    
    //hA[510]->Fill(ch, inpJP);
    hA[514]->Fill(ch, outJP);
    
    if(outJP!=inpJP) {
      
      //dsm1TreeADC->print();//tmp
      //  printf("###\n");
      
      hA[515]->Fill(ch, outJP-inpJP);
      hA[516]->Fill(ch);
      
      if(!mMCflag)  printf("BaD 3JP JP2bit(ch=%d)  emu-out1=%d  inp2=%d   diff=%d\n",ch, outJP,inpJP,outJP-inpJP);
      // assert(500==i);
    } 
    
    int outHT=dsm1TreeADC->getOutHT2bit(ch);
    int inpHT=dsm2TreeTRG->getInpHT2bit(ibr,ch);
    
    outHTdiff[ch]=outHT;
    inpHTdiff[ch]=inpHT;
    
    //printf("!!! HT2bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outHT,inpHT,outHT-inpHT);
    
    //hA[510]->Fill(ch+3, inpHT);
    hA[514]->Fill(ch+3, outHT);
    
    if(outHT!=inpHT) {
      //dsm1TreeADC->print();//tmp
      // printf("###\n");
      
      hA[515]->Fill(ch+3, outHT-inpHT);
      hA[516]->Fill(ch+3);
      
      if(!mMCflag)  printf("BaD 3JP HT2bit(ch=%d)  emu-out1=%d  inp2=%d   diff=%d\n",ch, outHT,inpHT,outHT-inpHT);
      // assert(100==i);
    }  
    
    int outHTTP=dsm1TreeADC->getOutHTTP1bit(ch);
    int inpHTTP=dsm2TreeTRG->getInpHTTP1bit(ibr,ch);
    
    outHTTPdiff[ch]=outHTTP;
    inpHTTPdiff[ch]=inpHTTP;
    
    //printf("!!! HTTP1bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outHTTP,inpHTTP,outHTTP-inpHTTP);
    
    //hA[510]->Fill(ch+6, inpHTTP);
    hA[514]->Fill(ch+6, outHTTP);
    
    if(outHTTP!=inpHTTP) {
      
      //dsm1TreeADC->print();//tmp
      // printf("###\n");
      
      hA[515]->Fill(ch+6, outHTTP-inpHTTP);
      hA[516]->Fill(ch+6);
      
      if(!mMCflag)  printf("BaD 3JP HTTP1bit(ch=%d)  emu-out1=%d  inp2=%d   diff=%d\n",ch, outHTTP,inpHTTP,outHTTP-inpHTTP);
      // assert(300==i);
    } 
    
    int outTP=dsm1TreeADC->getOutTP1bit(ch);
    int inpTP=dsm2TreeTRG->getInpTP1bit(ibr,ch);
    
    outTPdiff[ch]=outTP;
    inpTPdiff[ch]=inpTP;
    
    // printf("!!! TP1bit(ch=%d)  out1=%d  inp2=%d   diff=%d\n",ch, outTP,inpTP,outTP-inpTP);
    
    //hA[510]->Fill(ch+9, inpTP);
    hA[514]->Fill(ch+9, outTP);
    
    if(outTP!=inpTP) {
      
      //dsm1TreeADC->print();//tmp
      // printf("###\n");
      
      hA[515]->Fill(ch+9, outTP-inpTP);
      hA[516]->Fill(ch+9);
      
      if(!mMCflag) printf("BaD 3JP TP1bit(ch=%d)  emu-out1=%d  inp2=%d   diff=%d\n",ch, outTP,inpTP,outTP-inpTP);
      //assert(200==i);
    } 
    
  }
  
  //----------------- compare Esum5bit for both Endcap & Barrel
  
  for(ibr=0;ibr<EMCdsm2Tree::Nbe2;ibr++) { // loop over DSM2 boards
    for(ch=0;ch<2;ch++) {
      int jch = 2*ibr+ch;
      int outE = dsm2TreeADC->getInpEsum5bit(ibr,ch);// Bemc is only avaliable here
      int inpE = dsm2TreeTRG->getInpEsum5bit(ibr,ch);

      hA[500]->Fill(jch, inpE); 
      hA[504]->Fill(jch, outE);
      
      if(outE!=inpE) {
	hA[505]->Fill(jch, outE-inpE);
	hA[506]->Fill(jch);
	if(!mMCflag) printf("BaD  Esum5bit(ibr=%d,ch=%d)  adcOut1=%d  trgInp2=%d   diff=%d\n",ibr,ch, outE,inpE,outE-inpE);
      } 
    }
  }
}

//==================================================
//==================================================
void 
StEemcTriggerSimu::compareADC2_TRG3(){
  //printf("Wow!\n");
  int outEtot=dsm2TreeADC->getOutEtot1bit();
  int inpEtot=dsm3TRG->getEtotThr1bit();

  int ch=0;
  
  //hA[620]->Fill(ch, inpEtot);
  hA[624]->Fill(ch, outEtot);

  if(outEtot!=inpEtot){

    hA[625]->Fill(ch, outEtot-inpEtot);
    hA[626]->Fill(ch);

     if(!mMCflag) printf("BaD Etot 1bit emu-out2=%d inp3=%d, diff=%d\n", outEtot, inpEtot, outEtot-inpEtot);
  }
}

//==================================================
//==================================================
void 
StEemcTriggerSimu::DSM2EsumSpectra(){

  int BarreEsum = dsm2TreeTRG->getIntBarreSum();
  int adcBarreEsum = dsm2TreeADC->getIntBarreSum();

  //printf("BarreEsum=%d, adcBarreEsum=%d\n", BarreEsum, adcBarreEsum); 

  hA[550]->Fill(BarreEsum);
  hA[560]->Fill(BarreEsum);
  hA[551]->Fill(adcBarreEsum);

  if(BarreEsum!=adcBarreEsum){
    int chB=1;
    hA[556]->Fill(chB);
  }

  int EndcapEsum = dsm2TreeTRG->getIntEndcapSum();
  int adcEndcapEsum = dsm2TreeADC->getIntEndcapSum();

  //printf("EndcapEsum=%d, adcEndcapEsum=%d\n", EndcapEsum, adcEndcapEsum); 

  hA[552]->Fill(EndcapEsum);
  hA[562]->Fill(EndcapEsum);
  hA[553]->Fill(adcEndcapEsum);

  if(EndcapEsum!=adcEndcapEsum){
    int chE=4;
    hA[556]->Fill(chE);
  }

  int Etot = dsm2TreeTRG->getIntEtot();
  int adcEtot = dsm2TreeADC->getIntEtot();

  hA[554]->Fill(Etot);
  hA[564]->Fill(Etot);
  hA[555]->Fill(adcEtot);

  if(Etot!=adcEtot){
    int chT=7;
    hA[556]->Fill(chT);
  }
  
}

//
// $Log: StEemcTriggerCompare.cxx,v $
// Revision 1.3  2009/10/13 17:06:52  pibero
// Changed location of #include files
//
// Revision 1.2  2007/07/23 02:59:59  balewski
// cleanup, bbc for M-C still not working
//
