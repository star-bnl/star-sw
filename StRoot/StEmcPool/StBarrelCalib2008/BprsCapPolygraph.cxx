#include <stdio.h>
#include <TH2F.h>
#include <TObjArray.h>
#include <TGraph.h>

#include "BprsCapPolygraph.h"
#include "StJanBarrelDbMaker.h"
#include "JanBarrelEvent.h"


//________________________________________________
//________________________________________________
BprsCapPolygraph::BprsCapPolygraph( TObjArray *HList, StJanBarrelDbMaker* jdb, int pedFlag) {
    par_pedFlag=pedFlag;
    mJanDbMaker=jdb; assert(mJanDbMaker);

    const char *fname="all"; //tmp
    //QA histos

    hChiB=new TH1F("bppg_chb","BprsPoly logN(chi2/dof), nominal capID ; logN(chi2/dof)",200,-1.,9.);
    hChiGap=new TH1F("bppg_chgI","BprsPoly chi2/dof (nominal - best_capID), any minimum; logN( nominal_chi2 - best_chi2 ) ",200,-6,10);
    hChiGap2=new TH1F("bppg_chgA","BprsPoly chi2/dof (nominal - best_capID), acceted; logN( nominal_chi2 - best_chi2 ) ",200,-6,10);
      
    char tt2[1000];
    hCh2D=new TH2F("bppg_ch2D","BprsPoly chi2/DOF(best capID.NE.nominalCapID) ; LogN( chi2/DOF), best; logN(chi2/DOF), nominal ",50,-1,10,50,-1,10);
    sprintf(tt2,"BprsPoly caps in sync, %s; nominal capID; BPRS crate ID",fname);
    hCapGood=new TH2F("bppg_capSyn",tt2, 128,-0.5,127.5,mxBprsCrate,-0.5,mxBprsCrate-0.5);
    sprintf(tt2,"BprsPoly caps NOT sync, %s; nominal capID; BPRS crateID",fname);
    hCapCorr=new TH2F("bppg_capDeSyn",tt2, 128,-0.5,127.5,mxBprsCrate,-0.5,mxBprsCrate-0.5);

    HList->Add( hChiB);
    HList->Add( hChiGap);
    HList->Add( hChiGap2);
    HList->Add( hCh2D);
    HList->Add(hCapGood);
    HList->Add(hCapCorr);
 
   int nb=200;
    float adc1=-50;
    float adc2=adc1+nb;

    hAdcGood=new TH1F("bppg_adcGd","BprsPoly ADC for capID in sync; rawAdc - ped(nominal capID)",nb,adc1,adc2);
    hAdcCorr=new TH1F("bppg_adcCor","BprsPoly ADC for capID NOT in sync,fixed; rawAdc - ped(best capID)",nb,adc1,adc2);
    hAdcCorr->SetLineColor(kRed);

    HList->Add(hAdcGood);
    HList->Add( hAdcCorr);
   
  }

//________________________________________________
//________________________________________________
void BprsCapPolygraph::doBaseline( JanBprsEveA & bprsEve, JanBarrelEvent &fullEve){
  
  int crateID=bprsEve.crateID;
  int   capID=bprsEve.capID;
  printf("doBaseline: bprsEve: %d rawADC, doCrate=%d nominalCapID=%d\n", bprsEve.raw.GetN(),crateID,capID);
  TGraph res; //residua:  X=ADC-ped, y=index of raw; 
  
  int ibp=kBPrs;
  for(int i=0;i< bprsEve.raw.GetN();i++) {
    double rawAdc, yid;
    bprsEve.raw.GetPoint(i,rawAdc,yid);
    int id= (int) yid;
    // printf("K i=%d id=%d rawAdc=%f\n",i,id,rawAdc);
    int   stat =mJanDbMaker->statTile(ibp,id);
    if(stat) continue;
    float ped  =mJanDbMaker->pedTile(ibp,id,capID);
    float sig  =mJanDbMaker->sigPedTile(ibp,id,capID);
    double del=rawAdc-ped;
    if(del> cut_adcMax) continue;
    //  if(i<20)
    //printf("i=%d id=%d del=%f sig=%f\n",i,id,del,sig);
    assert(sig!=0);
    del/=sig;
    res.SetPoint(res.GetN(),del,i);
  } // end of loop over raw data
  
  res.Sort();
  int i2=(int)(res.GetN()*cut_fracSkip);
  int i1=res.GetN()-i2;
  printf("res size=%d , use=[%d,%d]\n", res.GetN(),i1,i2);
  // res.Print();
  
  double sum=0;
  for(int i=i1; i<i2;i++) {
    double rdel, yj;
    res.GetPoint(i,rdel,yj);
    sum+=rdel*rdel; // compute base chi2/dof
    //if(i<20) printf("CC i=%d rdel=%f sig=%f  sum=%f\n",i,rdel,sig,sum);    
    // prepare array of index for chi2 minimalization
    int j= (int) yj; // index in 'raw' array
    double rawAdc, yid;
    bprsEve.raw.GetPoint(j,rawAdc,yid);
    bprsEve.addGoodValue(yid,rawAdc);
  }// base chi2 computed 
  int dof=i2-i1;
  float chi2dof=sum/dof;
  printf(" base chi2=%f DOF=%d, chi/DOF=%f nGood=%d\n",sum,dof,chi2dof,bprsEve.good.GetN());
  bprsEve.chi2=sum;
  bprsEve.chi2dof=chi2dof;
  hChiB->Fill(log(chi2dof));
}

//________________________________________________
//________________________________________________
void BprsCapPolygraph::findBestCap(JanBprsEveA & bprsEve, JanBarrelEvent &fullEve){

   printf("find BPRS bestCap: eve: %d good\n", bprsEve.good.GetN());
   int ibp=kBPrs;
 
   float minChi2dof=bprsEve.chi2dof-0.0001;
   int bestCapID=-1;

   int cap1=bprsEve.capID-par_mxDelCap+mxBcap;
   int cap2=bprsEve.capID+par_mxDelCap+mxBcap;
   for(int ic=cap1; ic<=cap2;ic++) {
     int cap=ic%mxBcap;
     // printf("xx cap=%d\n",cap);
     double sum=0;
     for(int i=0;i< bprsEve.good.GetN();i++) {
      double rawAdc, yid;
      bprsEve.good.GetPoint(i,rawAdc,yid);
      int id= (int) yid;
      // printf("bK i=%d id=%d rawAdc=%f\n",i,id,rawAdc);
      int   stat =mJanDbMaker->statTile(ibp,id);

      assert(stat==0);// bad channels were already discarded
      float ped  =mJanDbMaker->pedTile(ibp,id,cap);
      float sig  =mJanDbMaker->sigPedTile(ibp,id,cap);
      double del=rawAdc-ped;
      //if(i<20) printf("i=%d id=%d del=%f sig=%f\n",i,id,del,sig);
      del/=sig;
      sum+=del*del;
     } // end of data loop 
     float chi2dof=sum/bprsEve.good.GetN();
     printf(" capID=%d chi2=%.1f DOF=%d, chi/DOF=%f\n",cap,sum, bprsEve.good.GetN(),chi2dof);
     if(minChi2dof<=chi2dof) continue;
     //.... found new minimum
     minChi2dof=chi2dof;
     bestCapID=cap;
   }// end of loop over caps

   //  float minChi2dof=bprsEve.chi2dof-cut_stepChi2dof;

   if(bestCapID>=0)hChiGap->Fill(log(bprsEve.chi2dof-minChi2dof));

   if( minChi2dof<bprsEve.chi2dof-cut_stepChi2dof) { // better minimum was found, event corrupted
     assert(bestCapID>=0);
     // printf("ccc %f %f %f\n",minChi2dof,bprsEve.chi2dof ,log(bprsEve.chi2dof-minChi2dof));
     hChiGap2->Fill(log(bprsEve.chi2dof-minChi2dof));
     hCh2D->Fill(log(minChi2dof),log(bprsEve.chi2dof));
     hCapCorr->Fill(bprsEve.capID,bprsEve.crateID);
     printf("BETTER capID=%d\n",bestCapID);
     bprsEve.bestCapID=bestCapID;
     bprsEve.bestChi2dof=minChi2dof;
     bprsEve.useFix=true;
   }  else { // caps are in cync
     hCapGood->Fill(bprsEve.capID,bprsEve.crateID);
   } 
}

//________________________________________________
//________________________________________________
void BprsCapPolygraph::doPedResidua(JanBprsEveA & bprsEve){
   printf("doPedRes: eveID=%d: %d good\n", bprsEve.eveID,bprsEve.good.GetN());
 
   int capID=bprsEve.capID;
   int ibp=kBPrs;

   switch(par_pedFlag){
   case 0: break; // all w/o correction
   case 1: if(bprsEve.bestCapID>=0) return; // skip corrupted events
     break;
   case 2: if(bprsEve.bestCapID<0) return; // skip good event
     capID=bprsEve.bestCapID; // fix capID
     break;
   case 3: if(bprsEve.bestCapID>=0) capID=bprsEve.bestCapID; // fix & keep all
     break;
   }

   int nX=0;//tmp

   for(int i=0;i< bprsEve.raw.GetN();i++) {
     double rawAdc, yid;
     bprsEve.raw.GetPoint(i,rawAdc,yid);
     int id= (int) yid;
     // printf("cK i=%d id=%d rawAdc=%f\n",i,id,rawAdc);
     int   stat =mJanDbMaker->statTile(ibp,id);
     if(stat) continue;// drop bad channels      
     float ped  =mJanDbMaker->pedTile(ibp,id,capID);
     double del=rawAdc-ped;
     //    hPedRes2D->Fill(id,del);
     if(bprsEve.bestCapID<0) hAdcGood->Fill(del); // good
     else  hAdcCorr->Fill(del);// fixed

     if(del<-10) nX++;
     } // end of data loop 
   printf("nX=%d\n",nX);
 }

