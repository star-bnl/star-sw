// $Id: EEqaSorterC.cxx,v 1.4 2009/01/25 01:36:54 ogrebeny Exp $
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <TObjArray.h>
#include <TClonesArray.h>

#include <TH2.h>
#include <TFile.h>
#include <TLine.h>
#include <TText.h>

#include "EEqaSorterC.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

#include "StEEmcDbMaker/EEmcDbItem.h"

//tmp until I find better solution
#include "EEqaPresenter.h"
// tmp end

#include "StEEmcDbMaker/StEEmcDbMaker.h"

ClassImp(EEqaSorterC)

//-------------------------------------------
//-------------------------------------------
EEqaSorterC:: EEqaSorterC( TObjArray*L,StEEmcDbMaker*dbx) {
  // printf("\n\n  EEqaSorterC:: EEqaSorterC() \n\n");
  HList=L; 
  eeDb=dbx;

  adcThrTw=40;
  adcThrPrs=200;
  adcThrPost=50;
  adcThrSmd=100;
  memset(h2D,0,sizeof(h2D));
  memset(hMult,0,sizeof(hMult));
} 
 


//-------------------------------------------
//-------------------------------------------
void  EEqaSorterC::sort( EztEmcRawData  *t,  EztEmcRawData  *s, int ver ) {
  eETow=t;
  eESmd=s;
  sortTower();
  sortMapmt(ver);

}


//-------------------------------------------
//-------------------------------------------
void  EEqaSorterC::sortTower(){
  if(eETow==0) return;
  int nTw=0;
  int icr;
  for(icr=0;icr<eETow->getNBlocks();icr++) {
    if(eETow->isCrateVoid(icr)) continue;
    int crateID=icr+1;
    int i;
    //printf("cr=%d\n",crateID);
    const UShort_t* data=eETow->data(icr);
    for(i=0;i<eETow->sizeData(icr);i++) {
      int chan=i;
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(!x) continue; // noDB info
      if(x->fail ) continue;  // drop broken channels
      float adc=data[i]-x->ped; // ped subtracted ADC
      if(adc<adcThrTw) continue;
      nTw++;
      int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
      h2D[0]->Fill(iphi,x->eta);
    } // end of loop over hist
  }// end of loop over crates
  hMult[0] ->Fill(nTw);
}


//-------------------------------------------
//-------------------------------------------
void  EEqaSorterC::sortMapmt( int ver){
  if(eESmd==0) return;
  int nHit[mxh],nSmdH[MaxSectors][MaxSmdPlains];
  memset(nHit,0,sizeof(nHit));
  memset(nSmdH,0,sizeof(nSmdH));

  int icr;
  for(icr=0;icr<eESmd->getNBlocks();icr++) {
    if(eESmd->isCrateVoid(icr)) continue;
    int crateID=icr+64;
    // in 2004 there was only 16 MAPMT crates for sectors 5-8
    if(ver<0x22) {
      if(icr>=16) break;
      crateID=icr+84;
    }
    //printf("ddd %d %d\n",icr,crateID);
    int i;
    const UShort_t* data=eESmd->data(icr);
    for(i=0;i<eESmd->sizeData(icr);i++) {
      int chan=i;
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(!x) continue; // noDB info
      if(x->fail) continue;  // drop broken channels
     
      float thr=0;
      char cD=x->name[2];
      switch(cD) {
      case 'P':
      case 'Q': thr=adcThrPrs;  break;
      case 'R': thr=adcThrPost; break;
      case 'U':
      case 'V': thr=adcThrSmd;  break;
      default :; 
      }
      if(thr<=0) continue; // should never happend     
      float adc=data[i]-x->ped; // ped subtracted ADC
      if(adc<thr) continue;
      // printf("%d %f =%c= p=%p i=%d ",data[i],adc,cPQR,h,1-cPQR-'P'); x->print();
      
      if(x->isSMD()) {
	    int strip=x->strip;
	    int iplane =x->plane-'U';
	    int isec=x->sec-1;
	    hSmd[isec][iplane]->Fill(strip);
	    nSmdH[isec][iplane]++;
      } else {
	    int ik=1+cD-'P';
	    nHit[ik]++;
	    int iphi=(x->sec-1)*MaxSubSec+(x->sub-'A');
	    h2D[ik]->Fill(iphi,x->eta);
      }
      hMAPMT->Fill(crateID,x->mapmtId());
    }
  }// end of loop over crates

  int j,pl;
  for(j=1;j<=3;j++) hMult[j]->Fill(nHit[j]);

  for (j=0; j<MaxSectors; j++) {
    for (pl=0; pl<MaxSmdPlains; pl++) {
      hnHSmd[j][pl]->Fill(nSmdH[j][pl]);
    }
  } 
}


//--------------------------------------------------
//--------------------------------------------------
void EEqaSorterC::initHisto(){
  int i;
  char tit[500];
  sprintf (tit, "Tower hits>ped+%d; phibin : 1TA=0  3TA=10  5TA=20  7TA=30  9TA=40  11TA=50 ; #eta bin ",adcThrTw);
  h2D[0] = new TH2F("TowHits",tit, 60, -0.5, 59.5, 12, 0.5, 12.5);

   addJPphiLimits( h2D[0]);
   //TList *Lx= h2D[0]->GetListOfFunctions(); 

  sprintf (tit, "pres-1 hits>ped+%d; phi bin : 1TA=0  3TA=10  5TA=20  7TA=30  9TA=40  11TA=50 ; #eta bin ",adcThrPrs);
  h2D[1] = new TH2F("Pre1Hits",tit , 60, -0.5, 59.5, 12, 0.5, 12.5);
 sprintf (tit, "pres-2 hits>ped+%d; phi bin: 1TA=0  3TA=10  5TA=20  7TA=30  9TA=40  11TA=50 ; #eta bin ",adcThrPrs);
  h2D[2] =new TH2F("Pre2Hits",tit , 60, -0.5, 59.5, 12, 0.5, 12.5);

  sprintf (tit, "post hits>ped+%d; phi bin : 1TA=0  3TA=10  5TA=20  7TA=30  9TA=40  11TA=50 ; #eta bin ",adcThrPost);
  h2D[3] = new TH2F("PostHits",tit , 60, -0.5, 59.5, 12, 0.5, 12.5);


  sprintf (tit, "# Tower w/ adc>ped+%d; # Towers",adcThrTw);
  hMult[0] = new TH1F("HTow", tit, 100, -0.5, 99.5);

  sprintf (tit, "# Pre-1 w/ adc>ped+%d; # Tiles",adcThrPrs);
  hMult[1] = new TH1F("HPre1", tit, 100, -0.5, 199.5);
  sprintf (tit, "# Pre-2 w/ adc>ped+%d; # Tiles",adcThrPrs);
  hMult[2] = new TH1F("HPre2", tit, 100, -0.5, 199.5);
  sprintf (tit, "# Post w/ adc>ped+%d; # Tiles",adcThrPrs);
  hMult[3] = new TH1F("HPost", tit, 100, -0.5, 199.5);


  for(i=0;i<mxh;i++) {
    if(hMult[i]) HList->Add(hMult[i]);  
    if(h2D[i]) HList->Add(h2D[i]);  
  }

  hMAPMT= new TH2F("MAPMHits","MAPMT Hits adc>ped+thr; crateID ; tube no.", 48, 63.5, 111.5, 12, 0.5, 12.5);
  HList->Add(hMAPMT);

  int iSec;
  for(iSec=0; iSec<MaxSectors; iSec++) {
    char cid[50], ctitl[50];
    int pl;
    for(pl=0;pl<MaxSmdPlains;pl++) {
      char uv='U'+pl;
      sprintf (cid, "SmdA%d%c",iSec+1,uv);
      sprintf (ctitl, "SMD %02d%c adc>ped+%02d;"
	       "Strip no.", iSec+1,uv, adcThrSmd);
      hSmd[iSec][pl] = new TH1F(cid, ctitl, 290, -0.5, 289.5);
      HList->Add(hSmd[iSec][pl]);
    
      sprintf (cid, "HSmd%d%c",iSec+1,uv);
      sprintf (ctitl, "SMD %02d%c Hits/eve  adc>ped+%d; No. Hits", iSec+1, uv,adcThrSmd);
      hnHSmd[iSec][pl] = new TH1F(cid, ctitl, 100, -0.5, 99.5);
      HList->Add(hnHSmd[iSec][pl]);
    }
  }

}
  
//-------------------------------------------
//-------------------------------------------
void  EEqaSorterC::initRun() {


}



// $Log: EEqaSorterC.cxx,v $
// Revision 1.4  2009/01/25 01:36:54  ogrebeny
// *** empty log message ***
//
// Revision 1.3  2009/01/23 00:14:50  ogrebeny
// Inherited EEmcDb from StEEmcDbMaker to fix run-time bug http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1378
//
// Revision 1.2  2007/06/01 17:47:05  jml
// attempt to fix panitkin plot compile
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
// Revision 1.8  2004/03/13 22:03:13  balewski
