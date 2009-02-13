#include "FMSHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <stdio.h>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"
#include "TStyle.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "StEvent/StTriggerData.h"
#  include "TriggerData.h"
#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"

ClassImp(FMSHistogramGroup) ;

FMSHistogramGroup::FMSHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
  : HistogramGroup(group,subGroup,trigger,detector) {
  createHistos();
}

FMSHistogramGroup::~FMSHistogramGroup() {
  deleteHistos();
}

void FMSHistogramGroup::createHistos(){
  if(h_fms_nqtdata!=0) {
    printf("FMSHistogramGroup::createHistos() - Histogranms already exist... Not creating\n"); 
    return; 
  }  
  readParams();
  h_fms_nqtdata      = new TH1D("FMS_NQTdata","FMS_NQTdata",1586,-0.5,1585.5);
  h_fms_nqtheader    = new TH1D("FMS_NQTHeader","FMS_NQTHeader",49,-0.5,48.5);
  h_fms_qtdecode_err = new TH1D("FMS_QTdecode_Error","FMS_QTdecode_Error",11,-0.5,10.5);
  for(int i=0; i<mMaxCrate; i++){
    char name[100];
    sprintf(name,"FMS_QT%1d_LED_SUM",i+1);
    h_fms_quad_sum[i]=new TH1D(name,name,sbin[i],smin[i],smax[i]);
    sprintf(name,"FMS_QT%1d_LED_Mult_ADC>%04d",i+1,mthr[i]);
    h_fms_quad_mult[i]=new TH1D(name,name,mbin[i],mmin[i],mmax[i]);
  }
}

void FMSHistogramGroup::deleteHistos() {
  delete h_fms_nqtdata;
  delete h_fms_nqtheader;
  delete h_fms_qtdecode_err;
  for(int i=0; i<mMaxCrate; i++){ 
    delete h_fms_quad_sum[i];
    delete h_fms_quad_mult[i];
  }
}

void FMSHistogramGroup::reset() {
  h_fms_nqtdata->Reset();
  h_fms_nqtheader->Reset();
  h_fms_qtdecode_err->Reset();
  for(int i=0; i<mMaxCrate; i++){ 
    h_fms_quad_sum[i]->Reset();
    h_fms_quad_mult[i]->Reset();
  }
}

void FMSHistogramGroup::draw(TCanvas* cc) {
  gStyle->SetOptStat(111110);
  gStyle->SetStatFontSize(0.14);
  gStyle->SetTitleFontSize(0.1);
  cc->cd();
  cc->Clear();
  cc->Divide(2, 4);
  //cc->cd(1); h_fms_nqtdata->Draw();
  //cc->cd(2); h_fms_nqtheader->Draw();
  //cc->cd(3); h_fms_qtdecode_err->Draw();
  for(int i=0; i<mMaxCrate; i++) { 
    cc->cd(i*2+1); 
    h_fms_quad_sum[i]->GetXaxis()->SetLabelSize(0.1); 
    h_fms_quad_sum[i]->GetYaxis()->SetLabelSize(0.1); 
    h_fms_quad_sum[i]->Draw(); 
    cc->cd(i*2+2); 
    h_fms_quad_mult[i]->GetXaxis()->SetLabelSize(0.1); 
    h_fms_quad_mult[i]->GetYaxis()->SetLabelSize(0.1); 
    h_fms_quad_mult[i]->Draw(); 
  }
  cc->Update();
} 

bool FMSHistogramGroup::fill(evpReader* evp, char* datap) { 
#ifndef NEW_DAQ_READER
  int ret = trgReader(datap);
  if(ret <= 0) {
    fprintf(stderr, "TRG RAW: problems in data (%d) - continuing...", ret);
    return false;
  }
  TrgSumData* sumData = static_cast<TrgSumData*>(trg.trg_sum);
  if(!sumData) {
    fprintf(stderr, "TRG RAW: cannot get TrgSumData data - continuing...");
    return false;
  }
  L0_DSM_Data* dsmData = &sumData->DSMdata;
  int isLED  = (dsmData->lastDSM[7] & 0x800) >> 11;
  //printf("lastDSM[7]=%x  isLED=%x\n",dsmData->lastDSM[3],isLED);
  if(isLED==1){
    if(decodeQT()){
      h_fms_nqtdata->Fill(mNumQTdata);
      h_fms_nqtheader->Fill(mNumHeader);
      for(int i=0; i<mMaxCrate; i++) {
	h_fms_quad_sum[i]->Fill(mQuadSum[i]);
	h_fms_quad_mult[i]->Fill(mQuadMult[i]);
      }    
    }
    //printf("NQTDATA=%d NHeader=%d\n",mNumQTdata,mNumHeader);
  }
#else
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(trgd){
  }
#endif
  return true;
}

bool FMSHistogramGroup::decodeQT(){
#ifndef NEW_DAQ_READER
  mNumHeader=0;
  mNumQTdata=trg.QQTdataBytes/4;  
  if(mNumQTdata==0) return true;
  if(mNumQTdata>mMaxLine) {h_fms_qtdecode_err->Fill(1); return false;}
  if(trg.QQTdata[mNumQTdata-1]!=mQTLastWord) {h_fms_qtdecode_err->Fill(2); return false;}
  memset(mADC,0,sizeof(mADC));
  memset(mTDC,0,sizeof(mTDC));
  memset(mBdSum,0,sizeof(mBdSum));
  memset(mQuadSum,0,sizeof(mQuadSum));
  memset(mQuadMult,0,sizeof(mQuadMult));
  int header=1, nline=0, iline;
  int crate=0, addr=0;
  h_fms_qtdecode_err->Fill(0);
  for (int i=0; i<static_cast<int>(mNumQTdata-1); i++){    
    unsigned int d = trg.QQTdata[i];    
    if (header==1){
      crate = getCRT(d)-mOffsetCrate;
      addr  = getADR(d)-mOffsetAddr;
      nline = getNHT(d);
      if(crate<0 || crate>mMaxCrate) h_fms_qtdecode_err->Fill(3);
      if(addr<0  || addr>mMaxAddr) h_fms_qtdecode_err->Fill(4);
      if(nline<0 || nline>32) h_fms_qtdecode_err->Fill(5);
      if(nline>0) {header=0; iline=0;}
      mNumHeader++;
    } else {
      iline++;
      unsigned short dcard = getQT8(d);
      unsigned short dch   = getCHA(d);
      unsigned short adc   = getADC(d);
      if(dcard>mMaxDCard) h_fms_qtdecode_err->Fill(6);
      if(dch>mMaxChan)    h_fms_qtdecode_err->Fill(7);
      if(getUNU(d)!=0)    h_fms_qtdecode_err->Fill(8);
      if(adc==0)          h_fms_qtdecode_err->Fill(9);
      int tst=(char*)&mADC[crate][addr][dcard][dch]-(char*)&mADC[0][0][0][0];       
      if(tst>=0 && tst<(int)sizeof(mADC)){
	mADC[crate][addr][dcard][dch]=adc;
	mTDC[crate][addr][dcard][dch]=getTDC(d);
	mBdSum[crate][addr]+=adc;
	mQuadSum[crate]+=adc;
	if(adc>mthr[crate]) mQuadMult[crate]++; 
	//	printf("crate=%d addr=%d dcard=%d ch=%d adc=%d thr=%d\n",crate,addr,dcard,dch,adc,mthr[crate]);
      }else{
	h_fms_qtdecode_err->Fill(10);
      }
      if(nline==iline) header=1;
    }
  }
#endif
  return true;
}

void FMSHistogramGroup::readParams() {
#ifndef NEW_DAQ_READER
  FILE *fp;
  char buf[100];
  if ( ( fp = fopen("/home/operator/akio/fms_params.txt","r")) ){
    //printf("Reading /home/operator/akio/fms_params.txt\n");
    for(int i=0; i<mMaxCrate; i++){      
      fgets(buf,sizeof(buf),fp);
      int n=sscanf(buf,"%d %lf %lf",&sbin[i],&smin[i],&smax[i]);
      if(n!=3){printf("Erro reading /home/operator/akio/fms_params.txtr\n");}
      //printf("%6d %10.2f %10.2f\n",sbin[i],smin[i],smax[i]);
    }
    for(int i=0; i<mMaxCrate; i++){
      fgets(buf,sizeof(buf),fp);
      int n=sscanf(buf,"%d %lf %lf %d",&mbin[i],&mmin[i],&mmax[i],&mthr[i]);
      if(n!=4){printf("Error /home/operator/akio/fms_params.txt\n");}
      //printf("%6d %10.2f %10.2f %6d\n",mbin[i],mmin[i],mmax[i],mthr[i]);
    }
    //fclose(fp);
  }else{
    for(int i=0; i<mMaxCrate; i++){
      sbin[i]=100; smin[i]=0; smax[i]=10000;
      mbin[i]=100; mmin[i]=0; mmax[i]=500; mthr[i]=5;
    }
  }
#endif
}
