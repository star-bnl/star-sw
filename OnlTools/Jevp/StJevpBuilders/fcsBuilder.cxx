#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_FCS/daq_fcs.h"

#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StTriggerData2019.h"
#include "StRoot/StEvent/StTriggerData2022.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "fcsBuilder.h"
#include <RTS/include/rtsLog.h>

#include "fcsMap.h"

const int TBMAX=300;
//const int TBTRG[2]={51,207+8};
const int TBTRG[2] ={  50,207+4};
const int MINTB[2] ={   0,150};
const int MAXTB[2] ={ 100,300};
const int MAXADC[2]={ 512,4096};
const int MAXSUM[2]={1000,20000};
const int OFF[2]  = {0,0};
const int mFakeData=0;    
const int FCS_DEBUG=0;

ClassImp(fcsBuilder);
  
fcsBuilder::fcsBuilder(JevpServer *parent) : JevpBuilder(parent) {
    plotsetname = (char *)"fcs";    
    // start with histograms undefined...
    memset(&contents, 0, sizeof(contents));
}

fcsBuilder::~fcsBuilder() {
  // Delete any existing histograms...
    int n = sizeof(contents) / sizeof(TH1 *);
    for(int i=0;i<n;i++) {
	if(contents.array[i]) delete contents.array[i];
    }
}

void fcsBuilder::initialize(int argc, char *argv[]) {
    makeMap();

    // Initialization of histograms.
    contents.h_evt_size = new TH1F("h_evt_size","log of Event Size; log(evt size)",50,1,10);
    
    contents.h_fcs_crt_depch_tbin[0] = new TH2F("Crt0_DepCh_TBin","Crt0 EcalN; Slt+Ch/32; TBin",32*20,0,20,TBMAX,0,TBMAX);
    contents.h_fcs_crt_depch_tbin[1] = new TH2F("Crt1_DepCh_TBin","Crt1 MixN;  Slt+Ch/32; TBin",32*20,0,20,TBMAX,0,TBMAX);
    contents.h_fcs_crt_depch_tbin[3] = new TH2F("Crt3_DepCh_TBin","Crt3 MixS;  Slt+Ch/32; TBin",32*20,0,20,TBMAX,0,TBMAX);
    contents.h_fcs_crt_depch_tbin[4] = new TH2F("Crt4_DepCh_TBin","Crt4 EcalS; Slt+Ch/32; TBin",32*20,0,20,TBMAX,0,TBMAX);
    contents.h_fcs_crt_depch_tbin[0]->GetXaxis()->SetNdivisions(-20);
    contents.h_fcs_crt_depch_tbin[1]->GetXaxis()->SetNdivisions(-20);
    contents.h_fcs_crt_depch_tbin[3]->GetXaxis()->SetNdivisions(-20);
    contents.h_fcs_crt_depch_tbin[4]->GetXaxis()->SetNdivisions(-20);
    TLine* l1 = new TLine(0,TBTRG[0]-3,20,TBTRG[0]-3); l1->SetLineColor(kMagenta);
    TLine* l2 = new TLine(0,TBTRG[0]+4,20,TBTRG[0]+4); l2->SetLineColor(kMagenta);
    TLine* l3 = new TLine(0,TBTRG[1]-3,20,TBTRG[1]-3); l3->SetLineColor(kMagenta);
    TLine* l4 = new TLine(0,TBTRG[1]+4,20,TBTRG[1]+4); l4->SetLineColor(kMagenta);

    contents.h_fcs_det_tbin_adc[0] = new TH2F("EcalTbAdc","Ecal; Timebin; ADC",TBMAX,0,TBMAX,1024,0,4095);
    contents.h_fcs_det_tbin_adc[1] = new TH2F("HcalTbAdc","Hcal; Timebin; ADC",TBMAX,0,TBMAX,1024,0,4095);
    contents.h_fcs_det_tbin_adc[2] = new TH2F("PresTbAdc","Pres; Timebin; ADC",TBMAX,0,TBMAX,1024,0,4095);
    TLine* l5 = new TLine(TBTRG[0]-3,0,TBTRG[0]-3,512); l5->SetLineColor(kMagenta);
    TLine* l6 = new TLine(TBTRG[0]+4,0,TBTRG[0]+4,512); l6->SetLineColor(kMagenta);
    TLine* l7 = new TLine(TBTRG[1]-3,0,TBTRG[1]-3,512); l7->SetLineColor(kMagenta);
    TLine* l8 = new TLine(TBTRG[1]+4,0,TBTRG[1]+4,512); l8->SetLineColor(kMagenta);

    int ecal_xmax = nColumn(0)+OFF[0];
    int ecal_ymax = nRow(0);
    contents.h_fcs_det_hitmap[0] = new TH2F("Ecal","Ecal; +-Col   North <-> South; -Row",
					    ecal_xmax*2+1,-ecal_xmax-0.5,ecal_xmax+0.5,
					    ecal_ymax,-ecal_ymax-0.5,-0.5); 
    int hcal_xmax = nColumn(2)+OFF[1];
    int hcal_ymax = nRow(2);
    contents.h_fcs_det_hitmap[1] = new TH2F("Hcal","Hcal; +-Col   North <-> South; -Row",
					    hcal_xmax*2+1,-hcal_xmax-0.5,hcal_xmax+0.5,
					    hcal_ymax,-hcal_ymax-0.5,-0.5); 
    int pres_xmax = nColumn(4);
    int pres_ymax = nRow(4);
    contents.h_fcs_det_hitmap[2] = new TH2F("Pres","Pres; +-Radius/Col (North <-> South); Phi/-Row",
					    pres_xmax*2+1,-pres_xmax-0.5,pres_xmax+0.5,
					    pres_ymax,-pres_ymax-0.5,-0.5); 
    //contents.h_fcs_det_hitmap[2] = new TH2F("Pres","Pres; View from back",
    //				    pres_ymax*2,-pres_ymax,pres_ymax,
    //					    pres_xmax+1,0.0,pres_xmax+1);
     
    char csum[30];
    sprintf(csum,"AdcSum(TB=%d-%d)",TBTRG[0]-3,TBTRG[0]+4);
    contents.h_fcs_ehpns_id_adcsum[0][0] = new TH2F("Ecal_N_Id_Adcsum",Form("EcalNorth; Id/22; %s",csum),maxId(0),0.0,maxId(0)/22.0,1000,0.0,MAXSUM[1]);
    contents.h_fcs_ehpns_id_adcsum[0][1] = new TH2F("Ecal_S_Id_Adcsum",Form("EcalSouth; Id/22; %s",csum),maxId(1),0.0,maxId(1)/22.0,1000,0.0,MAXSUM[1]);
    contents.h_fcs_ehpns_id_adcsum[1][0] = new TH2F("Hcal_N_Id_Adcsum",Form("HcalNorth; Id/13; %s",csum),maxId(2),0.0,maxId(2)/13.0,1000,0.0,MAXSUM[1]);
    contents.h_fcs_ehpns_id_adcsum[1][1] = new TH2F("Hcal_S_Id_Adcsum",Form("HcalSouth; Id/13; %s",csum),maxId(3),0.0,maxId(3)/13.0,1000,0.0,MAXSUM[1]);
    contents.h_fcs_ehpns_id_adcsum[2][0] = new TH2F("Pres_N_Id_Adcsum",Form("PresNorth; Id/16; %s",csum),maxId(4),0.0,maxId(4)/16.0,1000,0.0,MAXSUM[1]);
    contents.h_fcs_ehpns_id_adcsum[2][1] = new TH2F("Pres_S_Id_Adcsum",Form("PresSouth; Id/16; %s",csum),maxId(5),0.0,maxId(5)/16.0,1000,0.0,MAXSUM[1]);
    contents.h_fcs_ehpns_id_adcsum[0][0]->GetXaxis()->SetNdivisions(-nRow(0));
    contents.h_fcs_ehpns_id_adcsum[0][1]->GetXaxis()->SetNdivisions(-nRow(1));
    contents.h_fcs_ehpns_id_adcsum[1][0]->GetXaxis()->SetNdivisions(-nRow(2));
    contents.h_fcs_ehpns_id_adcsum[1][1]->GetXaxis()->SetNdivisions(-nRow(3));
    contents.h_fcs_ehpns_id_adcsum[2][0]->GetXaxis()->SetNdivisions(-nRow(4));
    contents.h_fcs_ehpns_id_adcsum[2][1]->GetXaxis()->SetNdivisions(-nRow(5));
    contents.h_fcs_ehpns_id_adcsum[0][0]->GetXaxis()->SetLabelSize(0.02);
    contents.h_fcs_ehpns_id_adcsum[0][1]->GetXaxis()->SetLabelSize(0.02);
    contents.h_fcs_ehpns_id_adcsum[0][0]->GetYaxis()->SetLabelSize(0.02);
    contents.h_fcs_ehpns_id_adcsum[0][1]->GetYaxis()->SetLabelSize(0.02);
    contents.h_fcs_ehpns_id_adcsum[1][0]->GetYaxis()->SetLabelSize(0.02);
    contents.h_fcs_ehpns_id_adcsum[1][1]->GetYaxis()->SetLabelSize(0.02);
    contents.h_fcs_ehpns_id_adcsum[2][0]->GetYaxis()->SetLabelSize(0.02);
    contents.h_fcs_ehpns_id_adcsum[2][1]->GetYaxis()->SetLabelSize(0.02);
    
    // Add root histograms to Plots
    int np = sizeof(contents) / sizeof(TH1 *);
    JevpPlot *plots[np];
    mPlots = plots;

    int n=0;
    plots[n] = new JevpPlot(contents.h_evt_size); n++;
    
    plots[n] = new JevpPlot(contents.h_fcs_crt_depch_tbin[0]); plots[n]->setDrawOpts("colz");
    plots[n]->addElement(l1); plots[n]->addElement(l2);
    plots[n]->addElement(l3); plots[n]->addElement(l4); 
    n++;
    plots[n] = new JevpPlot(contents.h_fcs_crt_depch_tbin[1]); plots[n]->setDrawOpts("colz"); 
    plots[n]->addElement(l1); plots[n]->addElement(l2);
    plots[n]->addElement(l3); plots[n]->addElement(l4); 
    n++;
    plots[n] = new JevpPlot(contents.h_fcs_crt_depch_tbin[3]); plots[n]->setDrawOpts("colz"); 
    plots[n]->addElement(l1); plots[n]->addElement(l2);
    plots[n]->addElement(l3); plots[n]->addElement(l4); 
    n++;
    plots[n] = new JevpPlot(contents.h_fcs_crt_depch_tbin[4]); plots[n]->setDrawOpts("colz"); 
    plots[n]->addElement(l1); plots[n]->addElement(l2);
    plots[n]->addElement(l3); plots[n]->addElement(l4); 
    n++;
    
    plots[n] = new JevpPlot(contents.h_fcs_det_hitmap[0]);     plots[n]->setDrawOpts("colz"); n++;
    plots[n] = new JevpPlot(contents.h_fcs_det_hitmap[1]);     plots[n]->setDrawOpts("colz"); n++;
    plots[n] = new JevpPlot(contents.h_fcs_det_hitmap[2]);     plots[n]->setDrawOpts("colz"); n++;
    
    plots[n] = new JevpPlot(contents.h_fcs_ehpns_id_adcsum[0][0]); plots[n]->setDrawOpts("colz"); n++;
    plots[n] = new JevpPlot(contents.h_fcs_ehpns_id_adcsum[0][1]); plots[n]->setDrawOpts("colz"); n++;
    plots[n] = new JevpPlot(contents.h_fcs_ehpns_id_adcsum[1][0]); plots[n]->setDrawOpts("colz"); n++;
    plots[n] = new JevpPlot(contents.h_fcs_ehpns_id_adcsum[1][1]); plots[n]->setDrawOpts("colz"); n++;
    plots[n] = new JevpPlot(contents.h_fcs_ehpns_id_adcsum[2][0]); plots[n]->setDrawOpts("colz"); n++;
    plots[n] = new JevpPlot(contents.h_fcs_ehpns_id_adcsum[2][1]); plots[n]->setDrawOpts("colz"); n++;
    
    plots[n] = new JevpPlot(contents.h_fcs_det_tbin_adc[0]);     plots[n]->setDrawOpts("colz");
    plots[n]->addElement(l5); plots[n]->addElement(l6);
    plots[n]->addElement(l7); plots[n]->addElement(l8);
    n++;
    plots[n] = new JevpPlot(contents.h_fcs_det_tbin_adc[1]);     plots[n]->setDrawOpts("colz");
    plots[n]->addElement(l5); plots[n]->addElement(l6);
    plots[n]->addElement(l7); plots[n]->addElement(l8);
    n++;
    plots[n] = new JevpPlot(contents.h_fcs_det_tbin_adc[2]);     plots[n]->setDrawOpts("colz");
    plots[n]->addElement(l5); plots[n]->addElement(l6);
    plots[n]->addElement(l7); plots[n]->addElement(l8);
    n++;

    for(int i=0; i<n; i++){
      plots[i]->optlogz = 1;
      plots[i]->optstat = 0;
	LOG(DBG, "Adding plot %d = %s",i,plots[i]->GetPlotName());
	addPlot(plots[i]);
    }
}
  
void fcsBuilder::startrun(daqReader *rdr) {
    LOG("JEFF", "fcsBuilder starting run #%d",rdr->run);
    resetAllPlots();    
    mEvt=-1;
    mPhyLed=-1;
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

// Fill Histograms...
void fcsBuilder::event(daqReader *rdr){  
    mEvt++;

    if(FCS_DEBUG) {
      dd = rdr->det("trg")->get("raw");
      if(!dd){
	//printf("trg/raw not found\n");
      }else{
	while(dd->iterate()) {
	  u_char *trg_raw = dd->Byte;
	  int mRun=22921001;
	  TriggerDataBlk2019* trgdata2019 = (TriggerDataBlk2019*)dd->Byte;  
	  StTriggerData* trg = (StTriggerData*) new StTriggerData2019(trgdata2019,mRun,1,0);
	  //printf("Creating StTriggerData for ver=0x46 (2019) with run=%d\n",mRun);
	  unsigned short lastdsm4 = trg->lastDSM(4);
	  unsigned short fcs1   = (lastdsm4 >>  5) & 0x1;
	  unsigned short fcs2   = (lastdsm4 >>  7) & 0x1;
	  unsigned short fcs3   = (lastdsm4 >>  8) & 0x1;
	  unsigned short fcs4   = (lastdsm4 >>  9) & 0x1;
	  unsigned short fcs2019= (lastdsm4 >> 10) & 0x1;
	  unsigned short fcs5   = (lastdsm4 >> 12) & 0x1;
	  unsigned short fcs6   = (lastdsm4 >> 13) & 0x1;
	  unsigned short fcs7   = (lastdsm4 >> 14) & 0x1;
	  unsigned short fcs8   = (lastdsm4 >> 15) & 0x1;	  
	  printf("evt=%8d fcs2019=fcs0=%1d 1=%1d 2=%1d 3=%1d 4=%1d 5=%1d 6=%1d 7=%1d 8=%1d\n",
		 mEvt,fcs2019,fcs1,fcs2,fcs3,fcs4,fcs5,fcs6,fcs7,fcs8);
	  unsigned short lastdsm5 = trg->lastDSM(5);
	  printf("evt=%8d FMSBITS = 0x%4x\n",lastdsm5);
	  delete trg;
	  trg = NULL;
	}	
      }
    }
    
    int fcs_size = rdr->getDetectorSize("fcs");
    int sz = rdr->getDetectorSize("/");
    LOG(DBG, "getDetectorSize(fcs) = %d,  evtSize = %d (diff=%d)", sz, rdr->event_size,rdr->event_size-sz);
    contents.h_evt_size->Fill(safelog(fcs_size));
    
    int mReadMode=-1;
    int trgcmd = rdr->trgcmd;
    //printf("trgcmd=%d\n",trgcmd);
    switch(trgcmd){
    case  4: mReadMode=1; break; //ZS bank for Physics
    case 10: mReadMode=0; break; //ADC bank for LED
    default: return;
    }
        
    string mode[2]={"adc","zs"};
    dd = rdr->det("fcs")->get(mode[mReadMode].c_str());
    if(!dd){
      LOG(DBG,"No fcs_%s found",mode[mReadMode].c_str());
    }else{
      if(mPhyLed<0){ //first time in the run
	if(trgcmd==4) {
	  mPhyLed=0;  
	}else{ 
	  mPhyLed=1;
	}
	char csum[30];    
	sprintf(csum,"AdcSum(TB=%d-%d)",TBTRG[mPhyLed]-3,TBTRG[mPhyLed]+4);
	contents.h_fcs_ehpns_id_adcsum[0][0]->SetTitle(Form("EcalNorth; Id/22; %s",csum));
	contents.h_fcs_ehpns_id_adcsum[0][1]->SetTitle(Form("EcalSouth; Id/22; %s",csum));
	contents.h_fcs_ehpns_id_adcsum[1][0]->SetTitle(Form("HcalNorth; Id/13; %s",csum));
	contents.h_fcs_ehpns_id_adcsum[1][1]->SetTitle(Form("HcalSouth; Id/13; %s",csum));
	contents.h_fcs_ehpns_id_adcsum[2][0]->SetTitle(Form("PresNorth; Id/16; %s",csum));
	contents.h_fcs_ehpns_id_adcsum[2][1]->SetTitle(Form("PresSouth; Id/16; %s",csum));
	contents.h_fcs_crt_depch_tbin[0]->GetYaxis()->SetRange(MINTB[mPhyLed],MAXTB[mPhyLed]);
	contents.h_fcs_crt_depch_tbin[1]->GetYaxis()->SetRange(MINTB[mPhyLed],MAXTB[mPhyLed]);
	contents.h_fcs_crt_depch_tbin[3]->GetYaxis()->SetRange(MINTB[mPhyLed],MAXTB[mPhyLed]);
	contents.h_fcs_crt_depch_tbin[4]->GetYaxis()->SetRange(MINTB[mPhyLed],MAXTB[mPhyLed]);
	contents.h_fcs_det_tbin_adc[0]->GetXaxis()->SetRangeUser(MINTB[mPhyLed],MAXTB[mPhyLed]);
	contents.h_fcs_det_tbin_adc[1]->GetXaxis()->SetRangeUser(MINTB[mPhyLed],MAXTB[mPhyLed]);
	contents.h_fcs_det_tbin_adc[2]->GetXaxis()->SetRangeUser(MINTB[mPhyLed],MAXTB[mPhyLed]);
	contents.h_fcs_det_tbin_adc[0]->GetYaxis()->SetRangeUser(0,MAXADC[mPhyLed]);
	contents.h_fcs_det_tbin_adc[1]->GetYaxis()->SetRangeUser(0,MAXADC[mPhyLed]);
	contents.h_fcs_det_tbin_adc[2]->GetYaxis()->SetRangeUser(0,MAXADC[mPhyLed]);	  
	contents.h_fcs_ehpns_id_adcsum[0][0]->GetYaxis()->SetRangeUser(0,MAXSUM[mPhyLed]);
	contents.h_fcs_ehpns_id_adcsum[0][1]->GetYaxis()->SetRangeUser(0,MAXSUM[mPhyLed]);
	contents.h_fcs_ehpns_id_adcsum[1][0]->GetYaxis()->SetRangeUser(0,MAXSUM[mPhyLed]);
	contents.h_fcs_ehpns_id_adcsum[1][1]->GetYaxis()->SetRangeUser(0,MAXSUM[mPhyLed]);
	contents.h_fcs_ehpns_id_adcsum[2][0]->GetYaxis()->SetRangeUser(0,MAXSUM[mPhyLed]);
	contents.h_fcs_ehpns_id_adcsum[2][1]->GetYaxis()->SetRangeUser(0,MAXSUM[mPhyLed]);
      }
      
      while(dd->iterate()) {
	int sec = ((dd->sec >> 11) & 0x1F) + 1;
	int rdo = ((dd->sec >> 8) & 0x7) + 1;
	int ehp = (dd->sec >> 6) & 0x3;
	int ns  = (dd->sec >> 5) & 1;
	int dep = dd->row ;
	int ch = dd->pad ;
	u_int n=dd->ncontent;
	
	//getting ids from DEP info
	int detid,id,crt,slt;
	getIdfromDep(ehp,ns,dep,ch,detid,id,crt,slt);
	
	//printf("sec=%2d ehp=%1d ns=%1d dep=%2d ch=%2d  det=%1d id=%3d crt=%1d slt=%2d\n",
	//        sec,ehp,ns,dep,ch,detid,id,crt,slt);
	if(detid>=6) continue; //skip unmapped data
	if(ch>=32) continue; //skip trigger data
	if(ehp>=3) continue; //skip main crates for now
	
	u_short *d16 = (u_short *)dd->Void;
	u_short tb, adc;
	u_int sum=0;
	for(u_int i=0; i<n; i++) {		
	  if(mReadMode==0){ //none ZS data
	    tb  = i;
	    adc = d16[i] & 0xFFF;		    
	  }else{            // ZS data
	    tb  = dd->adc[i].tb;                                                               
	    adc = dd->adc[i].adc & 0xFFF;    
	  }
	  contents.h_fcs_crt_depch_tbin[crt]->Fill(slt+float(ch+0.5)/32.0,float(tb),float(adc));		
	  contents.h_fcs_det_tbin_adc[ehp]->Fill(float(tb),float(adc));
	  if(TBTRG[mPhyLed]-3 <= tb && tb <= TBTRG[mPhyLed]+4) sum+=adc;
	}
	int c = getColumnNumber(detid,id);
	int r = getRowNumber(detid,id);
	float x = (c+OFF[ehp]) * (ns*2-1);
	float y = - r;
	//if(ehp==1) printf("ehp=%1d r=%2d c=%2d sum=%4d\n",ehp,r,c,sum);
	if(sum>3) contents.h_fcs_det_hitmap[ehp]->Fill(x,y,float(sum));		
	contents.h_fcs_ehpns_id_adcsum[ehp][ns]->Fill((id+0.5)/nColumn(detid),float(sum));
      }
    }
    
    //fake data
    if(mFakeData==0) return;    
    for(int ehp=0; ehp<3; ehp++){
      for(int ns=0; ns<2; ns++){
	for(int dep=0; dep<getNDep(ehp,ns); dep++){
	  for(int ch=0; ch<32; ch++){	   
	    int detid,id,crt,slt;
	    getIdfromDep(ehp,ns,dep,ch,detid,id,crt,slt);
	    if(detid==6) continue;
	    u_int sum=0;	    
	    for(u_short tb=TBTRG[0]-3; tb<TBTRG[0]+3; tb++){
	      u_short adc=1000 - 333 * abs(tb-TBTRG[0]);
	      contents.h_fcs_crt_depch_tbin[crt]->Fill(slt+float(ch+0.5)/32.0,float(tb),float(adc));
	      int c = getColumnNumber(detid,id);
	      int r = getRowNumber(detid,id);
	      float x = (c+OFF[ehp]) * (ns*2-1);
	      float y = - r;
	      if(adc>0) contents.h_fcs_det_hitmap[ehp]->Fill(x,y,float(adc));		
	      if(TBTRG[0]-3 <= tb && tb <= TBTRG[0]+4) sum+=adc;
	    }
	    contents.h_fcs_ehpns_id_adcsum[ehp][ns]->Fill((id+0.5)/nColumn(detid),float(sum));
	  }
	}
      }
    }
}
  
void fcsBuilder::stoprun(daqReader *rdr) {
}

void fcsBuilder::main(int argc, char *argv[])
{
  fcsBuilder me;  
  me.Main(argc, argv);
}

