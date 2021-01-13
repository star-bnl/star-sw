#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_FCS/daq_fcs.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "fcsBuilder.h"
#include <RTS/include/rtsLog.h>

#include "fcsMap.h"

const int TBMAX=300;
const int TBTRG[2]={50,207+8};
const int MAXSUM=20000;
const int OFF[2]  = {0,0};
const int mReadMode=0;
const int mFakeData=0;
    
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

    int ecal_xmax = nColumn(0)+OFF[0];
    int ecal_ymax = nRow(0);
    contents.h_fcs_det_hitmap[0] = new TH2F("Ecal",Form("Ecal; +-Col   North <-> South; %d-Row",nRow(0)+1),
					    ecal_xmax*2+1,-ecal_xmax-0.5,ecal_xmax+0.5,
					    ecal_ymax,0.0,ecal_ymax); 
    int hcal_xmax = nColumn(2)+OFF[1];
    int hcal_ymax = nRow(2);
    contents.h_fcs_det_hitmap[1] = new TH2F("Hcal",Form("Hcal; +-Col   North <-> South; %d-Row",nRow(2)+1),
					    hcal_xmax*2+1,-hcal_xmax-0.5,hcal_xmax+0.5,
					    hcal_ymax,0.0,hcal_ymax); 
    int pres_xmax = nColumn(4);
    int pres_ymax = nRow(4);
    contents.h_fcs_det_hitmap[2] = new TH2F("Pres","Pres; +-Radius (North <-> South); Phi",
					    pres_xmax*2+1,-pres_xmax-0.5,pres_xmax+0.5,
					    pres_ymax,0.0,pres_ymax); 
    //contents.h_fcs_det_hitmap[2] = new TH2F("Pres","Pres; View from back",
    //				    pres_ymax*2,-pres_ymax,pres_ymax,
    //					    pres_xmax+1,0.0,pres_xmax+1);
     
    char csum[30];
    sprintf(csum,"AdcSum(TB=%d-%d)",TBTRG[0]-3,TBTRG[0]+4);
    contents.h_fcs_ehpns_id_adcsum[0][0] = new TH2F("Ecal_N_Id_Adcsum",Form("EcalNorth; Id/22; %s",csum),maxId(0),0.0,maxId(0)/22.0,500,0.0,MAXSUM);
    contents.h_fcs_ehpns_id_adcsum[0][1] = new TH2F("Ecal_S_Id_Adcsum",Form("EcalSouth; Id/22; %s",csum),maxId(1),0.0,maxId(1)/22.0,500,0.0,MAXSUM);
    contents.h_fcs_ehpns_id_adcsum[1][0] = new TH2F("Hcal_N_Id_Adcsum",Form("HcalNorth; Id/13; %s",csum),maxId(2),0.0,maxId(2)/13.0,500,0.0,MAXSUM);
    contents.h_fcs_ehpns_id_adcsum[1][1] = new TH2F("Hcal_S_Id_Adcsum",Form("HcalSouth; Id/13; %s",csum),maxId(3),0.0,maxId(3)/13.0,500,0.0,MAXSUM);
    contents.h_fcs_ehpns_id_adcsum[2][0] = new TH2F("Pres_N_Id_Adcsum",Form("PresNorth; Id/16; %s",csum),maxId(4),0.0,maxId(4)/16.0,500,0.0,MAXSUM);
    contents.h_fcs_ehpns_id_adcsum[2][1] = new TH2F("Pres_S_Id_Adcsum",Form("PresSouth; Id/16; %s",csum),maxId(5),0.0,maxId(5)/16.0,500,0.0,MAXSUM);
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
    
    mCrtDepT=n;
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
    
    for(int i=0; i<n; i++){
      plots[i]->optlogz = 1;
      plots[i]->optstat = 0;
	LOG(DBG, "Adding plot %d = %s",i,plots[i]->GetPlotName());
	addPlot(plots[i]);
    }
}
  
void fcsBuilder::startrun(daqReader *rdr) {
    LOG(NOTE, "fcsBuilder starting run #%d",rdr->run);
    resetAllPlots();    
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

// Fill Histograms...
void fcsBuilder::event(daqReader *rdr){  
    int fcs_size = rdr->getDetectorSize("fcs");
    int sz = rdr->getDetectorSize("/");
    LOG(DBG, "getDetectorSize(fcs) = %d,  evtSize = %d (diff=%d)", sz, rdr->event_size,rdr->event_size-sz);
    contents.h_evt_size->Fill(safelog(fcs_size));
    
    string mode[2]={"adc","zs"};
    dd = rdr->det("fcs")->get(mode[mReadMode].c_str());
    if(!dd){
      LOG(DBG,"No fcs_%s found",mode[mReadMode].c_str());
    }else{
        int trgcmd = rdr->trgcmd;      
        if(trgcmd!=4 && trgcmd !=10) return;
	static int phyled=-1;
	if(phyled<0){
	  if(trgcmd==4) {
	    phyled=0;
	    contents.h_fcs_crt_depch_tbin[0]->GetYaxis()->SetRange(0,100);
	    contents.h_fcs_crt_depch_tbin[1]->GetYaxis()->SetRange(0,100);
	    contents.h_fcs_crt_depch_tbin[3]->GetYaxis()->SetRange(0,100);
	    contents.h_fcs_crt_depch_tbin[4]->GetYaxis()->SetRange(0,100);
	  }else{
	    phyled=1;
	    char csum[30];    
	    sprintf(csum,"AdcSum(TB=%d-%d)",TBTRG[1]-3,TBTRG[1]+4);
	    contents.h_fcs_ehpns_id_adcsum[0][0]->SetTitle(Form("EcalNorth; Id/22; %s",csum));
	    contents.h_fcs_ehpns_id_adcsum[0][1]->SetTitle(Form("EcalSouth; Id/22; %s",csum));
	    contents.h_fcs_ehpns_id_adcsum[1][0]->SetTitle(Form("HcalNorth; Id/13; %s",csum));
	    contents.h_fcs_ehpns_id_adcsum[1][1]->SetTitle(Form("HcalSouth; Id/13; %s",csum));
	    contents.h_fcs_ehpns_id_adcsum[2][0]->SetTitle(Form("PresNorth; Id/16; %s",csum));
	    contents.h_fcs_ehpns_id_adcsum[2][1]->SetTitle(Form("PresSouth; Id/16; %s",csum));
	    contents.h_fcs_crt_depch_tbin[0]->GetYaxis()->SetRange(150,300);
	    contents.h_fcs_crt_depch_tbin[1]->GetYaxis()->SetRange(150,300);
	    contents.h_fcs_crt_depch_tbin[3]->GetYaxis()->SetRange(150,300);
	    contents.h_fcs_crt_depch_tbin[4]->GetYaxis()->SetRange(150,300);
	  }	  
	  /*
	  TLine* l1 = new TLine(0,TBTRG[phyled]-3,20,TBTRG[phyled]-3); l1->SetLineColor(kMagenta);
	  TLine* l2 = new TLine(0,TBTRG[phyled]+4,20,TBTRG[phyled]+4); l2->SetLineColor(kMagenta);	  
	  mPlots[mCrtDepT+0]->addElement(l1); mPlots[mCrtDepT+0]->addElement(l2);
	  mPlots[mCrtDepT+1]->addElement(l1); mPlots[mCrtDepT+1]->addElement(l2);
	  mPlots[mCrtDepT+2]->addElement(l1); mPlots[mCrtDepT+2]->addElement(l2);
	  mPlots[mCrtDepT+3]->addElement(l1); mPlots[mCrtDepT+3]->addElement(l2);
	  */
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
		    adc = dd->adc[i].adc;    
		}
		contents.h_fcs_crt_depch_tbin[crt]->Fill(slt+float(ch+0.5)/32.0,float(tb),float(adc));		
		if(TBTRG[phyled]-3 <= tb && tb <= TBTRG[phyled]+4) sum+=adc;
	    }
	    int c = getColumnNumber(detid,id);
	    int r = getRowNumber(detid,id);
	    float x = (c+OFF[ehp]) * (ns*2-1);
	    float y = (nRow(detid) - r + 0.5);
	    contents.h_fcs_det_hitmap[ehp]->Fill(x,y,float(sum));		
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
	      float y = (nRow(detid) - r + 0.5);
	      contents.h_fcs_det_hitmap[ehp]->Fill(x,y,float(adc));		
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

