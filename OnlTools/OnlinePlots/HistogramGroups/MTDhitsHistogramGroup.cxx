#include "MTDhitsHistogramGroup.h"

#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <algorithm>

#include "TVirtualPad.h"
#include "TLine.h"
#include "TLatex.h"
#include "TStyle.h"

#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "daqFormats.h"
#  include "cfgutil.h"
#else
#  include "DAQ_READER/daqReader.h"
//#  include "DAQ_TRG/trgReader.h"
//#  include "DAQ_TOF/tofReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_MTD/daq_mtd.h"
#  include "DAQ_READER/cfgutil.h"
#  include "StEvent/StTriggerData.h"
//#  include "DAQ_L3/l3Reader.h"
#  include "TriggerData.h"
#endif
#include "TMapFile.h"
#include "EvpUtil.h"
#include "HistoHandler.h"




using namespace std;

ClassImp(MTDhitsHistogramGroup) ;

MTDhitsHistogramGroup::MTDhitsHistogramGroup() {
	// For ROOT I/O
//	memset( MTD_hitmap, 0, sizeof(MTD_hitmap));
}

MTDhitsHistogramGroup::MTDhitsHistogramGroup(const char* group, const char* subGroup, const char* trigger, const char* detector)
: HistogramGroup(group,subGroup,trigger,detector) {
	
	char tmpchr[200];
	sprintf(tmpchr,"MTD26E_EastEnd_hitmap");
	MTD26E_hitmap[0][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	MTD26E_hitmap[0][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	sprintf(tmpchr,"MTD26E_WestEnd_hitmap");
	MTD26E_hitmap[1][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	MTD26E_hitmap[1][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	
	sprintf(tmpchr,"MTD26C_EastEnd_hitmap");
	MTD26C_hitmap[0][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	MTD26C_hitmap[0][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	sprintf(tmpchr,"MTD26C_WestEnd_hitmap");
	MTD26C_hitmap[1][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	MTD26C_hitmap[1][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	
	//the 3rd tray is rotated: HiZ - WestEnd
	sprintf(tmpchr,"MTD26W_WestEnd_hitmap");
	MTD26W_hitmap[0][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	MTD26W_hitmap[0][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	sprintf(tmpchr,"MTD26W_EastEnd_hitmap");
	MTD26W_hitmap[1][0]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	MTD26W_hitmap[1][1]=new TH1F(tmpchr,tmpchr,12,0.5,12.5);
	
	sprintf(tmpchr,"MTD1_EastEnd_hitmap");
	MTD1_hitmap[0][0]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
	MTD1_hitmap[0][1]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
	sprintf(tmpchr,"MTD1_WestEnd_hitmap");
	MTD1_hitmap[1][0]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
	MTD1_hitmap[1][1]=new TH1F(tmpchr,tmpchr,18,0.5,18.5);
	
	MTD_ToT    =new TH2F("MTD_ToT","MTD ToT vs Chan #",36,-0.5,35.5,70,0,70);
	MTD_ToT->SetXTitle("MTD Chan#");
	//MTD_ToT->SetYTitle("ToT (ns)");
	
	MTD_eastT_vs_westT=new TH2F("MTD_eastT_vs_westT","MTD eastT vs westT",1024,0,51200,1024,0,51200);
	MTD_eastT_vs_westT->SetXTitle("east time (ns)");
	//MTD_eastT_vs_westT->SetYTitle("west time (ns)");
	
	MTD_eastT_westT=new TH1F("MTD_eastT_westT","MTD eastT - westT",40,-10,10);
	MTD_eastT_westT->SetXTitle("(east-west) time diff (ns)");
	//MTD_eastT_vs_westT->SetYTitle("west time (ns)");
	
	MTD_hits_vs_TOF_hits=new TH2F("MTD_hits_vs_TOF_hits","MTD chan vs TOF MRPC chan ",36,-0.5,35.5,960,-0.5,959.5);
	MTD_hits_vs_TOF_hits->SetXTitle("MTD chan #");
	
}


MTDhitsHistogramGroup::~MTDhitsHistogramGroup() {
	
	for (int i = 0; i < 2; ++i)delete MTD26E_hitmap[0][i];
	delete MTD_ToT;
}


void MTDhitsHistogramGroup::reset() {
	
	for (int i = 0; i < 2; ++i)MTD26E_hitmap[0][i]->Reset();
	MTD_ToT->Reset();
}


void MTDhitsHistogramGroup::draw(TCanvas* cc) {
	
	TLatex label;
	label.SetTextAlign(23);  // center, top
	label.SetTextSize(0.055);
	label.SetTextColor(45);
	TLatex labely;
	//labely.SetTextAlign(23);  // center, top
	labely.SetTextSize(0.04);
	labely.SetTextColor(1);
	labely.SetTextAngle(90);
	
	TLine  line;
	line.SetLineColor(4);
	line.SetLineWidth(1);
	//
	gROOT->SetStyle("Plain");
	gStyle->SetPaperSize(TStyle::kUSLetter);
	
	gStyle->SetPalette(1);
	gStyle->SetLabelSize(0.09,"y");
	gStyle->SetLabelSize(0.09,"x");
	gStyle->SetLabelSize(0.06,"xyz");
	gStyle->SetLabelSize(0.06,"y");
	gStyle->SetLabelSize(0.08,"x");
	gStyle->SetLabelOffset(0.01,"x");
	gStyle->SetLabelOffset(0.01,"y");
	
	gStyle->SetOptTitle(11);
	gStyle->SetTitleX(0.1); gStyle->SetTitleY(1.);
	gStyle->SetTitleW(0.8); gStyle->SetTitleH(0.086);
	//gStyle->SetTitleSize(0.06);
	
	gStyle->SetOptStat(110110);
	gStyle->SetStatX(0.99); gStyle->SetStatY(0.91);
	gStyle->SetStatW(0.18); gStyle->SetStatH(0.14);
	
	gStyle->SetNdivisions(505,"xyz");
	
	
	gStyle->SetPadGridX(0);
	gStyle->SetPadGridY(0);
	
	cc->cd(); //cc->SetFillColor(0);
	cc->Clear();
	cc->Divide(2, 4,0.00005,0.00005);
	cc->cd(1);
	
//	MTD26E_hitmap[0][0]->SetMinimum(0);
	MTD26E_hitmap[0][0]->SetLineColor(2);
	MTD26E_hitmap[0][0]->GetYaxis()->SetLabelSize(0.07);
	MTD26E_hitmap[0][0]->GetXaxis()->SetLabelSize(0.055);
	MTD26E_hitmap[0][0]->Draw();
	MTD26E_hitmap[0][1]->SetLineColor(4);
	MTD26E_hitmap[0][1]->Draw("same");

	
	cc->cd(2);
//	MTD26E_hitmap[1][0]->SetMinimum(0);
	MTD26E_hitmap[1][0]->SetLineColor(2);
	MTD26E_hitmap[1][0]->GetYaxis()->SetLabelSize(0.07);
	MTD26E_hitmap[1][0]->GetXaxis()->SetLabelSize(0.055);
	MTD26E_hitmap[1][0]->Draw();
	MTD26E_hitmap[1][1]->SetLineColor(4);
	MTD26E_hitmap[1][1]->Draw("same");
	
	cc->cd(3);
//	MTD26C_hitmap[0][0]->SetMinimum(0);
	MTD26C_hitmap[0][0]->SetLineColor(2);
	MTD26C_hitmap[0][0]->GetYaxis()->SetLabelSize(0.07);
	MTD26C_hitmap[0][0]->GetXaxis()->SetLabelSize(0.055);
	MTD26C_hitmap[0][0]->Draw();
	MTD26C_hitmap[0][1]->SetLineColor(4);
	MTD26C_hitmap[0][1]->Draw("same");

	cc->cd(4);
//	MTD26C_hitmap[1][0]->SetMinimum(0);
	MTD26C_hitmap[1][0]->SetLineColor(19);
	MTD26C_hitmap[1][0]->GetYaxis()->SetLabelSize(0.07);
	MTD26C_hitmap[1][0]->GetXaxis()->SetLabelSize(0.055);
	MTD26C_hitmap[1][0]->Draw();
	MTD26C_hitmap[1][1]->SetLineColor(4);
	MTD26C_hitmap[1][1]->Draw("same");

	cc->cd(5);
	MTD26W_hitmap[1][0]->SetMinimum(0);
	MTD26W_hitmap[1][0]->SetLineColor(2);
	MTD26W_hitmap[1][0]->GetYaxis()->SetLabelSize(0.07);
	MTD26W_hitmap[1][0]->GetXaxis()->SetLabelSize(0.055);
	MTD26W_hitmap[1][0]->Draw();
	MTD26W_hitmap[1][1]->SetLineColor(4);
	MTD26W_hitmap[1][1]->Draw("same");
//	MTD26W_hitmap[0][0]->SetMinimum(0);

	cc->cd(6);
	MTD26W_hitmap[0][0]->SetLineColor(2);
	MTD26W_hitmap[0][0]->GetYaxis()->SetLabelSize(0.07);
	MTD26W_hitmap[0][0]->GetXaxis()->SetLabelSize(0.055);
	MTD26W_hitmap[0][0]->Draw();
	MTD26W_hitmap[0][1]->SetLineColor(4);
	MTD26W_hitmap[0][1]->Draw("same");

	cc->cd(7);
	MTD1_hitmap[0][0]->SetMinimum(0);
	MTD1_hitmap[0][0]->SetLineColor(2);
	MTD1_hitmap[0][0]->GetYaxis()->SetLabelSize(0.07);
	MTD1_hitmap[0][0]->GetXaxis()->SetLabelSize(0.055);
	MTD1_hitmap[0][0]->Draw();
	MTD1_hitmap[0][1]->SetLineColor(4);
	MTD1_hitmap[0][1]->Draw("same");

	cc->cd(8);
	MTD1_hitmap[1][0]->SetMinimum(0);
	MTD1_hitmap[1][0]->SetLineColor(2);
	MTD1_hitmap[1][0]->GetYaxis()->SetLabelSize(0.07);
	MTD1_hitmap[1][0]->GetXaxis()->SetLabelSize(0.055);
	MTD1_hitmap[1][0]->Draw();
	MTD1_hitmap[1][1]->SetLineColor(4);
	MTD1_hitmap[1][1]->Draw("same");
	
	cc->Update();
	
} 


bool MTDhitsHistogramGroup::fill(evpReader* evp, char* datap) { 
	int timeinbin=0;
	float time=0.;
	int halftrayid=-1;
	int trayid=-1;
	leadinghits.clear();
	trailinghits.clear();
	
	daq_dta *dd = evp->det("mtd")->get("legacy");//evp->det("tof")
	mtd_t *mtd;
	if(dd) {
		while(dd->iterate()) {
			mtd = (mtd_t *)dd->Void;
			
			int ifib=0;
			int ndataword = mtd->ddl_words[ifib];    
			if(ndataword<=0) continue;
			for(int iword=0;iword<ndataword;iword++){
				int dataword=mtd->ddl[ifib][iword];
				
				if( (dataword&0xF0000000)>>28 == 0x2) continue;  //TDC header
				if( (dataword&0xF0000000)>>28 == 0xD) continue;  //Header tag
				if( (dataword&0xF0000000)>>28 == 0xE) continue;  //TDIG Separator
				if( (dataword&0xF0000000)>>28 == 0xA) {  // header trigger data flag
					// do nothing at this moment.
					continue;
				}
				
				// geographical data words for tray number.
				if( (dataword&0xF0000000)>>28 == 0xC) { //Geographical Data
					halftrayid = dataword&0x01;    
					trayid     = (dataword&0x0FE)>>1;
					continue;
				}
				
				if( (dataword&0xF0000000)>>28 == 0x6) {continue;} //error
				//
				int edgeid =int( (dataword & 0xf0000000)>>28 );
				if((edgeid !=4) && (edgeid!=5)) continue; //leading edge or trailing edge
				
				int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
				int tdigboardid=tdcid/4;   // for halftray 0, 0-2 for tdig0; 4-6 for tdig1
				if (halftrayid==1) tdigboardid=4; //for halftray1, 0-2 for tdig 4
				int  tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
				//int globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray
				timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
				time = timeinbin * 25./1024;   // time in ns 
				
				//int moduleid=-1;
				int globalstripid=-1;
				int stripid=-1;
				int zendid=-1;
//				
				if(trayid==26){
					globalstripid=tdcchan2globalstrip(tdigboardid,tdcid,tdcchan,trayid);
				stripid=(globalstripid-1)%12+1;
				zendid=(globalstripid-1)/12; //0 for Hi Z end; 1 for Lo Z end
				}
				if(trayid==1){
					globalstripid=tdcchan2globalstrip(tdigboardid,tdcid,tdcchan,trayid);
					stripid=globalstripid;
					zendid=tdigboardid/4; //0 for east end; 1 for west end
				}
				
//				cout<<"=======MTD========:: trayid="<<trayid<<" tdigboardid="<<tdigboardid<<" zendid="<<zendid<<
//				" halftray="<<halftrayid<<" tdcid="<<tdcid<<" tdcchan="<<tdcchan<<" stripid="<<stripid <<endl;
				
				if(trayid==26&&tdigboardid==0) MTD26E_hitmap[zendid][edgeid-4]->Fill(stripid);
				if(trayid==26&&tdigboardid==1) MTD26C_hitmap[zendid][edgeid-4]->Fill(stripid);
				if(trayid==26&&tdigboardid==4) MTD26W_hitmap[zendid][edgeid-4]->Fill(stripid);
				if(trayid==1&&tdigboardid==0) MTD1_hitmap[0][edgeid-4]->Fill(stripid);
				if(trayid==1&&tdigboardid==4) MTD1_hitmap[1][edgeid-4]->Fill(stripid);
			}  // end loop nword
		}
	}
	
	return true;
	
}

int MTDhitsHistogramGroup::tdcchan2globalstrip(int tdigboardid,int tdcid,int tdcchan,int trayid)
{
	int globalstripid=-1;
	if(trayid==26){
		if (tdcid>3) tdcid=tdcid-4; //scale to H#
		int globaltdcchan=(tdcid+1)*10+tdcchan;
		int mtdstrip[24]={34,22,10,37,27,17,33,23,16,36,26,15,
			21,12,32,20,14,35,25,13,30,24,11,31};
		for(int i=0;i<24;i++){
			if(mtdstrip[i]==globaltdcchan) {globalstripid=i+1;break;}
		}
	}
	
	if(trayid==1){
		int globaltdcchan=(tdcid+1)*10+tdcchan;
		int mtdstrip[18]=  {34,22,10,37,27,17,33,23,16,36,26,15,32,20,30,24,11,31};
		for(int i=0;i<18;i++){
			if(mtdstrip[i]==globaltdcchan) {globalstripid=i+1;break;}
		}
	}
	
	return globalstripid;
}

