// \class StFmsEventDisplay
// \author Akio Ogawa
//
//  $Id: StFmsEventDisplay.cxx,v 1.6 2017/05/26 18:55:53 akio Exp $
//  $Log: StFmsEventDisplay.cxx,v $
//  Revision 1.6  2017/05/26 18:55:53  akio
//  added protection for a crash in MC mudst file reading
//
//  Revision 1.5  2016/06/08 16:31:50  akio
//  c++11 style initialization
//
//  Revision 1.4  2016/01/20 19:56:39  akio
//  *** empty log message ***
//
//  Revision 1.2  2016/01/20 19:46:40  akio
//  *** empty log message ***
//
//  Revision 1.1  2015/10/20 19:55:51  akio
//  Initial version of FMS event display
//
//

#include "StFmsEventDisplay.h"

#include "StMessMgr.h"
#include "Stypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StThreeVectorF.hh"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StEnumerations.h"
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFmsPointPair.h"

#include "TApplication.h"
#include "TCanvas.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TLine.h"
#include "TBox.h"
#include "TMarker.h"
#include "TString.h"
#include "TColor.h"
#include "TStyle.h"
#include "TText.h"
#include "TROOT.h"

ClassImp(StFmsEventDisplay);

StFmsEventDisplay::StFmsEventDisplay(const Char_t* name):
    StMaker(name),mFilename((char *)"fmsEventDisplay.pdf"){}

StFmsEventDisplay::~StFmsEventDisplay(){}

Int_t StFmsEventDisplay::Init(){  
    mFmsDbMaker=static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
    if(!mFmsDbMaker){
	LOG_ERROR  << "StFmsEventDisplay::InitRun Failed to get StFmsDbMaker" << endm;
	return kStFatal;
    }
    
    mApplication = new TApplication("EvtDsp",0,0);
    mCanvas=new TCanvas("FMSEventtDisplay","FMSEventtDisplay",10,10,900,900);
    gStyle->SetOptStat(0);
    return kStOK;
}

Int_t StFmsEventDisplay::Finish(){
    mApplication->Terminate();
    return kStOK;
}

void setColor(int id, float v){ //blue->green for 0->0.5 and green->red for 0.5->1.0
    TColor *color = gROOT->GetColor(id);
    if(!color) color=new TColor(id,0,0,0);    
    float r=0.0,g=0.0,b=0.0;
    if(v>1.0) v=1.0;
    if(v<0.0) v=0.0;
    if(v>0.5){
	r=(v-0.5)*2.0;
	g=1.0-(v-0.5)*2.0;
    }else{
	g=v*2.0;
	b=1.0-(v*2.0);
    }
    color->SetRGB(r,g,b);
    //printf("%d %f %f %f %f\n",id,v,r,g,b);
}

void scale(float x=100.0, float dx=5.0, float ymin=-100.0, float ymax=100.0, int ny=40){
    for(int i=0; i<ny; i++){
	setColor(2000+i,float(i+0.5)/float(ny));
	float dy=(ymax-ymin)/float(ny);
	TBox* b=new TBox(x,ymin+dy*i,x+dx,ymin+dy*(i+1));
	b->SetFillColor(2000+i);
	b->Draw();
    }
}

Int_t StFmsEventDisplay::Make(){
    int bunch=-1;
    StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
    if(mudst) {
	if(mudst->event()->triggerData()){
	    bunch = mudst->event()->triggerData()->bunchId7Bit();
	    LOG_INFO << Form("Bunch=%3d from Mudst\n",bunch) << endm;
	}
    }

    StEvent* event = (StEvent*)GetInputDS("StEvent");
    if(!event) {LOG_ERROR << "StFmsEventDisplay::Make did not find StEvent"<<endm; return kStErr;}
    mFmsColl = event->fmsCollection();
    if(!mFmsColl) {LOG_ERROR << "StFmsEventDisplay::Make did not find StEvent->FmsCollection"<<endm; return kStErr;}
    if(bunch==-1){
	if(event->triggerData()) {
	    bunch = event->triggerData()->bunchId7Bit();
	    LOG_INFO << Form("Bunch=%3d from StEvent\n",bunch) <<endm;
	}
    }

    StSPtrVecFmsHit& hits = mFmsColl->hits();
    StSPtrVecFmsCluster& clusters = mFmsColl->clusters();
    //StSPtrVecFmsPoint& points = mFmsColl->points(); 
    //StSPtrVecFpsSlat& slats = mFmsColl->fpsSlats(); 
    //vector<StFmsPointPair*>& pairs = mFmsColl->pointPairs();
    int nh=mFmsColl->numberOfHits();
    int nc=mFmsColl->numberOfClusters();
    int np=mFmsColl->numberOfPoints();
    //int ns=slats.size();
    //int npair=mFmsColl->numberOfPointPairs();
    
    if(mNAccepted < mMaxEvents){
	//filters
	if(mFilter==1 && np<2) {mNEvents++; return kStOK;}
	if(mFilter==2 && (bunch<30 || bunch>=40)) {mNEvents++; return kStOK;}

	char cc[100]; 
	sprintf(cc,"FMSEventDisplayEvt=%d",mNEvents);
	mCanvas->Clear();	
	//frame
	TH2F* frame=new TH2F(cc,cc,1,-100.0,105.0,1,-100.0,100.0);
	frame->Draw();
	scale();
	TText* t1=new TText(106, 100.0,"100GeV"); t1->SetTextSize(0.03); t1->Draw();
	TText* t2=new TText(106, 33.3, "10GeV");  t2->SetTextSize(0.03); t2->Draw();
	TText* t3=new TText(106,-33.3, "1GeV");   t3->SetTextSize(0.03); t3->Draw();
	TText* t4=new TText(106,-100.0,"0.1GeV"); t4->SetTextSize(0.03); t4->Draw();

	for(int det=kFmsNorthLargeDetId; det<=kFmsSouthSmallDetId; det++){
	    for(int ch=0; ch<mFmsDbMaker->maxChannel(det); ch++){
		if(mFmsDbMaker->getGain(det,ch)>0.0){
		    StThreeVectorF xyz = mFmsDbMaker->getStarXYZ(det,ch);
		    float wx=mFmsDbMaker->getXWidth(det);
		    float wy=mFmsDbMaker->getYWidth(det);		
		    TBox* cell=new TBox(xyz.x()-wx/2.0, xyz.y()-wy/2.0, xyz.x()+wx/2.0, xyz.y()+wy/2.0);
		    cell->SetFillColor(0);
		    cell->SetLineColor(1); cell->SetLineWidth(1);
		    cell->Draw("l");
		}
	    }
	}
		
	//First draw hits 
	for(int i=0; i<nh; i++){
	    StFmsHit* hit=hits[i];
	    int det=hit->detectorId();
	    if(det>=kFmsNorthLargeDetId && det<=kFmsSouthSmallDetId){
		//int ch=hit->channel();
		StThreeVectorF xyz = mFmsDbMaker->getStarXYZ(hit);
		float wx=mFmsDbMaker->getXWidth(det);
		float wy=mFmsDbMaker->getYWidth(det);
		TBox* cell=new TBox(xyz.x()-wx/2.0, xyz.y()-wy/2.0, xyz.x()+wx/2.0, xyz.y()+wy/2.0);
		int color = 1000+i;
		float ladc=log10(hit->energy());
		if(ladc>2.0) ladc=2.0;
		if(ladc<-1.0) ladc=-1.0;
		setColor(color,(ladc+1.0)/3.0);
		cell->SetFillColor(color);
		cell->Draw();
	    }	    
	}

	for(int i=0; i<nc; i++){
	    StFmsCluster* clu=clusters[i];
	    float nt=float(clu->nTowers());
	    for(int j=0; j<nt; j++){
		StFmsHit* hit = clu->hits()[j];
		int det=hit->detectorId();
		StThreeVectorF xyz = mFmsDbMaker->getStarXYZ(hit);
                float wx=mFmsDbMaker->getXWidth(det);
                float wy=mFmsDbMaker->getYWidth(det);
                TBox* cell=new TBox(xyz.x()-wx/2.0, xyz.y()-wy/2.0, xyz.x()+wx/2.0, xyz.y()+wy/2.0);		
		cell->SetFillStyle(0);
		cell->SetLineColor(2+i);
		cell->SetLineWidth(2);
                cell->Draw("l");		
	    }
	    int nphoton=clu->nPhotons();
	    for(int j=0; j<nphoton; j++){
		StFmsPoint* point = clu->points()[j];
		TMarker* m=new TMarker(point->XYZ().x(),point->XYZ().y(),29);
		m->SetMarkerColor(1);
		m->SetMarkerSize(2);
		m->Draw();
	    }
	}

	mCanvas->Update();
	TString f(mFilename);
	if(f.Contains(".pdf")){ //not working
	    if(mNEvents==0) f.Append("(");
	    else if(mNEvents==mMaxEvents-1) f.Append(")");
	    LOG_INFO << "Saving " << f.Data() << endm;
	    mCanvas->Print(f.Data(),"pdf");	
	}else if(f.Contains(".png")){
	    f.ReplaceAll("eventDisplay.png",Form("%d.eventDisplay.png",mNEvents));
	    LOG_INFO << "Saving " << f.Data() << endm;
	    mCanvas->SaveAs(f.Data());
	}
	mNAccepted++;
	mNEvents++;	
	//std::cin.get();
    }
    return kStOK;
}
