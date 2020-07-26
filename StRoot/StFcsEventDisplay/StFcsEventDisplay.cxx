// \class StFmsEventDisplay
// \author Akio Ogawa
//
//  $Id: StFcsEventDisplay.cxx,v 1.13 2020/05/29 18:54:29 akio Exp $
//  $Log: StFcsEventDisplay.cxx,v $
//  Revision 1.13  2020/05/29 18:54:29  akio
//  Adding EPD as PRES
//
//  Revision 1.12  2019/10/23 19:20:50  akio
//  forgot to add display offset for point
//
//  Revision 1.11  2019/10/23 13:30:49  akio
//  adding display of FcsPoint
//
//  Revision 1.10  2019/08/01 18:38:22  akio
//  Added STGC
//
//  Revision 1.9  2019/07/02 14:43:16  akio
//  fixed TColor id ovewrite problem, fixed pdf file creation
//
//  Revision 1.8  2019/06/27 16:13:27  akio
//  minor updates on not printing energy for ecal (unless run19) for visibility
//
//  Revision 1.7  2019/06/26 18:01:38  akio
//  make clusterid/energy printing smaller for run21
//
//  Revision 1.6  2019/06/25 16:39:30  akio
//  Added run19 version, and drawing cluster Id and tower energy
//
//  Revision 1.5  2019/06/21 18:53:25  akio
//  added run19 version
//
//  Revision 1.4  2019/06/07 18:13:23  akio
//  added filter
//
//  Revision 1.3  2019/05/17 15:58:06  akio
//  removing TApplication
//
//  Revision 1.2  2019/05/16 17:24:01  akio
//  minor bug fix drawing det=6 which doesn't exist
//
//  Revision 1.1  2018/11/14 16:50:14  akio
//  FCS codes in offline/upgrade/akio
//

#include "StFcsEventDisplay.h"

#include "StEvent/StEnumerations.h"
#include "StMessMgr.h"
#include "Stypes.h"
#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StThreeVectorF.hh"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StStgcDbMaker/StStgcDbMaker.h"

#include "StEpdUtil/StEpdGeom.h"

//#include "TApplication.h"
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

ClassImp(StFcsEventDisplay);

StFcsEventDisplay::StFcsEventDisplay(const Char_t* name):
    StMaker(name),mFilename((char *)"fcsEventDisplay.pdf"){}

StFcsEventDisplay::~StFcsEventDisplay(){}

Int_t StFcsEventDisplay::Init(){  
    mFcsDbMaker=static_cast<StFcsDbMaker*>(GetMaker("fcsDb"));  
    if(!mFcsDbMaker){
	LOG_ERROR  << "StFcsEventDisplay::InitRun Failed to get StFcsDbMaker" << endm;
	return kStFatal;
    }
    mStgcDbMaker=static_cast<StStgcDbMaker*>(GetMaker("stgcDb"));  
    if(!mStgcDbMaker){
	LOG_WARN  << "StFcsEventDisplay::InitRun Failed to get StStgcDbMaker" << endm;
	//return kStFatal;
    }
    
    //mApplication = new TApplication("EvtDsp",0,0);
    mCanvas=new TCanvas("FCSEventtDisplay","FCSEventtDisplay",10,10,2000,2000);
    gStyle->SetOptStat(0);

    //EPD geom
    mEpdgeo=new StEpdGeom;

    return kStOK;
}

Int_t StFcsEventDisplay::Finish(){
    //mApplication->Terminate();
    return kStOK;
}

void setColor(int id, float v, float alpha=1.0){ //blue->green for 0->0.5 and green->red for 0.5->1.0
    static const float PI=3.141592654;
    TColor *color = gROOT->GetColor(id);
    if(!color) color=new TColor(id,0,0,0);    
    float r=0.0,g=0.0,b=0.0;
    if(v>1.0) v=1.0;
    if(v<0.0) v=0.0;
    if(v>0.5){
	r=sin((v-0.5)/0.5*PI/2.0);
	g=cos((v-0.5)/0.5*PI/2.0);
    }else{
	g=sin(v/0.5*PI/2.0);
	b=cos(v/0.5*PI/2.0);
    }
    color->SetRGB(r,g,b);
    color->SetAlpha(alpha);
    //printf("%d %f %f %f %f\n",id,v,r,g,b);
}

void scale(float x=160.0, float dx=5.0, float ymin=-220.0, float ymax=220.0, int ny=40){
    for(int i=0; i<ny; i++){
	setColor(5000+i,float(i+0.5)/float(ny));
	float dy=(ymax-ymin)/float(ny);
	TBox* b=new TBox(x,ymin+dy*i,x+dx,ymin+dy*(i+1));
	b->SetFillColor(5000+i);
	b->Draw();
    }
}

Int_t StFcsEventDisplay::Make(){
    StEvent* event = (StEvent*)GetInputDS("StEvent");
    if(!event) {LOG_ERROR << "StFcsEventDisplay::Make did not find StEvent"<<endm; return kStErr;}
    mFcsColl = event->fcsCollection();
    if(!mFcsColl) {LOG_ERROR << "StFcsEventDisplay::Make did not find StEvent->StFcsCollection"<<endm; return kStErr;}
    mStgcColl = event->stgcCollection();
    if(!mStgcColl) {LOG_ERROR << "StFcsEventDisplay::Make did not find StEvent->StStgcCollection"<<endm;}
    
    if(mNAccepted < mMaxEvents){
	mNEvents++;	
	if(mFilter==1 && 
	   mFcsColl->numberOfHits(0)+ mFcsColl->numberOfHits(1)+ mFcsColl->numberOfHits(2)+ mFcsColl->numberOfHits(3)==0) return kStOK;
	mNAccepted++;

	mCanvas->Clear();	
	char cc[100]; 
	sprintf(cc,"FCSEventDisplayEvt=%d",mNEvents);
	mFcsColl->print(); 

	//frame	
	float xmin=-160;
	float xmax=160;
	float ymin=-220;
	float ymax=220;
	float yoff[kFcsNDet]={100.0,100.0,-110.0,-110.0,100.0,100.0};
	float xoff[kFcsNDet]={  0.0,  0.0,   0.0,   0.0,  0.0,  0.0};
	float stgcOffX=0.0;
	float stgcOffY=0.0;
	float cTxSize=0.01;
	float eTxSize=0.007;
	TH2F* frame;	
	if(mRun19>0){
	  xmin=0;
	  xmax=120;
	  ymin=-110;
	  ymax=10;
	  xoff[1]=0;
	  xoff[3]=-50;
	  xoff[5]=0;
	  yoff[1]=0;
	  yoff[3]=0;
	  yoff[5]=40;
	  cTxSize=0.02;
	  eTxSize=0.015;
	}
	frame=new TH2F(cc,cc,1,xmin,xmax,1,ymin,ymax);
	float dy=ymax-ymin;
	
	frame->Draw();
	scale(xmax,5,ymin,ymax);
	TText* t1=new TText(xmax+5, ymax,          "100GeV"); t1->SetTextSize(0.02); t1->Draw();
	TText* t2=new TText(xmax+5, ymin+dy*2/3.0, "10GeV" ); t2->SetTextSize(0.02); t2->Draw();
	TText* t3=new TText(xmax+5, ymin+dy/3.0,   "1GeV"  ); t3->SetTextSize(0.02); t3->Draw();
	TText* t4=new TText(xmax+5, ymin,          "0.1GeV"); t4->SetTextSize(0.02); t4->Draw();
	
	int color = 1000;
	for(int det=0; det<kFcsNDet; det++){
	    StSPtrVecFcsHit& hits = mFcsColl->hits(det);
	    StSPtrVecFcsCluster& clusters = mFcsColl->clusters(det);
	    StSPtrVecFcsPoint& points = mFcsColl->points(det); 
	    int nh=mFcsColl->numberOfHits(det);
	    int nc=mFcsColl->numberOfClusters(det);
	    int np=mFcsColl->numberOfPoints(det);
	    if(mDebug>0) LOG_INFO << Form("StFcsEventDisplay Det=%1d nhit=%4d nclu=%3d",det,nh,nc) << endm;

	    //first all towers
	    if(det<=kFcsHcalSouthDetId){ //ecal & hcal
		for(int id=0; id<mFcsDbMaker->maxId(det); id++){
		    StThreeVectorF xyz = mFcsDbMaker->getStarXYZ(det,id);
		    float wx=mFcsDbMaker->getXWidth(det);
		    float wy=mFcsDbMaker->getYWidth(det);
		    TBox* cell=new TBox(xyz.x()-wx/2.0+xoff[det], xyz.y()+yoff[det]-wy/2.0, 
					xyz.x()+wx/2.0+xoff[det], xyz.y()+yoff[det]+wy/2.0);
		    cell->SetFillStyle(0);
		    //cell->SetFillColorAlpha(0,0.9); 
		    cell->SetLineColor(1); cell->SetLineWidth(1);
		    cell->Draw("l");
		}
	    }
	    
	    //draw hits 
	    for(int i=0; i<nh; i++){
		StFcsHit* hit=hits[i];
		if(det<=kFcsHcalSouthDetId){ //ecal & hcal
		    StThreeVectorF xyz = mFcsDbMaker->getStarXYZ(hit);
		    float wx=mFcsDbMaker->getXWidth(det);
		    float wy=mFcsDbMaker->getYWidth(det);
		    TBox* cell=new TBox(xyz.x()-wx/2.0+xoff[det], xyz.y()+yoff[det]-wy/2.0,
					xyz.x()+wx/2.0+xoff[det], xyz.y()+yoff[det]+wy/2.0);
		    float e=hit->energy();
		    float logE=log10(e);
		    if(logE>2.0) logE=2.0;
		    if(logE<-1.0) logE=-1.0;
		    setColor(color,(logE+1.0)/3.0);
		    cell->SetFillColor(color);
		    cell->Draw();
		    color++;
		    if(mRun19 || det>=kFcsHcalNorthDetId){
			TText* tx=new TText(xyz.x()+xoff[det],xyz.y()+yoff[det]-wy/4.0,Form("%4.2f",e));
			tx->SetTextSize(eTxSize); tx->SetTextAlign(22);
			tx->Draw();
		    }
		}else if(det==kFcsPresNorthDetId || det==kFcsPresSouthDetId){//EPD as Pres
		    double zepd=375.0;
		    double zfcs=710.0+13.90+15.0;
		    double zr=zfcs/zepd;
		    int pp,tt,id,n;
		    double x[5],y[5];
		    mFcsDbMaker->getEPDfromId(det,hit->id(),pp,tt);
		    mEpdgeo->GetCorners(100*pp+tt,&n,x,y);
		    for(int i=0; i<n; i++){
			int j=i+1;
			if(i==n-1) j=0;
			TLine *l=new TLine(zr*x[i]+xoff[det],zr*y[i]+yoff[det],
					   zr*x[j]+xoff[det],zr*y[j]+yoff[det]);
			l->SetLineColor(kRed);
			l->SetLineWidth(4);
			l->Draw();
		    }
		}
		if(mDebug>0) LOG_INFO << Form("StFcsEventDisplay Det=%1d nhit=%4d i=%3d color=%4d",det,nh,i,color) << endm;
	    }	    
	    
	    //clusters
	    for(int i=0; i<nc; i++){
		StFcsCluster* clu=clusters[i];
		float nt=float(clu->nTowers());
		for(int j=0; j<nt; j++){
		    StFcsHit* hit = clu->hits()[j];
		    StThreeVectorF xyz = mFcsDbMaker->getStarXYZ(hit);
		    float wx=mFcsDbMaker->getXWidth(det);
		    float wy=mFcsDbMaker->getYWidth(det);
		    TBox* cell=new TBox(xyz.x()-wx/2.0+xoff[det], xyz.y()+yoff[det]-wy/2.0,
					xyz.x()+wx/2.0+xoff[det], xyz.y()+yoff[det]+wy/2.0);
		    cell->SetFillStyle(0);
		    cell->SetLineColor(2+i);
		    cell->SetLineWidth(2);
		    cell->Draw("l");				    
		    TText* tx;
		    if(mRun19 || det>=kFcsHcalNorthDetId){
			tx=new TText(xyz.x()+xoff[det],xyz.y()+yoff[det]+wy/4.0,Form("%d",i));
		    }else{
			tx=new TText(xyz.x()+xoff[det],xyz.y()+yoff[det],Form("%d",i));
		    }
		    tx->SetTextSize(cTxSize); tx->SetTextAlign(22);
		    tx->Draw();
		}
		int nphoton=clu->nPoints();
		for(int j=0; j<nphoton; j++){
		    StFcsPoint* point = clu->points()[j];
		    StThreeVectorF xyz=point->XYZ();
		    TMarker* m=new TMarker(xyz.x()+xoff[det],xyz.y()+yoff[det],29);
		    m->SetMarkerColor(1);
		    m->SetMarkerSize(2);
		    m->SetMarkerColor(kRed);
		    m->Draw();
		}
	    }
	}

	//STGC
	if(mStgcColl){
	    for(int det=0; det<kStgcNDet; det++){
		StSPtrVecFtsStgcHit& hits = mStgcColl->hits(det);
		int ns=mStgcColl->numberOfHits(det);
		if(mDebug>0) LOG_INFO << Form("StFcsEventDisplay STGC det=%1d nhit=%4d",det,ns) << endm;
		for(int i=0; i<ns; i++){
		    StFtsStgcHit* hit=hits[i];
		    unsigned short fee  = hit->fee();
		    unsigned short altro= hit->altro();
		    unsigned short ch   = hit->ch();
		    unsigned int id     = mStgcDbMaker->toId(fee,altro,ch);
		    float x,y,dx,dy,z,dz;
		    mStgcDbMaker->globalPosition(id,x,y,dx,dy,z,dz);
		    TBox* cell=new TBox(x-dx/2+stgcOffX, y-dy/2+stgcOffY,
					x+dx/2+stgcOffX, y+dy/2+stgcOffY);
		    cell->SetFillStyle(0);
		    cell->SetLineColor(kRed);
		    cell->SetLineWidth(1);
		    cell->Draw();
		}
	    }
	    
	}
	
	mCanvas->Update();
	TString f(mFilename);
	if(f.Contains(".pdf")){ //not working
	    if(mNAccepted==1) f.Append("(");
	    else if(mNAccepted==mMaxEvents) f.Append(")");
	    LOG_INFO << "Saving " << f.Data() << endm;
	    mCanvas->Print(f.Data(),"pdf");	
	}else if(f.Contains(".png")){
	    f.ReplaceAll("eventDisplay.png",Form("%d.eventDisplay.png",mNEvents));
	    LOG_INFO << "Saving " << f.Data() << endm;
	    mCanvas->SaveAs(f.Data());
	}
    }
    return kStOK;
}
