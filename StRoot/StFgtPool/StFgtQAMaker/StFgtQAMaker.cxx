/*!
 * \class  StFgtQAMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 * $Id: StFgtQAMaker.cxx,v 1.6 2013/03/03 07:40:59 akio Exp $
 *
 */

#include "StFgtQAMaker.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StDcaGeometry.h"
#include "StThreeVectorF.hh"
#include "StFgtQAMaker.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtDbMaker/StFgtDbMaker.h"
#include "StRoot/StFgtDbMaker/StFgtDb.h"

#include "StarClassLibrary/StThreeVectorF.hh"

static const int mDebug=0;      // debug mesasge level

inline void getPRC(int rdo, int arm, int apv, int& page, int& row, int& col){
  page = ((rdo-1)*6 + arm)/2;
  row = (((rdo-1)*6 + arm)%2)*4 + (apv/12)*2 + (apv%12)/5;
  col = (apv%12)%5;
  //printf("rdo=%1d arm=%1d apv=%2d page=%d row=%d col=%d\n",rdo,arm,apv,page,row,col);
}

ClassImp(StFgtQAMaker);

StFgtQAMaker::StFgtQAMaker(const Char_t *name) : StMaker(name),mEventCounter(0),mRunNumber(0),mSigmaCut(10),mAdcCut(500){
}

Int_t StFgtQAMaker::Init(){
  bookHist();
  for(int i=0; i<7; i++){
    char c[40];
    sprintf(c,"QA%d",i);
    mCanvas[i] = new TCanvas(c,c,50,0,700,800);
    TH2F* frame;
    sprintf(c,"ScopeTrace%d",i);
    if(i==6) { frame = new TH2F(c,c,1,0,15*4,1,-1000,5000*6); frame->GetXaxis()->SetNdivisions(1504,kFALSE);}
    else     { frame = new TH2F(c,c,1,0,15*5,1,-1000,5000*8); frame->GetXaxis()->SetNdivisions(1505,kFALSE);}    
    frame->SetStats(0); frame->Draw();  
  }
  memset(mNTrace,0,sizeof(mNTrace));
  memset(mNTrace2,0,sizeof(mNTrace2));
  return kStOK; 
}

Int_t StFgtQAMaker::InitRun(Int_t runnum){
  LOG_INFO << "StFgtQAMaker::InitRun for "  << runnum << endm;
  StFgtDbMaker *fgtDbMkr = static_cast<StFgtDbMaker * >( GetMaker("fgtDb"));
  if( !fgtDbMkr ){
    LOG_FATAL << "StFgtDb not provided and error finding StFgtDbMaker" << endm;
    return kStFatal;      
  } else {
    mDb = fgtDbMkr->getDbTables();
    if( !mDb ){
      LOG_FATAL << "StFgtDb not provided and error retrieving pointer from StFgtDbMaker '"
		<< fgtDbMkr->GetName() << endm;
      return kStFatal;
    }
  }
  return kStOK;
}

Int_t StFgtQAMaker::Make() {
  fillHist();
  mEventCounter++;
  return kStOK;
}

Int_t StFgtQAMaker::Finish() {
  gMessMgr->Info() << "StFgtQAMaker::Finish()" << endm;
  saveHist();
  saveTrace();
  return kStOK;
}

void StFgtQAMaker::saveTrace(){
  char cq[4][3]={"DA","AB","BC","CD"};
  char c[20];

  char cthr[100];
  sprintf(cthr,"MaxADC>%2.0f*sigma and %4.0f",mSigmaCut,mAdcCut);
  TText *tthr = new TText(0.6,0.95,cthr);
  tthr->SetNDC(); tthr->SetTextSize(0.03);

  for(int rdo=1; rdo<=kFgtNumRdos; rdo++){
    for(int arm=0; arm<kFgtNumArms; arm++){
      for(int apv=0; apv<kFgtMaxApvId; apv++){
	if(apv==10 || apv==11 || apv==22 || apv==23) continue;
	int page,row,col;
	getPRC(rdo,arm,apv,page,row,col);
	mCanvas[page]->cd();

	sprintf(c,"Apv%1d",apv);
	float x = 2 + col*15;   
	float y = 4000 + (7-row)*5000;
	TText *t1 = new TText(x,y,c);
	t1->SetTextSize(0.02); t1->Draw();	
	
	if(apv==9|| apv==21){
	  Short_t disc, quad, strip;
	  Char_t layer;
	  int geoid=mDb->getGeoIdFromElecCoord(rdo,arm,apv-9,0);
	  StFgtGeom::decodeGeoId(geoid,disc,quad,layer,strip);
	  sprintf(c,"Rdo%1dArm%1d/%1d%2s",rdo,arm,disc+1,cq[quad]);
	  float x = 18 + col*15;   
	  float y = 1000 + (7-row)*5000;
	  TText *t1 = new TText(x,y,c);
	  t1->SetTextSize(0.022); t1->SetTextAngle(90); t1->Draw();	
	}
      }
    }
  }
  for(int i=0; i<6; i++){
    mCanvas[i]->cd();
    tthr->Draw();
    char ff[50],fname[50]="fgtScopeTrace.pdf";
    if(mRunNumber>0) {
      sprintf(fname,"%d/fgtScopeTrace_%d.pdf",mRunNumber/1000,mRunNumber);
    }
    if(i==0) {
      cout << "Writing " << fname << endl;
      sprintf(ff,"%s(",fname);
    }else if(i==5){ 
      sprintf(ff,"%s)",fname);
    }else{
      sprintf(ff,"%s",fname);
    }       
    mCanvas[i]->Update();
    mCanvas[i]->Print(ff,"pdf");
  }
  mCanvas[6]->cd();
  tthr->Draw();
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    sprintf(c,"Disc%1d",disc+1);
    float y=0.82-disc*0.13;
    TText *t1 = new TText(0.91,y,c);
    t1->SetNDC(); t1->SetTextSize(0.03); t1->Draw();      
  }
  for(int quad=0; quad<kFgtNumQuads; quad++){
    sprintf(c,"Quad%1c",'A'+quad);
    float x=0.15 + quad*0.20;    
    TText *t1 = new TText(x,0.91,c);
    t1->SetNDC(); t1->SetTextSize(0.03); t1->Draw();      
  }
  char fname[50]="fgtScopeTrace.png";
  if(mRunNumber>0) {
    sprintf(fname,"%d/fgtScopeTrace_%d.png",mRunNumber/1000,mRunNumber);
  }
  cout << "Writing " << fname << endl;
  mCanvas[6]->Update();
  mCanvas[6]->SaveAs(fname);
}

void StFgtQAMaker::bookHist(){
  char c[50];
  int ns=kFgtNumStrips;
  const char* cquad[kFgtNumQuads]={"A","B","C","D"};  
  const char* cHist[NHist]={"MaxTimeBin","ADC","DataSize",       "ZSdata",      "10sigma", "Nevt"};
  const int   nHist[NHist]={          15,  200,       256, kFgtNumElecIds, kFgtNumElecIds,      1}; 
  const float lHist[NHist]={           0,    0,         0,              0,              0,      0};
  const float hHist[NHist]={          15, 4000,     30975, kFgtNumElecIds, kFgtNumElecIds,      1};
  const char* c1dHist[N1dHist]={"NHitStrip", "PhiHit",   "RHit", "NCluster","ClusterSize","ClusterCharge","MaxADC","ChargeAsy"};
  const int   n1dHist[N1dHist]={         50,       ns,       ns,         25,           15,             50,      50,         50};
  const float l1dHist[N1dHist]={          0,        0,        0,          0,            0,              0,       0,       -0.3};
  const float h1dHist[N1dHist]={        100,float(ns),float(ns),       25.0,           15,          10000,    4000,        0.3};
  const char* c2dHist[N2dHist] ={       "XY", "ADCvsTB"};
  const int   xn2dHist[N2dHist]={         50,        15}; 
  const float xl2dHist[N2dHist]={        -40,         0};
  const float xh2dHist[N2dHist]={         40,        15};
  const int   yn2dHist[N2dHist]={         50,       200}; 
  const float yl2dHist[N2dHist]={        -40,         0};
  const float yh2dHist[N2dHist]={         40,      4000};
  //histos for whole FGT
  for(int i=0; i<NHist; i++){
    hist0[i]=new TH1F(cHist[i],cHist[i],nHist[i],lHist[i],hHist[i]);
  }
  //1d histos per disc/quad  
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    for(int quad=0; quad<kFgtNumQuads; quad++){
      for(int i=0; i<N1dHist; i++){
	sprintf(c,"%1d%1s-%s",disc+1,cquad[quad],c1dHist[i]);
	hist1[disc][quad][i]=new TH1F(c,c,n1dHist[i],l1dHist[i],h1dHist[i]);
      }
    }
  }
  //2d histos per disc
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    for(int i=0; i<N2dHist; i++){
      sprintf(c,"Disc%1d%s",disc+1,c2dHist[i]);
      hist2[disc][i]=new TH2F(c,c,xn2dHist[i],xl2dHist[i],xh2dHist[i],yn2dHist[i],yl2dHist[i],yh2dHist[i]);
    }
  }
}

void StFgtQAMaker::saveHist(){
  char fname[50]="fgtQA.root";
  if(mRunNumber>0) {
    sprintf(fname,"%d/fgtQA_%d.root",mRunNumber/1000,mRunNumber);
  }
  cout << "Writing " << fname << endl;
  TFile *hfile = new TFile(fname,"update");  
  for(int i=0; i<NHist; i++){
    hist0[i]->Write();
  }
  //1d histos per disc/quad
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    for(int quad=0; quad<kFgtNumQuads; quad++){
      for(int i=0; i<N1dHist; i++){
        hist1[disc][quad][i]->Write();
      }
    }
  }
  //2d histos per disc
  for(int disc=0; disc<kFgtNumDiscs; disc++){
    for(int i=0; i<N2dHist; i++){
      hist2[disc][i]->Write();
    }
  }
  hfile->Close();
}

void StFgtQAMaker::fillHist(){
  StEvent* eventPtr = (StEvent*)GetInputDS("StEvent");
  if( !eventPtr ) {
    LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
    return;
  };
  StFgtCollection* fgtCollectionPtr = eventPtr->fgtCollection();
  if( !fgtCollectionPtr) {
    LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
    return;
  };
  
  int ntimebin=fgtCollectionPtr->getNumTimeBins();
  int datasize=0;
  for (int disc=0; disc<kFgtNumDiscs; disc++){    
    //Strips
    int nhit[kFgtNumQuads]; memset(nhit,0,sizeof(nhit));
    StFgtStripCollection *cstrip = fgtCollectionPtr->getStripCollection(disc);
    StSPtrVecFgtStrip &strip=cstrip->getStripVec();
    for(StSPtrVecFgtStripIterator it=strip.begin(); it!=strip.end(); it++) {
      datasize++;
      int geoid   =(*it)->getGeoId();
      int maxadc  =(*it)->getMaxAdc();
      float pederr=(*it)->getPedErr();
      char layer;
      short idisc,iquad,ipr,istr;
      StFgtGeom::decodeGeoId(geoid,idisc,iquad,layer,istr);
      if(layer=='P') {ipr=0;}
      if(layer=='R') {ipr=1;}
      Int_t rdo, arm, apv, chan;
      (*it)->getElecCoords(rdo, arm, apv, chan);
      int eid = StFgtGeom::encodeElectronicId(rdo, arm, apv, chan);
      hist0[3]->Fill(float(eid));
      if(maxadc>mSigmaCut*pederr && maxadc>mAdcCut){
	hist0[4]->Fill(float(eid));
	if(mNTrace2[rdo-1][arm][apv]<MAXTRACE) {
	  printf("ntrace %d %d %d %d\n",rdo,arm,apv,mNTrace2[rdo-1][arm][apv]);
	  mNTrace2[rdo-1][arm][apv]++;
	  TGraph* g = new TGraph();
	  int page,row,col;
	  getPRC(rdo,arm,apv,page,row,col);
	  for(int tb=0; tb<ntimebin; tb++){
	    g->SetPoint(tb,float(tb+col*15),float((*it)->getAdc(tb)+(7-row)*5000));
	  }
	  mCanvas[page]->cd(); 
	  g->SetLineColor(kBlue); g->SetLineWidth(0.3); g->Draw("C"); 	
	}
	if(mNTrace[idisc][iquad]<MAXTRACE) {
	  mNTrace[idisc][iquad]++;
	  TGraph* g = new TGraph();
	  for(int tb=0; tb<ntimebin; tb++){
	    g->SetPoint(tb,float(tb+iquad*15),float((*it)->getAdc(tb)+25000-idisc*5000));
	  }
	  mCanvas[6]->cd(); 
	  g->SetLineColor(kBlue); g->SetLineWidth(0.5); g->Draw("C"); 
	}
      }
      hist0[1]->Fill(float(maxadc));	  
      if(maxadc>4*pederr && maxadc>160) {
	nhit[iquad]++;	
	hist1[disc][iquad][1+ipr]->Fill(float(istr));
      }
      if(maxadc>mSigmaCut*pederr && maxadc>mAdcCut){
	for(int tb=0; tb<ntimebin; tb++){
	  float a=float((*it)->getAdc(tb));
	  if(a>200) hist2[idisc][1]->Fill(float(tb),a);
	}
      }	
    }
    for (int quad=0; quad<kFgtNumQuads; quad++) hist1[disc][quad][0]->Fill(float(nhit[quad]));
  }
  hist0[2]->Fill(float(datasize));
  if(datasize>0) hist0[5]->Fill(0);
  if(mEventCounter==100 || mEventCounter==500 || mEventCounter==1000){
    printf("Event=%d Updating canvas...\n",mEventCounter); mCanvas[6]->Update(); printf("...Done\n");
  }

  //Clusters
  for (int disc=0; disc<kFgtNumDiscs; disc++){    
    int ncluster[kFgtNumQuads]; memset(ncluster,0,sizeof(ncluster));
    StFgtHitCollection *chit = fgtCollectionPtr->getHitCollection(disc);
    StSPtrVecFgtHit    &hit=chit->getHitVec();
    for(StSPtrVecFgtHitIterator it=hit.begin(); it!=hit.end(); it++) {
      int geoid=(*it)->getCentralStripGeoId();
      char layer;
      short idisc,iquad,istr;
      StFgtGeom::decodeGeoId(geoid,idisc,iquad,layer,istr);
      ncluster[iquad]++;      
      hist1[disc][iquad][4]->Fill(float((*it)->getNstrip()));
      hist1[disc][iquad][5]->Fill((*it)->charge());
      hist1[disc][iquad][6]->Fill((*it)->getMaxAdc());
      if((*it)->getMaxAdc() > 500){
	hist0[0]->Fill(float((*it)->getMaxTimeBin()));
      }
    }
    for (int quad=0; quad<kFgtNumQuads; quad++) hist1[disc][quad][3]->Fill(float(ncluster[quad]));
  }
  //Points
  StFgtPointCollection *cpoint = fgtCollectionPtr->getPointCollection();
  StSPtrVecFgtPoint &point = cpoint->getPointVec();
  for(StSPtrVecFgtPointIterator it=point.begin(); it!=point.end(); it++) {
    int idisc=(*it)->getDisc();
    int iquad=(*it)->getQuad();
    float phi = (*it)->getPositionPhi();
    float r   = (*it)->getPositionR();
    TVector3 xyz;
    mDb->getStarXYZ(idisc,iquad,r,phi,xyz);      
    hist2[idisc][0]->Fill(xyz.X(),xyz.Y());
    hist1[idisc][iquad][7]->Fill((*it)->getChargeAsymmetry());		       
  }    
}
