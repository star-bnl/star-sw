/*!
 * \class  StFgtQAMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 * $Id: StFgtQAMaker.cxx,v 1.13 2014/03/05 19:10:16 akio Exp $
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
#include "StRoot/StFgtPool/StFgtClusterTools/StFgtGeneralBase.h"
#include "StRoot/StFgtPool/StFgtClusterTools/StFgtStraightTrackMaker.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#include <TMath.h>
#include <TGraphAsymmErrors.h>

static const int mDebug=0;      // debug mesasge level
static const int NTB=15;
static const char* cquad[kFgtNumQuads]={"A","B","C","D"};  

inline int globalapv(int rdo, int arm, int apv){
  return (rdo-1)*6*2 + arm*2 + apv/12;
  //  if(apv<10) { return (rdo-1)*6*2*10 + arm*2*10 + apv;}
  //else       { return (rdo-1)*6*2*10 + arm*2*10 + apv - 2;}
}

inline void getPRC(int rdo, int arm, int apv, int& page, int& row, int& col){
  page = ((rdo-1)*6 + arm)/2;
  row = (((rdo-1)*6 + arm)%2)*4 + (apv/12)*2 + (apv%12)/5;
  col = (apv%12)%5;
  //printf("rdo=%1d arm=%1d apv=%2d page=%d row=%d col=%d\n",rdo,arm,apv,page,row,col);
}

inline double localphi(int quad, double phi){
  static const double mPi = TMath::Pi();
  static const double phioff[kFgtNumQuads] = {-15.0*mPi/180.0, -105.0*mPi/180.0, 165.0*mPi/180.0, 165.0*mPi/180.0};
  double local=phi - phioff[quad];
  while(local<0.0) {local+=2.0*mPi;}
  while(local>2.0*mPi) {local-=2.0*mPi;}
  return local;
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
    if(i==6) { frame = new TH2F(c,c,1,0,NTB*4,1,-1000,5000*6); frame->GetXaxis()->SetNdivisions(NTB*100+4,kFALSE);}
    else     { frame = new TH2F(c,c,1,0,NTB*5,1,-1000,5000*8); frame->GetXaxis()->SetNdivisions(NTB*100+5,kFALSE);}    
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
  mEventCounter++;   //if(mEventCounter%100==0) printf("Event=%d\n",mEventCounter);
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
	float x = 2 + col*NTB;   
	float y = 4000 + (7-row)*5000;
	TText *t1 = new TText(x,y,c);
	t1->SetTextSize(0.02); t1->Draw();	
	
	if(apv==9|| apv==21){
	  Short_t disc, quad, strip;
	  Char_t layer;
	  int geoid=mDb->getGeoIdFromElecCoord(rdo,arm,apv-9,0);
	  StFgtGeom::decodeGeoId(geoid,disc,quad,layer,strip);
	  sprintf(c,"Rdo%1dArm%1d/%1d%2s",rdo,arm,disc+1,cq[quad]);
	  float x = (col+1.2)*NTB; 
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
  const int nbin= 512;
  const int max=30720;
  const int binwid=max/nbin;
  const float min=0.5-binwid;
  const float max2=max+0.5; 
  const char* cHist[NHist]={"MaxTimeBin","ADC","DataSize",       "ZSdata",      "10sigma", "Nevt", "LandauChi2","LandauSig","LandauMpv",  "Mpv-3Sig"};
  const int   nHist[NHist]={         NTB,  200,    nbin+1, kFgtNumElecIds, kFgtNumElecIds,      1,           40,         40,         60,          60}; 
  const float lHist[NHist]={           0,    0,       min,              0,              0,      0,            0,          0,          0,          -3};    
  const float hHist[NHist]={  float(NTB), 4200,      max2, kFgtNumElecIds, kFgtNumElecIds,      1,           20,          4, float(NTB),float(NTB-3)}; 
  const char* c1dHist[N1dHist]={"NHitStrip", "PhiHit",   "RHit", "NCluster","ClusterSize","ClusterCharge","MaxADC","ChargeAsy","CluChargeT","MaxADCT","ChargeAsyTrk","LandauN"};
  const int   n1dHist[N1dHist]={         50,       ns,       ns,         25,           15,             50,      50,         25,          50,       50,            25,       50};
  const float l1dHist[N1dHist]={          0,        0,        0,          0,            0,              0,       0,       -0.3,           0,        0,          -0.3,        0};
  const float h1dHist[N1dHist]={        100,float(ns),float(ns),       25.0,           15,          30000,    4000,        0.3,       30000,     4000,           0.3,    30000};
  const char* c2dHist[N2dHist] ={       "XY", "ADCvsTB",   "APVTB", "XYT"};
  const int   xn2dHist[N2dHist]={         50,       NTB,        60,    50}; 
  const float xl2dHist[N2dHist]={        -40,         0,         0,   -40};
  const float xh2dHist[N2dHist]={         40,float(NTB),float(NTB),    40};
  const int   yn2dHist[N2dHist]={         50,       200,        24,    50}; 
  const float yl2dHist[N2dHist]={        -40,         0,         0,   -40};
  const float yh2dHist[N2dHist]={         40,      4000,        24,    40};
  const char* cTHist[NTrkHist]={ "NTrk","NHitTrk","DCA","ZVTX","Chi2","HitDisc"};
  const int   nTHist[NTrkHist]={     10,        7,   60,    50,    50,        6};
  const float lTHist[NTrkHist]={      0,        0,    0,  -100,     0,      0.5};
  const float hTHist[NTrkHist]={     10,        7,    4,   100, 0.015,      6.5};
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
  //1d histos per quad
  for(int quad=0; quad<kFgtNumQuads; quad++){
    for(int i=0; i<NTrkHist; i++){
      sprintf(c,"Quad%1s-%s",cquad[quad],cTHist[i]);
      histTrk[quad][i]=new TH1F(c,c,nTHist[i],lTHist[i],hTHist[i]);
    }
  }  
}

void StFgtQAMaker::saveHist(){
  char fname[50]="fgtQA.root";
  if(mRunNumber>0) {
    sprintf(fname,"%d/fgtQA_%d.root",mRunNumber/1000,mRunNumber);
  }
  cout << "Writing " << fname << endl;
  TFile *hfile = new TFile(fname,"RECREATE");  
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
  //1d histos per quad
  for(int quad=0; quad<kFgtNumQuads; quad++){
    for(int i=0; i<NTrkHist; i++){
      histTrk[quad][i]->Write();
    }
  }
  hfile->Close();
}

void StFgtQAMaker::fillHist(){  
  StEvent* eventPtr = (StEvent*)GetInputDS("StEvent");
  fgtCollectionPtr=0;
  if( eventPtr ) {
    //LOG_Info << "Found StFgtCollection from StEvent in '" << ClassName() << "'" << endm;
    fgtCollectionPtr = eventPtr->fgtCollection();
  }else{
    TObjectSet *os = (TObjectSet*)GetDataSet("FGTCOLLECTION");
    if (os) {
      fgtCollectionPtr = (StFgtCollection*)os->GetObject();
      //LOG_Info << "Found StFgtCollection from FGTCOLLECTION in '" << ClassName() << "'" << endm;
    }
  }
  if( !fgtCollectionPtr) {
    LOG_ERROR << "Error getting pointer to StFgtCollection in '" << ClassName() << "'" << endm;
    return;
  };
  
  mNTimeBin=fgtCollectionPtr->getNumTimeBins();
  int datasize=0;
  if(mNTimeBin==0) {hist0[2]->Fill(float(datasize)); return; }
  for (int disc=0; disc<kFgtNumDiscs; disc++){    
    //Strips
    int nhit[kFgtNumQuads]; memset(nhit,0,sizeof(nhit));
    StFgtStripCollection *cstrip = fgtCollectionPtr->getStripCollection(disc);
    StSPtrVecFgtStrip &strip=cstrip->getStripVec();
    for(StSPtrVecFgtStripIterator it=strip.begin(); it!=strip.end(); it++) {
      datasize++;
      int geoid   =(*it)->getGeoId();
      int maxadc  =(*it)->getMaxAdc();
      float ped   =(*it)->getPed();
      float pederr=(*it)->getPedErr();
      char layer;
      short idisc,iquad,ipr,istr;
      StFgtGeom::decodeGeoId(geoid,idisc,iquad,layer,istr);
      if(layer=='P') {ipr=0;}
      if(layer=='R') {ipr=1;}
      Int_t rdo, arm, apv, chan;
      (*it)->getElecCoords(rdo, arm, apv, chan);
      int gapv = globalapv(rdo,arm,apv);
      int eid = StFgtGeom::encodeElectronicId(rdo, arm, apv, chan);
      hist0[3]->Fill(float(eid));
      if(maxadc>mSigmaCut*pederr && maxadc>mAdcCut){
	hist0[4]->Fill(float(eid));
	if(mNTrace2[rdo-1][arm][apv]<MAXTRACE) {
	  //printf("ntrace %d %d %d %d\n",rdo,arm,apv,mNTrace2[rdo-1][arm][apv]);
	  mNTrace2[rdo-1][arm][apv]++;
	  TGraph* g = new TGraph();
	  int page,row,col;
	  getPRC(rdo,arm,apv,page,row,col);
	  for(int tb=0; tb<mNTimeBin; tb++){
	    g->SetPoint(tb,float(tb+col*NTB),float((*it)->getAdc(tb)+(7-row)*5000));
	  }
	  mCanvas[page]->cd(); 
	  g->SetLineColor(kBlue); g->SetLineWidth(0.3); g->Draw("C"); 	
	}
	if(mNTrace[idisc][iquad]<MAXTRACE) {
	  mNTrace[idisc][iquad]++;
	  TGraph* g = new TGraph();
	  for(int tb=0; tb<mNTimeBin; tb++){
	    g->SetPoint(tb,float(tb+iquad*NTB),float((*it)->getAdc(tb)+25000-idisc*5000));
	  }
	  mCanvas[6]->cd(); 
	  g->SetLineColor(kBlue); g->SetLineWidth(0.5); g->Draw("C"); 
	}
      }
      hist0[1]->Fill(float(maxadc)+ped);	  
      if(maxadc>4*pederr && maxadc>160) {
	nhit[iquad]++;	
	if(ipr==1) {hist1[disc][iquad][1+ipr]->Fill(float(istr));}
	else       {hist1[disc][iquad][1+ipr]->Fill(float(istr/2 + (istr%2)*360));}
      }
      if(maxadc>mSigmaCut*pederr && maxadc>mAdcCut){
	float max=0;
        int maxtb=0;
	for(int tb=0; tb<mNTimeBin; tb++){
	  float a=float((*it)->getAdc(tb));
	  if(a>200) hist2[idisc][1]->Fill(float(tb),a);
	  if(a>max) {max=a; maxtb=tb;}
	  hist2[0][2]->Fill(float(tb), float(gapv), a);
	}
	hist2[1][2]->Fill(float(maxtb), float(gapv));
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
	float tb = float((*it)->getMaxTimeBin());
	//float tb = (*it)->getLandauPeak();
	hist0[0]->Fill(tb);
	int rdo,arm,apv,ch;
	mDb->getElecCoordFromGeoId(geoid,rdo,arm,apv,ch);	
	int gapv=globalapv(rdo,arm,apv);
	hist2[2][2]->Fill(tb, float(gapv));
	hist2[3][2]->Fill((*it)->getLandauMpv(), float(gapv));
	hist2[4][2]->Fill((*it)->getLandauMpv()-(*it)->getLandauSigma()*3, float(gapv));
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

  //Tracks
  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker("fgtStraightTracker"));
  if (fgtSTracker){
    vector<AVTrack>& tracks=fgtSTracker->getTracks();
    int ntrk[kFgtNumQuads]; memset(ntrk,0,sizeof(ntrk));
    for(vector<AVTrack>::iterator t=tracks.begin();t!=tracks.end();t++){
      if(mDebug>0) cout<<Form("Trk chi2=%8.3f dca=%8.3f",t->chi2,t->dca)<<endl;
      if(t->dca>3 || t->chi2>0.01) continue;
      vector<AVPoint>* points=t->points;
      int quad=-1,disc=-1,nhit=0;
      for(vector<AVPoint>::iterator p=points->begin(); p!=points->end();p++){
	nhit++;
	disc=p->dID;
	quad = p->quadID;
	histTrk[quad][5]->Fill(float(disc+1));
	hist1[disc][quad][8]->Fill(p->rCharge);
	hist1[disc][quad][8]->Fill(p->phiCharge);	
	hist1[disc][quad][9]->Fill(p->fgtHitR->getMaxAdc());
	hist1[disc][quad][9]->Fill(p->fgtHitPhi->getMaxAdc());
	hist1[disc][quad][10]->Fill((p->phiCharge-p->rCharge)/(p->phiCharge+p->rCharge));
	hist1[disc][quad][11]->Fill(p->fgtHitPhi->getLandauNorm());
	hist1[disc][quad][11]->Fill(p->fgtHitR->getLandauNorm());	
	hist2[disc][3]->Fill(p->x,p->y);	
	StFgtHit *phi=p->fgtHitPhi;
	StFgtHit *r=p->fgtHitR;
	hist0[6]->Fill(phi->getLandauChi2());  hist0[6]->Fill(r->getLandauChi2());
	hist0[7]->Fill(phi->getLandauSigma()); hist0[7]->Fill(r->getLandauSigma());
	hist0[8]->Fill(phi->getLandauMpv());   hist0[8]->Fill(r->getLandauMpv());
	hist0[9]->Fill(phi->getLandauMpv()- phi->getLandauSigma()*3);  
	hist0[9]->Fill(r->getLandauMpv()  - r->getLandauSigma()*3);
	pulseFit(p->fgtHitPhi); pulseFit(p->fgtHitR);
      }
      histTrk[quad][1]->Fill(nhit);
      histTrk[quad][2]->Fill(t->dca);
      histTrk[quad][3]->Fill(t->trkZ);
      histTrk[quad][4]->Fill(t->chi2);
      ntrk[quad]++;
    }    
    for(int quad=0; quad<kFgtNumQuads; quad++) {histTrk[quad][0]->Fill(float(ntrk[quad]));}
  }
  textDump();
}

void StFgtQAMaker::pulseFit(StFgtHit* cluster){
  static TCanvas *C1=0;
  static int NPLOT=0;
  static int NPAGE=0;
  static int MAXPAGE=10;
  if(NPAGE>MAXPAGE) return;

  if(C1==0) C1=new TCanvas("Pulse","Pulse",50,50,750,800);
  if(NPLOT==0){
    C1->Clear();
    C1->Divide(3,3);
  }  
  TVirtualPad *pad = C1->cd(NPLOT+1);
  
  int geoid=cluster->getCentralStripGeoId();
  char layer;
  short disc,quad,str,pr;
  StFgtGeom::decodeGeoId(geoid,disc,quad,layer,str);
  if(layer=='P') {pr=0;}
  if(layer=='R') {pr=1;}

  int nstrip=0, center=0;
  StFgtStrip* strips[20]; 
  StFgtStripCollection *cstrip = fgtCollectionPtr->getStripCollection(disc);
  StSPtrVecFgtStrip &strip=cstrip->getStripVec();
   for(StSPtrVecFgtStripIterator it=strip.begin(); it!=strip.end(); it++) {
    int igeoid   =(*it)->getGeoId();
    char ilayer;
    short idisc,iquad,ipr,istr;
    StFgtGeom::decodeGeoId(igeoid,idisc,iquad,ilayer,istr);
    if(ilayer=='P') {ipr=0;}
    if(ilayer=='R') {ipr=1;}
    if(geoid==igeoid) {center=nstrip;}
    if(quad==iquad && pr==ipr && abs(str-istr)<10){ strips[nstrip]=(*it); nstrip++; }
  }
  float a[NTB]; memset(a,0,sizeof(a));
  float e[NTB]; memset(e,0,sizeof(e));
  int   f[NTB]; memset(f,0,sizeof(f));
  int nstr=0, nskip=0;
  for(int i=center; i<nstrip; i++){
    short type = strips[i]->getClusterSeedType();
    if(type<kFgtSeedType1 || type>=kFgtClusterTooBig) nskip++;
    if(nskip==2) break;
    if(nskip==1) continue;
    nstr++;
    float ped = strips[i]->getPed();
    float rms = strips[i]->getPedErr();
    for(int t=0; t<NTB; t++){ 
      int adc = strips[i]->getAdc(t);
      a[t]+=adc; 
      e[t]+=rms*rms;
      if(adc+ped>3200){f[t]=1;}// printf("Saturation??? %f\n",adc+ped);}
    }
  }
  nskip=0;
  for(int i=center-1; i>=0; i--){
    short type = strips[i]->getClusterSeedType();
    if(type<kFgtSeedType1 || type>=kFgtClusterTooBig) nskip++;
    if(nskip==2) break;
    if(nskip==1) continue;
    nstr++;
    float ped = strips[i]->getPed();
    float rms = strips[i]->getPedErr();
    for(int t=0; t<NTB; t++){
      float adc = strips[i]->getAdc(t);      
      a[t]+=adc; 
      e[t]+=rms*rms;
      if(adc+ped>3400){f[t]=1;}
    }
  }
  TGraphAsymmErrors *g1 = new TGraphAsymmErrors(mNTimeBin);
  TGraphAsymmErrors *g2 = new TGraphAsymmErrors(7);
  TGraphAsymmErrors *g3 = new TGraphAsymmErrors(6);
  //printf("%1d nstrip=%d adc=",NPLOT,nstrip);
  for(int t=0; t<mNTimeBin; t++){
    //printf("%4.0f(%4.0f) ",a[t],e[t]);
    e[t]=sqrt(e[t]);
    g1->SetPoint(t,float(t),a[t]);
    if(f[t]==0) {g1->SetPointError(t,0,0,e[t],e[t]);}
    else        {g1->SetPointError(t,0,0,e[t],2000);}     
  }
  //printf("\n");
  for(int t=0; t<7; t++){
    g2->SetPoint(t,float(t+2),a[t+2]);
    if(f[t]==0) {g2->SetPointError(t,0,0,e[t+2],e[t+2]);}
    else        {g2->SetPointError(t,0,0,e[t+2],2000);}     
  }
  for(int t=0; t<6; t++){
    g3->SetPoint(t,float(t+2),a[t+2]);
    if(f[t]==0) {g3->SetPointError(t,0,0,e[t+2],e[t+2]);}
    else        {g3->SetPointError(t,0,0,e[t+2],2000);}     
  }
  g1->SetMarkerStyle(20); g1->SetMarkerSize(1);
  g1->Draw("AP");

  gStyle->SetStatW(0.4);
  int res1=g1->Fit("landau","Q0");
  TF1 *f1 = g1->GetFunction("landau"); f1->SetLineColor(2); f1->SetLineWidth(2); f1->Draw("same");

  int res2=g2->Fit("landau","Q0");
  TF1 *f2 = g2->GetFunction("landau"); f2->SetLineColor(4); f2->SetLineWidth(2); f2->Draw("same");

  int res3=g3->Fit("landau","Q0");
  TF1 *f3 = g3->GetFunction("landau"); f3->SetLineColor(7); f3->SetLineWidth(2); f3->Draw("same");

  NPLOT++;
  if(NPLOT==9){
    C1->Update();
    //char tmp[100];
    char file[100];
    if(NPAGE==0){
      sprintf(file,"%d/%d.pulsefit.pdf(",int(mRunNumber)/1000,mRunNumber);
    }else if(NPAGE==MAXPAGE){
      sprintf(file,"%d/%d.pulsefit.pdf)",int(mRunNumber)/1000,mRunNumber);
    }else{
      sprintf(file,"%d/%d.pulsefit.pdf",int(mRunNumber)/1000,mRunNumber);
    }
    C1->Print(file,"pdf");
    //printf("Press key!!!!  -->");  cin >> tmp;
    NPLOT=0;
    NPAGE++;
  }
}

void StFgtQAMaker::dip(){
}

void StFgtQAMaker::textDump(){
  static int MAXEVT=100;
  static int NEVT=0;
  static FILE *mTextFile=0;
  if(NEVT==0){
    char filename[100];
    sprintf(filename,"%d/%d.evtdump.txt",int(mRunNumber)/1000,mRunNumber);
    printf("Writing %s\n",filename);
    mTextFile = fopen(filename,"w+");
    if(!mTextFile) {printf("Couldn't open %s\n",filename); NEVT=MAXEVT+1;}
  }
  //if(NEVT==MAXEVT) {fclose(mTextFile);}
  if(NEVT>=MAXEVT) {return;}
  fprintf(mTextFile,"===== Event %d =====\n",NEVT);
  char layer[2]={'P','R'};
  StEvent* eventPtr = (StEvent*)GetInputDS("StEvent");
  if( !eventPtr ) { return; }
  StFgtCollection* fgtCollectionPtr = eventPtr->fgtCollection();
  if( !fgtCollectionPtr) { return; }
  StFgtStraightTrackMaker *fgtSTracker = static_cast<StFgtStraightTrackMaker * >( GetMaker("fgtStraightTracker"));
  if (! fgtSTracker){ return; }
  vector<AVTrack>& tracks=fgtSTracker->getTracks();
  int missedhit[kFgtNumGeoIds]; memset(missedhit,0,sizeof(missedhit));

  for(int quad=0; quad<kFgtNumQuads; quad++){
    //print tracks
    for(vector<AVTrack>::iterator t=tracks.begin();t!=tracks.end();t++){
      vector<AVPoint>* points=t->points;
      int nhit=0, quad2=-1; 
      int dischit[kFgtNumDiscs]; memset(dischit,0,sizeof(dischit));
      for(vector<AVPoint>::iterator p=points->begin(); p!=points->end();p++){
	quad2=p->quadID;
	if(quad2 != quad) break;	  
	int disc=p->dID;
	dischit[disc]=1;
	if(nhit==0) fprintf(mTextFile,"TRK quad=%1s chi2=%6.3f dca=%6.2f vtxz=%6.2f\n",
			   cquad[quad],t->chi2,t->dca,t->trkZ);
	fprintf(mTextFile,"TRK quad=%1s hit=%1d disc=%1d Pchg=%6.0f Rchg=%6.0f x=%6.2f y=%6.2f\n",
	       cquad[quad], nhit, disc+1, p->fgtHitPhi->getLandauNorm(),p->fgtHitR->getLandauNorm(),p->x,p->y);
	//printf("TRK quad=%1s hit=%1d disc=%1d Pchg=%6.0f Rchg=%6.0f x=%6.2f y=%6.2f\n",
	//      cquad[quad], nhit, disc+1, p->fgtHitPhi->getLandauNorm(),p->fgtHitR->getLandauNorm(),p->x,p->y);
	nhit++;
      }
      if(quad2 != quad) break;
      for(int disc=0; disc<kFgtNumDiscs; disc++){
	if(dischit[disc]==0){ //this track is in this quad, but no hit in this disc
	  double binFrac[1];
	  double z=StFgtGeom::getDiscZ(disc);
	  double x=t->mx*z+t->ax;
	  double y=t->my*z+t->ay;
	  double r=sqrt(x*x+y*y);
	  double p=atan2(y,x);
	  double localp=localphi(quad,p);	  
	  int ip = StFgtGeom::phi2LocalStripId(r,localp,binFrac);
	  int ir = StFgtGeom::rad2LocalStripId(r,localp,binFrac);
	  int gp= StFgtGeom::encodeGeoId(disc,quad,'P',ip); missedhit[gp]=1;
	  int gr= StFgtGeom::encodeGeoId(disc,quad,'R',ir); missedhit[gr]=1;
	  //printf("%1d%1s x=%6.2f y=%6.2f r=%6.2f ir=%3d phi=%6.2f local=%6.2f ip=%3d\n",
	  //	 disc+1,cquad[quad],x,y,r,ir,p,localp,ip);
	}
      }
    }
    //Print strips & clusters (with track if associated)
    for (int disc=0; disc<kFgtNumDiscs; disc++){
      StFgtStripCollection *cstrip = fgtCollectionPtr->getStripCollection(disc);
      StSPtrVecFgtStrip &strips=cstrip->getStripVec();
      StFgtHitCollection *chit = fgtCollectionPtr->getHitCollection(disc);
      StSPtrVecFgtHit    &hits=chit->getHitVec();
      for(int ipr=0; ipr<2; ipr++){
	for(int strip=0; strip<kFgtNumStrips; strip++){
	  int geoid = StFgtGeom::encodeGeoId(disc,quad,layer[ipr],strip);
	  if(geoid<0) continue;
	  StFgtStrip* str = 0;
	  StFgtHit* hit = 0;
	  AVTrack* trk = 0;
	  int flags=0, flagc=0, flagt=0;
	  for(StSPtrVecFgtStripIterator its=strips.begin(); its!=strips.end(); its++) {
	    if((*its)->getGeoId() != geoid) continue;
	    str = (*its);
	    if(str->getClusterSeedType()>0) flags=1;
	  }
	  for(StSPtrVecFgtHitIterator ith=hits.begin(); ith!=hits.end(); ith++) {
	    if((*ith)->getCentralStripGeoId() != geoid) continue;
	    hit = (*ith);
	    flagc=1;
	  }
	  for(vector<AVTrack>::iterator t=tracks.begin();t!=tracks.end();t++){
	    vector<AVPoint>* points=t->points;
	    for(vector<AVPoint>::iterator p=points->begin(); p!=points->end();p++){
	      if(p->quadID != quad) break;
	      if(p->dID != disc) continue;
	      StFgtHit* thit;
	      if(ipr==0) {thit=p->fgtHitPhi;}
	      else       {thit=p->fgtHitR;}
	      if(thit->getCentralStripGeoId() != geoid) continue; //hit is in same disc/quad but somewhere else
	      flagt=1;
	    }
	  }
	  if(missedhit[geoid]>0) flagt=2;
	  if(flags>0 || flagc>0 || flagt>0){
	    int rdo, arm, apv, ch;
	    mDb->getElecCoordFromGeoId(geoid,rdo,arm,apv,ch);
	    Int_t elecId = StFgtGeom::encodeElectronicId( rdo, arm, apv, ch );
	    int status=mDb->getStatusFromElecId(elecId);
	    char cl;
	    short id,iq;	    
	    double rphi,l,h;
	    StFgtGeom::getGlobalPhysicalCoordinate(geoid,id,iq,cl,rphi,l,h);
	    fprintf(mTextFile,"%05d=%1d%1s%1c%03d|%05d=R%1dA%1dAPV%02dC%03d|",
		    geoid,disc+1,cquad[quad],layer[ipr],strip,elecId,rdo,arm,apv,ch);
	    if(str){ 
	      fprintf(mTextFile,"s=%1d psig=%3.0f|adc=", status,str->getPedErr()); 
	      for(int i=0; i<mNTimeBin; i++){ fprintf(mTextFile,"%4d ", str->getAdc(i)); }
	      fprintf(mTextFile," C=%7.0f sd=%02d",str->getCharge(), str->getClusterSeedType());	    
	    }
	    else { 
	      fprintf(mTextFile,"s=%1d no strip info                ", status); 
	      for(int i=0; i<mNTimeBin; i++){ fprintf(mTextFile,"     ");}
	    }
	    fprintf(mTextFile," %1c=%8.4f ",layer[ipr],rphi);
	    if(flagc==1){
	      if(ipr==0){
		fprintf(mTextFile,"C=%7.0f P=%8.3f N=%2d Sd=%2d",
		       hit->charge(),hit->getPositionPhi(),hit->getNstrip(),hit->getSeedType());
	      }else if(ipr==1){
		fprintf(mTextFile,"C=%7.0f R=%8.3f N=%2d Sd=%2d",
		       hit->charge(),hit->getPositionR(),hit->getNstrip(),hit->getSeedType());
	      }	      
	    }	
	    if(flagt==1) {fprintf(mTextFile," Track!");}
	    if(flagt==2) {fprintf(mTextFile," Miss (track should have left hit here)");}
	    fprintf(mTextFile,"\n");
	  }
	}//strip
      }//phir
    }//disc
  }//quad
  NEVT++;
}
