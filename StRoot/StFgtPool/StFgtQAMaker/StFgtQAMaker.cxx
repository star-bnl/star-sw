/*!
 * \class  StFgtQAMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 * $Id: StFgtQAMaker.cxx,v 1.3 2013/02/16 14:25:55 akio Exp $
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

ClassImp(StFgtQAMaker);

StFgtQAMaker::StFgtQAMaker(const Char_t *name) : StMaker(name),mEventCounter(0),mRunNumber(0) {
}

Int_t StFgtQAMaker::Init(){
  bookHist();
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
  return kStOK;
}

Int_t StFgtQAMaker::Finish() {
  gMessMgr->Info() << "StFgtQAMaker::Finish()" << endm;
  saveHist();
  return kStOK;
}

void StFgtQAMaker::bookHist(){
  char c[50];
  int ns=kFgtNumStrips;
  const char* cquad[kFgtNumQuads]={"A","B","C","D"};  
  const char* cHist[NHist]={"MaxTimeBin","ADC","DataSize"};
  const int   nHist[NHist]={          15,  200,       256}; 
  const float lHist[NHist]={           0,    0,         0};
  const float hHist[NHist]={          15, 4000,     30975};
  const char* c1dHist[N1dHist]={"NHitStrip", "PhiHit",   "RHit", "NCluster","ClusterSize","ClusterCharge","ChargeAsy"};
  const int   n1dHist[N1dHist]={         50,       ns,       ns,         25,           15,             50,        50};
  const float l1dHist[N1dHist]={          0,        0,        0,          0,            0,              0,       -0.5};
  const float h1dHist[N1dHist]={         50,float(ns),float(ns),       25.0,           15,          30000,        0.5};
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
    sprintf(fname,"fgtQA_%d.root",mRunNumber);
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
      if(disc==2 && (iquad==0 || iquad==3)) continue;
      if(idisc==4 && iquad==3) continue;
      hist0[1]->Fill(float(maxadc));	  
      if(maxadc>4*pederr && maxadc>160) {
	nhit[iquad]++;	
	hist1[disc][iquad][1+ipr]->Fill(float(istr));
      }
      if(maxadc>10*pederr && maxadc>400) {
	for(int tb=0; tb<ntimebin; tb++){
	  hist2[idisc][1]->Fill(float(tb),float((*it)->getAdc(tb)));
	}
      }	
    }
    for (int quad=0; quad<kFgtNumQuads; quad++) hist1[disc][quad][0]->Fill(float(nhit[quad]));
  }
  hist0[2]->Fill(float(datasize));

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
      if(idisc==2 && (iquad==0 || iquad==3)) continue;
      if(idisc==4 && iquad==3) continue;
      ncluster[iquad]++;      
      hist1[disc][iquad][4]->Fill(float((*it)->getNstrip()));
      hist1[disc][iquad][5]->Fill((*it)->charge());
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
    if(idisc==2 && (iquad==0 || iquad==3)) continue;
    if(idisc==4 && iquad==3) continue;
    mDb->getStarXYZ(idisc,iquad,r,phi,xyz);      
    hist2[idisc][0]->Fill(xyz.X(),xyz.Y());
    hist1[idisc][iquad][6]->Fill((*it)->getChargeAsymmetry());		       
  }    
}
