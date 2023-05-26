// $Id: StFcsClusterMaker.cxx,v 1.1 2021/03/30 13:40:02 akio Exp $
//
// $Log: StFcsClusterMaker.cxx,v $
// Revision 1.1  2021/03/30 13:40:02  akio
// FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
//
// Revision 1.18  2021/02/25 21:52:57  akio
// Int_t -> int
//
// Revision 1.17  2021/02/25 19:24:15  akio
// Modified for STAR code review (Hongwei)
//
// Revision 1.16  2020/12/17 21:18:01  akio
// Separate RATIO2SPLIT for ecal/hcal
//
// Revision 1.15  2020/09/03 19:42:24  akio
// moving sum & fit to StFcsWaveformFitMaker
//
// Revision 1.14  2019/11/22 17:26:17  akio
// fix typo
//
// Revision 1.13  2019/11/22 15:51:16  akio
// adding categorization
//
// Revision 1.12  2019/08/16 16:26:28  akio
// adding gaincorr to sum
//
// Revision 1.11  2019/08/01 18:37:01  akio
// calling makesum/makefit for all detectors
//
// Revision 1.10  2019/07/15 16:58:32  akio
// Adding hit->cluster pointer
//
// Revision 1.9  2019/07/10 06:14:17  akio
// adding cluster pointer to hit
//
// Revision 1.8  2019/07/03 16:13:29  akio
// separate neighbor distance for ecal and hcal
//
// Revision 1.7  2019/06/27 16:12:42  akio
// Using getLocalXYinCell for cell/cluster distance measure and in moment analysis
//
// Revision 1.6  2019/06/26 18:00:34  akio
// added some adjustable parameters and way to select how to get energy
//
// Revision 1.5  2019/06/25 16:46:56  akio
// removed debugging comment
//
// Revision 1.4  2019/06/25 16:38:17  akio
// Added TOWER_E_RATIO2SPLIT, neighbor clusters
//
// Revision 1.3  2019/06/21 16:32:42  akio
// Added neighbor cluster and factor threshold for cluster splitting at valley
//
// Revision 1.2  2019/06/07 18:17:24  akio
// added summing adc
//
// Revision 1.1  2018/11/14 16:50:11  akio
// FCS codes in offline/upgrade/akio
//

#include "StFcsClusterMaker.h"

#include "StLorentzVectorF.hh"

#include "StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StFcsCluster.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "tables/St_g2t_track_Table.h"

#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

#include <cmath>
#include "TMath.h"
#include "TVector2.h"

ClassImp(StFcsClusterMaker)

StFcsClusterMaker::StFcsClusterMaker(const char* name) : StMaker(name) {}

StFcsClusterMaker::~StFcsClusterMaker() { }

void StFcsClusterMaker::Clear(Option_t* option) {
    StMaker::Clear(option);
}

int StFcsClusterMaker::InitRun(int runNumber) {
    // Ensure we can access database information
    LOG_DEBUG << "StFcsClusterMaker initializing run" << endm;    
    //mDb = static_cast<StFcsDbMaker*>(GetMaker("fcsDb"));    
    mDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
    if (!mDb) {
	LOG_ERROR << "StFcsClusterMaker initializing failed due to no StFcsDb" << endm;
	return kStErr;
    }
    return StMaker::InitRun(runNumber);
}

int StFcsClusterMaker::Make() {
    LOG_DEBUG << "StFcsClusterMaker Make!!!" << endm;
    
    StEvent* event = static_cast<StEvent*>(GetInputDS("StEvent"));
    mFcsCollection=0;
    if (event) mFcsCollection = event->fcsCollection();
    if(!mFcsCollection) {
	LOG_WARN << "StFcsClusterMaker did not find fcsCollection in StEvent" << endm;
	return kStWarn;	
    }
    
    for(int det=0; det<=kFcsHcalSouthDetId; det++) {
      if(det==0){
	mNeighborDistance = mNeighborDistance_Ecal;
	mDistanceAdvantage = mDistanceAdvantage_Ecal;
	mTowerEThreSeed = mTowerEThreSeed_Ecal;
	mTowerEThreshold = mTowerEThreshold_Ecal;
	mTowerEThreMoment = mTowerEThreMoment_Ecal;
	mTowerERatio2Split = mTowerERatio2Split_Ecal;    
      }
      if(det==2){
	mNeighborDistance = mNeighborDistance_Hcal;
	mDistanceAdvantage = mDistanceAdvantage_Hcal;
	mTowerEThreSeed = mTowerEThreSeed_Hcal;
	mTowerEThreshold = mTowerEThreshold_Hcal;
	mTowerEThreMoment = mTowerEThreMoment_Hcal;
	mTowerERatio2Split = mTowerERatio2Split_Hcal;    
      }
      makeCluster(det);	
    }
    if(GetDebug()>0) mFcsCollection->print(3);
    return kStOk;
}

int StFcsClusterMaker::makeCluster(int det) {
  StSPtrVecFcsHit&      hits = mFcsCollection->hits(det);
  StSPtrVecFcsCluster&  clusters = mFcsCollection->clusters(det);
  clusters.clear();
  int nhit=hits.size();
  for(int i=0; i<nhit; i++) hits[i]->setCluster(0); //reset cluster pointer from hit

  if(mSortById==0){  //sort by energy
    std::sort(hits.begin(), hits.end(), [](StFcsHit* a, StFcsHit* b) {
	return b->energy() < a->energy();
      });
  }else{ //sort by Id
    std::sort(hits.begin(), hits.end(), [](StFcsHit* a, StFcsHit* b) {
	return b->id() > a->id();
      });
  }    

  for(int i=0; i<nhit; i++){ //loop over all hits 
    StFcsHit* hit=hits[i];
    float e=hit->energy();
    if(e < mTowerEThreshold && mSortById==0) break;
    float neighborClusterId=-1;
    float minDistance=999.0;
    int ncluster = clusters.size();
    int nNeighbor= 0;
    StPtrVecFcsCluster neighbor;
    for(int j=0; j<ncluster; j++){ //check all existing cluster
      StFcsCluster* clu=clusters[j];
      float neighborTowerE = isNeighbor(hit,clu);
      if(neighborTowerE>0.0) { //found neighbor cluster
	neighbor.push_back(clu);
	nNeighbor++;	      
	if(neighborTowerE * mTowerERatio2Split > e){ //merge to existing cluster
	  float d = distance(hit,clu);
	  if(d * mDistanceAdvantage < minDistance){
	    neighborClusterId=j;
	    minDistance=d;
	  }
	}
      }
    }
    StFcsCluster* cluster=0;
    if(neighborClusterId==-1) {
      //no neighbor, thus found new cluster seed
      if(e >= mTowerEThreSeed){ 
	cluster = new StFcsCluster();
	cluster->setId(ncluster);
	cluster->setDetectorId(det);
	cluster->hits().push_back(hit);
	hit->setCluster(cluster);
	updateCluster(cluster);
	mFcsCollection->addCluster(det,cluster); 
	neighbor.push_back(cluster);
	nNeighbor++;
      }else{
	//no neighbor and not exceeding seed threshold 
	//what should we do??? I guess nothing...
      }
    }else{ 
      //found neighbor tower which has higher energy
      //add to the cluster with closest cluster
      cluster = clusters[neighborClusterId];
      cluster->hits().push_back(hit);	   
      hit->setCluster(cluster);
      updateCluster(cluster);
    }
    if(nNeighbor>1) { //more than 1 neighbors found
      for(int k=0; k<nNeighbor-1; k++){
	StFcsCluster* clu1 = neighbor[k];
	for(int l=k+1; l<nNeighbor; l++){
	  StFcsCluster* clu2 = neighbor[l];
	  clu1->addNeighbor(clu2);
	  clu2->addNeighbor(clu1);
	}
      }
    }
  }
  
  //loop over all found cluster and fill fourMomentum and do moment analysis
  int nc = clusters.size();
  for(int j=0; j<nc; j++){
    StFcsCluster* clu=clusters[j];
    StThreeVectorD xyz = mDb->getStarXYZfromColumnRow(det,clu->x(),clu->y());
    clu->setFourMomentum(mDb->getLorentzVector(xyz,clu->energy(),0.0));
    const StLorentzVectorD& p = clu->fourMomentum();
    //LOG_DEBUG << Form("momentum= %lf %lf %lf %lf", p.px(), p.py(), p.pz(), p.e()) << endm;
    int ret=clusterMomentAnalysis(clu,mTowerEThreMoment); //moment analysis with default threshold
    if(ret==kStErr) ret=clusterMomentAnalysis(clu,0.0);   //Redo with 0 threshold
    categorization(clu);
  }

  //debug MC info
  if(GetDebug()>=5){
    g2t_track_st* g2ttrk=0;
    St_g2t_track* trackTable = static_cast<St_g2t_track*>(GetDataSet("g2t_track"));
    if(!trackTable) {
      LOG_INFO << "g2t_track Table not found" << endm;
    }else{
      const int nTrk = trackTable->GetNRows();
      LOG_INFO << Form("g2t_track table has %d tracks",nTrk) << endm;
      if(nTrk>0){
        g2ttrk = trackTable->GetTable();
        if(!g2ttrk){
          LOG_INFO << "g2t_track GetTable failed" << endm;
        }
      }
    }
    if(g2ttrk){
      int ntrk=0;
      float frc=0;
      int nh = hits.size();
      for(int i=0; i<nh; i++){
        StFcsHit* hit=hits[i];
        const g2t_track_st* trk = mDb->getParentG2tTrack(hit,g2ttrk,frc,ntrk);
        //const g2t_track_st* trk=0; 
        //std::tie(trk,frc,ntrk) = mDb->getParentG2tTrack(hit,g2ttrk);
        LOG_INFO << Form("Det=%1d Id=%3d E=%8.3f Parent  Id=%4d Pid=%4d E=%8.3f Frc=%6.3f N=%d",
                         det,hit->id(),hit->energy(),trk->id,trk->ge_pid,trk->e,frc,ntrk)<<endm;
        const g2t_track_st* ptrk = mDb->getPrimaryG2tTrack(hit,g2ttrk,frc,ntrk);
        LOG_INFO << Form("Det=%1d Id=%3d E=%8.3f Primary Id=%4d Pid=%4d E=%8.3f Frc=%6.3f N=%d",
                         det,hit->id(),hit->energy(),ptrk->id,ptrk->ge_pid,ptrk->e,frc,ntrk)<<endm;
      }
      int nc = clusters.size();
      for(int j=0; j<nc; j++){
        StFcsCluster* clu=clusters[j];
        const g2t_track_st* trk = mDb->getParentG2tTrack(clu,g2ttrk,frc,ntrk);
        LOG_INFO << Form("Det=%1d C#=%3d E=%8.3f Parent  Id=%4d Pid=%4d E=%8.3f Frc=%6.3f N=%d",
                         det,j,clu->energy(),trk->id,trk->ge_pid,trk->e,frc,ntrk)<<endm;
        const g2t_track_st* ptrk = mDb->getPrimaryG2tTrack(clu,g2ttrk,frc,ntrk);
        LOG_INFO << Form("Det=%1d C#=%3d E=%8.3f Primary Id=%4d Pid=%4d E=%8.3f Frc=%6.3f N=%d",
                         det,j,clu->energy(),ptrk->id,ptrk->ge_pid,ptrk->e,frc,ntrk)<<endm;
      }
    }
  }

  return kStOk;
}

//check if the hit is next to hits in the cluster
//returning energy of highest tower in cluster which is neighbor to the hit
float StFcsClusterMaker::isNeighbor(StFcsHit* hit, StFcsCluster* clu){ 
    int nhit = clu->hits().size();
    float ne=0.0;
    for(int i=0; i<nhit; i++){
	StFcsHit* h=clu->hits()[i];
	int ehp = mDb->ecalHcalPres(h->detectorId());
	float thr=1.01;
	if(ehp==0) thr=mNeighborDistance_Ecal;
	if(ehp==1) thr=mNeighborDistance_Hcal;
	float d = distance(hit,h);
	float e = h->energy();
	if(d < thr) ne=e;
    }
    return ne;
}

// distance between 2 hits in cell unit
float StFcsClusterMaker::distance(StFcsHit* hit1, StFcsHit* hit2){
    int   det1=hit1->detectorId();
    int   det2=hit2->detectorId();
    if(det1 != det2) return 999.0;
    float x1,x2,y1,y2;
    mDb->getLocalXYinCell(hit1,x1,y1);
    mDb->getLocalXYinCell(hit2,x2,y2);
    float dx=x1-x2;
    float dy=y1-y2;
    return sqrt(dx*dx + dy*dy);
}

// distance between a hit and cluster center
float StFcsClusterMaker::distance(StFcsHit* hit, StFcsCluster* clu){
    int   det1=hit->detectorId();
    int   det2=clu->detectorId();
    if(det1 != det2) return 999.0;
    float x,y;
    mDb->getLocalXYinCell(hit,x,y);
    float dx = x - clu->x();
    float dy = y - clu->y();
    return sqrt(dx*dx + dy*dy);
}

//Fill in number of towers, cluster energy and weighted mean x/y [local grid] based on hits collection
void StFcsClusterMaker::updateCluster(StFcsCluster* clu){
    int nhit=clu->hits().size();
    double etot=0.0, xe=0.0, ye=0.0;
    double wtot=0.0, xw=0.0, yw=0.0;
    for(int i=0; i<nhit; i++){
	StFcsHit* hit=clu->hits()[i];	
	float x,y;
	mDb->getLocalXYinCell(hit,x,y);
	double e= hit->energy();	
	double w=log(e + 1.0 - mTowerEThreMoment);
	if(w<0.0) w=0.0;
	etot+= e; xe += x*e; ye += y*e;
	wtot+= w; xw += x*w; yw += y*w;
    }
    clu->setNTowers(nhit);
    clu->setEnergy(etot);
    if(wtot>0.0){
	clu->setX(xw/wtot);
	clu->setY(yw/wtot);
    }else if(etot>0.0) {
	clu->setX(xe/etot);
	clu->setY(ye/etot);
    }else{
	clu->setX(0.0);
        clu->setY(0.0);
    }
}

// perform cluster moment analysis, return kStErr if no tower found above ecut
int StFcsClusterMaker::clusterMomentAnalysis(StFcsCluster* clu, float ecut){
    int nhit=clu->hits().size();
    double wtot=0.0, xx=0.0, yy=0.0, sx=0.0, sy=0.0, sxy=0.0;
    for(int i=0; i<nhit; i++){
	StFcsHit* hit=clu->hits()[i];	
	float xh,yh;
	mDb->getLocalXYinCell(hit,xh,yh);
	double e= hit->energy();	
	double w=log(e + 1.0 - ecut);
	if(w>0.0){
	    wtot+= w; 
	    xx += xh*w; 
	    yy += yh*w;
	    sx += xh*xh*w; 
	    sy += yh*yh*w; 
	    sxy+= xh*yh*w; 
	}
    }
    if(wtot<=0.0){ //cluster has no tower above ecoff
      if(ecut>0.0){
	return kStErr; // return error so one can re-try with lower threshold
      }else{ //even with ecut=0.0, no energy!?
	clu->setSigmaMin(0.0);
	clu->setSigmaMax(0.0);
	return kStOK;
      }
    }else{
        double x = xx/wtot;
	double y = yy/wtot;
	double sigx  = sqrt(fabs(sx / wtot - std::pow(x, 2.0)));
	double sigy  = sqrt(fabs(sy / wtot - std::pow(y, 2.0)));
	double sigxy = sxy/wtot - x*y;
	double dsig2 = sigx*sigx - sigy*sigy;
	double aA = sqrt(dsig2 * dsig2 + 4.0 * sigxy * sigxy) + dsig2;
	double bB = 2.0 * sigxy;
	if (sigxy < 1e-10 && aA < 1e-10) {
	    bB = sqrt(dsig2 * dsig2 + 4.0 * sigxy * sigxy) - dsig2;
	    aA = 2.0 * sigxy;
	}                                                                                                                
	double theta = atan2(bB, aA);
	while (theta > M_PI / 2.0) {
	    theta -= M_PI;
	}                                                                                                               
	while (theta < -M_PI / 2.0) {
	    theta += M_PI;
	} 
	clu->setTheta(theta);
	clu->setSigmaMin(getSigma(clu, theta, ecut));
	clu->setSigmaMax(getSigma(clu, theta - M_PI/2.0, ecut));	
    }
    return kStOK;
}

float StFcsClusterMaker::getSigma(StFcsCluster* clu, double theta, float ecut){
    double sigma = 0;
    // 2-d vector vaxis define the axis
    TVector2 vaxis(cos(theta), sin(theta));
    double wtot =0.0;
    int nhit=clu->hits().size();
    for (int i=0; i<nhit; i++){ // loop over all hits in cluster
	StFcsHit* hit = clu->hits()[i];
        int det=hit->detectorId();
	float x,y;
	mDb->getLocalXYinCell(hit,x,y);
	// the 2-d vector from the "center" of cluster to tower
	TVector2 v1(x - clu->x(), y - clu->y());
	// perpendicular distance to the axis = length of the component of vector                                             
	// "v1" that is norm to "vaxis"                                                                                       
	double dis = (v1.Norm(vaxis)).Mod();
	// contribution to sigma                                                                                              
	double w = log(hit->energy() + 1.0 - ecut);
	if(w>=0.0){
	    wtot  += w;
	    sigma += w * dis * dis;
	}
    }
    return wtot > 0.0 ? (float)sqrt(sigma / wtot) : 0.0;
}

void StFcsClusterMaker::categorization(StFcsCluster* cluster){
    if(cluster->nTowers() < 5){
	cluster->setCategory(1);
    }else{
	const double sigma=cluster->sigmaMax();
        const double e    =cluster->energy();
        if(sigma > 1/2.5 + 0.003*e + 7.0/e){
            cluster->setCategory(2);
        }else if(sigma < 1/2.1 - 0.001*e + 2.0/e){
            cluster->setCategory(1);
        }else{
            cluster->setCategory(0);
        }	
    }    
}
