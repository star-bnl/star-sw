// #include "StEnumerations.h"
// #include "StEventTypes.h"
// #include "StFcsDbMaker/StFcsDbMaker.h"
// #include "StMessMgr.h"
// #include "StMuDSTMaker/COMMON/StMuTypes.hh"
// #include "StThreeVectorF.hh"
// #include "Stypes.h"

#include "StMuFcsAnaCheckFillClusPoint.h"

#include "StEvent.h"
#include "StFcsCollection.h"
#include "StFcsHit.h"
#include "StFcsCluster.h"
#include "StFcsPoint.h"

ClassImp(StMuFcsAnaCheckFillClusPoint)

StMuFcsAnaCheckFillClusPoint::StMuFcsAnaCheckFillClusPoint()
{
}

StMuFcsAnaCheckFillClusPoint::~StMuFcsAnaCheckFillClusPoint()
{
}

UInt_t StMuFcsAnaCheckFillClusPoint::LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* anadata)
{
  UInt_t loaded = 0;
  if( histman==0 ){ return loaded; }
  return loaded;
}

Int_t StMuFcsAnaCheckFillClusPoint::DoMake(StMuFcsAnaData* anadata)
{
  //std::cout << "========== StMuFcsAnaCheckFillClusPoint::DoMake Start ==========" << std::endl;
  StEvent* event = anadata->event();
  if( event==0 ){ LOG_ERROR << "StMuFcsAnaCheckFillClusPoint::DoMake - No StEvent" << endm; return kStErr; }
  StFcsCollection* FcsColl = event->fcsCollection();
  StFcsDb* FcsDb = anadata->fcsDb();
  //Fcs Collection
  if( !FcsColl ){ LOG_ERROR << "StMuFcsAnaCheckFillClusPoint::DoMake did not find FcsCollection" << endm; return kStErr; }
  
  //std::cout << "|hits:"<<hits << "|clusters:"<<clusters << "|points:"<<points << std::endl;
  for( UInt_t idet=0; idet<kFcsNDet; ++idet ){
    //std::cout << "+ |idet:"<<idet << "|maxdet:"<<kFcsNDet;
    const StSPtrVecFcsCluster& clusters = FcsColl->clusters(idet);
    unsigned int nc = FcsColl->numberOfClusters(idet);
    for( unsigned int iclus = 0; iclus < nc; iclus++ ){
      StFcsCluster* clu = (StFcsCluster*)clusters[iclus];
      float iclu_x = clu->x();
      float iclu_y = clu->y();
      float iclu_energy = clu->energy();
      
      StThreeVectorD iclu_pos = FcsDb->getStarXYZfromColumnRow( idet, iclu_x, iclu_y );
      StLorentzVectorD iclu_p = FcsDb->getLorentzVector( iclu_pos, iclu_energy, 0 );
      std::cout << " + |idet:"<<idet <<"|iclus:"<<iclus << "|clusid:"<<clu->id() << "|npoints:"<<clu->nPoints()<<"|sigmamin:"<<clu->sigmaMin() << "|sigmamax:"<<clu->sigmaMax() << std::endl;
      if( clu->sigmaMin()<0.00001 || clu->sigmaMax()<0.00001 ){
	StPtrVecFcsHit& cluhits = clu->hits();
	for(unsigned int itow=0; itow<cluhits.size(); ++itow ){
	  StFcsHit* hit = (StFcsHit*)cluhits[itow];
	  std::cout << "    * |hit:"<<itow << "|col:"<< FcsDb->getColumnNumber(hit->detectorId(),hit->id()) << "|row:"<<FcsDb->getRowNumber(hit->detectorId(),hit->id()) << std::endl;
	}
      }
      //std::cout << "Cluster|detid:"<<ph->mDetId << "|mX:"<<ph->mX << "|mY:"<<ph->mY << "|mZ:"<<ph->mZ << std::endl;
    }
    
    //std::cout << "===== EventId:"<< mEvtInfo->mEvent <<" =====" << std::endl;
    const StSPtrVecFcsPoint& points = FcsColl->points(idet);
    unsigned int np = FcsColl->numberOfPoints(idet);
    for( unsigned int ipoint=0; ipoint<np; ++ipoint ){
      StFcsPoint* point = (StFcsPoint*)points[ipoint];
      float ipoi_x = point->x();
      float ipoi_y = point->y();
      float ipoi_energy = point->energy();
      
      StThreeVectorD ipoi_pos = FcsDb->getStarXYZfromColumnRow( idet, ipoi_x, ipoi_y );
      StLorentzVectorD ipoi_p = FcsDb->getLorentzVector(ipoi_pos, ipoi_energy, 0);

      //std::cout << "Point|detid:"<<idet << "|mX:"<<ipoi_pos[0] << "|mY:"<<ipoi_pos[1] << "|mZ:" <<ipoi_pos[2] << std::endl;
      std::cout << " - |idet:"<<idet << "|ipoint:"<<ipoint << "|clusid:"<<point->parentClusterId() << "|nparentpoints:"<<point->nParentClusterPhotons()<</*"|parentclusternpoints:"<< point->cluster()->nPoints() <<"|sigmamin:"<< point->cluster()->sigmaMin() << "|sigmamax:"<<point->cluster()->sigmaMax() <<*/ std::endl;
    }//i point
  }//fcs dets

  //std::cout << "|ncandidates:"<<ncandidates <<"|clustersize:"<<clustersize <<"|Size:"<<mPhArr->GetEntriesFast() << std::endl;
  return kStOk;
}


			  
