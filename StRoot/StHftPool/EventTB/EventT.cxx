#define __USE_GLOBAL__
#include "StMaker.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StEventInfo.h"
#include "StEventSummary.h"
#include "StTrack.h"
#include "StTrackNode.h"
#include "StPrimaryTrack.h"
#include "StGlobalTrack.h"
#include "StTrackDetectorInfo.h"
#include "StTrackGeometry.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StPionPlus.hh"
#include "StTpcHit.h"
#include "StPxlHit.h"
#include "StIstHit.h"
#include "StSsdHit.h"
#include "StIstHitCollection.h"
#include "StIstLadderHitCollection.h"
#include "StIstSensorHitCollection.h"
#include "StSsdHitCollection.h"
#include "StSsdLadderHitCollection.h"
#include "StSsdWaferHitCollection.h"
#include "StBTofCollection.h"
#include "StBTofHeader.h"
#include "StarRoot/THelixTrack.h"
#include "EventT.h"
#include "TrackT.h"
#include "HitT.h"
#include "HitMatchT.h"
#include "VertexT.h"
#include "TKey.h"
#include "TDirectory.h"
#include "TClass.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "TGeoMatrix.h"
#include "StPxlHitCollection.h"
#include "StDedxPidTraits.h"
#include "StBTofPidTraits.h"
#include "PhysicalConstants.h"
#include "StPxlDbMaker/StPxlDb.h"
#include "StIstDbMaker/StIstDb.h"
#include "StPxlDbMaker/StPxlDbMaker.h"
#include "StIstDbMaker/StIstDbMaker.h"
#include "StSsdDbMaker/StSsdDbMaker.h"
#include "StSsdDbMaker/StSstDbMaker.h"
#include "StSsdUtil/StSsdBarrel.hh"
ClassImp(EventTHeader);
ClassImp(EventT);
ClassImp(TrackT);
ClassImp(HitT);  
ClassImp(HitMatchT);
ClassImp(VertexT);

TClonesArray *EventT::fgTracks = 0;
TClonesArray *EventT::fgHits = 0;
TClonesArray *EventT::fgMatchHits = 0;
TClonesArray *EventT::fgVertices = 0;
static Int_t _debug =22;
//______________________________________________________________________________
EventT::EventT() : fIsValid(kFALSE)
{
  // Create an EventT object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  
  if (!fgTracks) fgTracks = new TClonesArray("TrackT", 1000);
  fTracks = fgTracks;
  fNtrack = 0;
  if (!fgHits) fgHits = new TClonesArray("HitT", 1000);
  fHits = fgHits;
  fNhit = 0;
  if (!fgMatchHits) fgMatchHits = new TClonesArray("HitMatchT", 1000);
  fMatchHits = fgMatchHits;
  fNmatchhit = 0;
  if (!fgVertices) fgVertices = new TClonesArray("VertexT", 1000);
  fVertices = fgVertices;
  fNvertex = 0;
}

//______________________________________________________________________________
EventT::~EventT()
{
  Clear();
}

//______________________________________________________________________________
Int_t  EventT::Build(StEvent *pEventT, UInt_t MinNoHits, Double_t pCut, StMaker *maker, StPxlDb *pxlDb, StIstDb *fIstDb) {

  Clear();

  Int_t iok    = 1;
  if (! pEventT) return iok;

  //Save current Object count

  fIsValid = kFALSE;


  // the way to access the db makers are not so consistent, need to be fixed later
  //StIstDbMaker *istDbMaker = (StIstDbMaker *)maker->GetMaker("istDb");
  THashList *istRot = fIstDb->getRotations();
  
  std::cout <<" JB : get SSD rotation matrices : " << std::endl;
  THashList        *ssdRot = gStSstDbMaker->GetRotations();
  //StSsdBarrel *mySsd =gStSsdDbMaker->GetSsd();

  UInt_t NprimVtx = pEventT->numberOfPrimaryVertices();
  if (! NprimVtx) return iok;
  StPrimaryVertex *pVertex=0;
  StThreeVectorF xyzP(-999,-999,-999);
  StMatrixF vCM(3,3);

  static const Int_t NoFitPointCutForGoodTrackT = 15;
  Int_t ibest = -1;
  Int_t nBestTracks = -1;
  Int_t nGoodTpcTracks;
  for (UInt_t ipr=0; ipr < NprimVtx ; ipr++) {
    pVertex = pEventT->primaryVertex(ipr);
    //if (! pVertex) continue;
    const StThreeVectorF& vtxPos = pVertex->position();
    Double32_t vtxXYZ[] = {vtxPos.x(), vtxPos.y(), vtxPos.z()};
    VertexT *vtx = AddVertexT();
    vtx->SetVertex(vtxXYZ);
    
    UInt_t nDaughters = pVertex->numberOfDaughters();
    vtx->SetNtracks(nDaughters);
    //      cout <<"  number of daughters : " << nDaughters << endl;
    nGoodTpcTracks = 0;
    for (UInt_t i=0; i < nDaughters; i++) {
      StTrack* pTrackT = pVertex->daughter(i);
      if ( pTrackT->fitTraits().numberOfFitPoints(kTpcId) >=  NoFitPointCutForGoodTrackT) nGoodTpcTracks++;
    }  
    if (nBestTracks < nGoodTpcTracks) {nBestTracks = nGoodTpcTracks; ibest = ipr;}
  }
  cout <<" # of tracks "  << nBestTracks << endl;
  if (ibest < 0) return iok;
  pVertex = pEventT->primaryVertex(ibest);
  xyzP = pVertex->position();
  vCM = pVertex->covariantMatrix();
  fNPTracks = pVertex->numberOfDaughters();
  fVertex[0] = xyzP.x();
  fVertex[1] = xyzP.y();
  fVertex[2] = xyzP.z();
  fCovariantMatrix[0] = vCM(1,1); // left triangular
  fCovariantMatrix[1] = vCM(1,2);
  fCovariantMatrix[2] = vCM(2,2);
  fCovariantMatrix[3] = vCM(1,3);
  fCovariantMatrix[4] = vCM(2,3);
  fCovariantMatrix[5] = vCM(3,3);

  StBTofCollection *btofColl = pEventT->btofCollection();
  fVzVpd = -9999.;
  fNVpdHits = 0;
  if(btofColl) {
    StBTofHeader *tofHeader = btofColl->tofHeader();
    if(tofHeader) {
      fVzVpd = tofHeader->vpdVz();
      int nE = tofHeader->numberOfVpdHits(east);
      int nW = tofHeader->numberOfVpdHits(west);
      fNVpdHits = nE*100 + nW;
    }
  }

  
  StEventInfo*      info = pEventT->info();
  Int_t ev = 0, run = 0, time = 0;
  if (info) {
    ev   = info->id();
    run  = info->runId();
    time = info->time();
  }

  cout <<" Event Infos : ev/run/time : " << ev<<"/"<<run<<"/"<<time << endl;
  StEventSummary* summary = pEventT->summary();
  Double32_t field = 0;
  if (summary) field = summary->magneticField();
  SetHeader(ev,run,time,field);
  SetFlag(1);
  //  Create and Fill the TrackT objects
//  cout <<" # of daughter tracks : "<< fNPTracks << endl;

  UInt_t nT = 0;

  const double SSD_Width_X = 2*3.8; // active area length is 3.75*2 --> make it a bit bigger
  const double SSD_Width_Z = 2*2.10;// active area width is 2.0 cm --> make it a bit bigger
  const double IST_Width_X = 3.8016;
  const double IST_Width_Z = 7.53;
  const double PXL_Width_X = 0.9605*2;
  const double PXL_Width_Z = 0.9936*2;


  // Load hits - PXL
  StPxlHitCollection* PxlHitCollection = pEventT->pxlHitCollection();
//  if(! PxlHitCollection){cout <<" no pxl hit collection !!!" << endl;}

  if(PxlHitCollection){
    cout << " Total Pxl Hits = " << PxlHitCollection->numberOfHits() << endl;
    UInt_t numberOfSectors=PxlHitCollection->numberOfSectors();
    for(UInt_t i=0;i<numberOfSectors;i++){
      StPxlSectorHitCollection* PxlSectorHitCollection=PxlHitCollection->sector(i);
      if(!PxlSectorHitCollection) continue;

      UInt_t numberOfLadders=PxlSectorHitCollection->numberOfLadders();
      for(UInt_t j=0;j<numberOfLadders;j++){
        StPxlLadderHitCollection* PxlLadderHitCollection=PxlSectorHitCollection->ladder(j);
        if(!PxlLadderHitCollection) continue;

        UInt_t numberOfSensors=PxlLadderHitCollection->numberOfSensors();
        for(UInt_t l=0;l<numberOfSensors;l++){
          StPxlSensorHitCollection* PxlSensorHitCollection=PxlLadderHitCollection->sensor(l);
          if(!PxlSectorHitCollection) continue;

          StSPtrVecPxlHit& vec = PxlSensorHitCollection->hits();
          if(vec.size()<=0) continue;

          if(_debug==2)
             cout<<"curr i/j/l (starting from 0) : " <<i<<"/"<<j<<"/"<<l<<" ==> StiPixelHitLoader - collection size: "<<vec.size()<<endl;

          UInt_t NoHits = vec.size();
          for (UInt_t ll = 0; ll < NoHits; ll++) {
            StPxlHit *hit = vec[ll];
            if(!hit) continue;
//            hit->Print("");
//            cout <<" layer/sector/ladder/sensor/idTruth/detectorId : " << (int)hit->layer()<<"/"<<(int)hit->sector() <<"/"<< (int)hit->ladder() <<"/"<< (int)hit->sensor() <<"/"<<(int)hit->idTruth()<<"/"<<(int)hit->detector() <<endl;
            int matId = ((int)hit->sector()-1)*40+(int)(hit->ladder()-1)*10+(int)(hit->sensor());
//            cout <<" -->matPix : " << matId << endl;
            Double_t globalPixHitPos[3] = {hit->position().x(),hit->position().y(),hit->position().z()};
            Double_t localPixHitPos[3]  = {hit->localPosition(0), hit->localPosition(1), hit->localPosition(2)};

//            cout << "globalPixHitPos = " << globalPixHitPos[0] << " " << globalPixHitPos[1] << " " << globalPixHitPos[2] << endl;
//            cout<< "localPixHitPos = " << localPixHitPos[0] << " " << localPixHitPos[1] << " " << localPixHitPos[2] << endl;            

            HitT *h = AddHitT();
            h->SetId(matId);
            h->Set(globalPixHitPos, localPixHitPos);
            h->SetNRawHits((UInt_t)hit->nRawHits());
          }  // end loop ll hits
        } // end loop l sensor
      } // end loop j ladder
    } // end loop i (sector
  } // end if 

  // load hits - IST
  StIstHitCollection *IstHitCollection = pEventT->istHitCollection();
  if(IstHitCollection) {
    cout << " Total Ist Hits = " << IstHitCollection->numberOfHits() << endl;
//    UInt_t numberOfLadders=IstHitCollection->numberOfLadders();
    for(UInt_t i=0;i<kIstNumLadders;i++) {
      StIstLadderHitCollection *IstLadderHitCollection = IstHitCollection->ladder(i);
      if(!IstLadderHitCollection) continue;
//      UInt_t numberOfSensors=IstLadderHitCollection->numberOfSensors();
      for(UInt_t j=0;j<kIstNumSensors;j++) {
        StIstSensorHitCollection *IstSensorHitCollection = IstLadderHitCollection->sensor(j);
        if(!IstSensorHitCollection) continue;
        StSPtrVecIstHit& vec = IstSensorHitCollection->hits();
        for(UInt_t l=0;l<vec.size();l++) {
          StIstHit *hit = vec[l];
          if(!hit) continue;
          int ladder = hit->getLadder();
          int sensor = hit->getSensor();
          int matId = (ladder-1)*6 + sensor + 1000;
          Double_t globalIstHitPos[3] = {hit->position().x(),hit->position().y(),hit->position().z()};
          Double_t localIstHitPos[3]  = {hit->localPosition(0), hit->localPosition(1), hit->localPosition(2)};

//          cout << "globalIstHitPos = " << globalIstHitPos[0] << " " << globalIstHitPos[1] << " " << globalIstHitPos[2] << endl;
//          cout<< "localIstHitPos = " << localIstHitPos[0] << " " << localIstHitPos[1] << " " << localIstHitPos[2] << endl;

          HitT *h = AddHitT();
          h->SetId(matId);
          h->Set(globalIstHitPos, localIstHitPos);
          h->SetNRawHits((UInt_t)hit->getNRawHits());
        } // end loop l hits
      } // end j sensor
    } // end i ladder
  } // end if

  // load hits - SSD
  StSsdHitCollection *SsdHitCollection = pEventT->ssdHitCollection();
  if(SsdHitCollection) {
    cout << " Total Ssd Hits = " << SsdHitCollection->numberOfHits() << endl;
    UInt_t numberOfLadders=SsdHitCollection->numberOfLadders();
    for(UInt_t i=0;i<numberOfLadders;i++) {
      StSsdLadderHitCollection *SsdLadderHitCollection = SsdHitCollection->ladder(i);
      if(!SsdLadderHitCollection) continue;
      UInt_t numberOfSensors=SsdLadderHitCollection->numberOfWafers();
      for(UInt_t j=0;j<numberOfSensors;j++) {
        StSsdWaferHitCollection *SsdWaferHitCollection = SsdLadderHitCollection->wafer(j);
        if(!SsdWaferHitCollection) continue;
        StSPtrVecSsdHit& vec = SsdWaferHitCollection->hits();
        for(UInt_t l=0;l<vec.size();l++) {
          StSsdHit *hit = vec[l];
          if(!hit) continue;
          Int_t ladder = hit->ladder();
          Int_t wafer  = hit->wafer();
          Int_t matId  = 7000 + 100*(wafer) + ladder ;
          Double_t globalSsdHitPos[3] = {hit->position().x(),hit->position().y(),hit->position().z()};
          Double_t localSsdHitPos[3]  = {hit->localPosition(0),hit->localPosition(1), hit->localPosition(2)};
	  //cout << "ladder/wafer/matId : " << ladder <<" " << " " << wafer <<" " << matId << endl;
	  //cout << "globalSsdHitPos = " << globalSsdHitPos[0] << " " << globalSsdHitPos[1] << " " << globalSsdHitPos[2] << endl;
	  //cout << "localSsdHitPos = " << localSsdHitPos[0] << " " << localSsdHitPos[1] << " " << localSsdHitPos[2] << endl;
	  //cout << "adcP : " << hit->getADC(0) <<" adcN  : " << hit->getADC(1) << endl; 
	  hit->Print();

          HitT *h = AddHitT();
          h->SetId(matId);
          h->Set(globalSsdHitPos, localSsdHitPos);
	  h->SetLadderWafer(ladder,wafer);
	  h->SetADC((int)hit->getADC(0),(int)hit->getADC(1));
        } // end loop l hits
      } // end j wafer
    } // end i ladder
  } // end if


  UInt_t onPXL2 = 0;
  UInt_t onPXL1 = 0;
  UInt_t onIST  = 0;
  UInt_t onSSD  = 0;


  StSPtrVecTrackNode& nodes = pEventT->trackNodes();
  cout << " TrackNode size = " << nodes.size() << endl;

  for (size_t t = 0; t < nodes.size(); t++) {
    if(_debug==1)cout <<" current track # :" << t << "/"<< nodes.size() << endl;
    StGlobalTrack *gTrackT = dynamic_cast<StGlobalTrack*>(nodes[t]->track(global));
    if(!gTrackT) continue;
    if(gTrackT->fitTraits().numberOfFitPoints(kTpcId)<NoFitPointCutForGoodTrackT) continue;

    StDcaGeometry *dcaGeometry = gTrackT->dcaGeometry();
    if(!dcaGeometry) { cout << " No dcaGeometry " << endl; continue; }

    StPrimaryTrack *pTrackT = dynamic_cast<StPrimaryTrack*>(nodes[t]->track(primary));
    if(pTrackT && pTrackT->vertex()!=pVertex) pTrackT = 0;
    if (! pTrackT) continue;
    StPhysicalHelixD helixI = pTrackT->geometry()->helix();
    StTrackDetectorInfo*  dInfo = gTrackT->detectorInfo();
    if (! dInfo) continue;

    TrackT *track = AddTrackT();

    int ii = 0;

    StPtrVecHit tpcHits = dInfo->hits(kTpcId);
    for(size_t ih = 0; ih < tpcHits.size(); ih++) {
      StTpcHit* aHit = dynamic_cast<StTpcHit *>(tpcHits[ih]);
      if(!aHit) continue;
      Double_t xyz[] = {aHit->position().x(), aHit->position().y(), aHit->position().z()};
      track->SetTpcHit(ii, xyz);
      ii++;
    }

    ii = 0;
    UInt_t npattern_ssd[2] = {0, 0};
    StPtrVecHit ssdHits = dInfo->hits(kSsdId);
    if(ssdHits.size()>0) cout << " Number of Ssd hits on this track " << ssdHits.size() << endl;
    for(size_t ih = 0; ih < ssdHits.size(); ih++) {
      StSsdHit* aHit = dynamic_cast<StSsdHit *>(ssdHits[ih]);
      if(!aHit) continue;
      npattern_ssd[ii] = aHit->wafer() + (aHit->ladder()-1)*16;
      Double_t xyz[] = {aHit->position().x(), aHit->position().y(), aHit->position().z()};
      track->SetSsdHit(ii, xyz);
      Double_t xyzL[] = {aHit->localPosition(0), aHit->localPosition(1), aHit->localPosition(2)};
      track->SetSsdHitLocal(ii, xyzL);
      ii++;
    }
    UInt_t nssdpattern = npattern_ssd[0] + npattern_ssd[1] * 1000;
    track->SetSsdHitPattern(nssdpattern);

    ii = 0;
    UInt_t npattern_ist[2] = {0, 0};
    StPtrVecHit istHits = dInfo->hits(kIstId);
    if(istHits.size()>0) cout << " Number of Ist hits on this track " << istHits.size() << endl;
    for(size_t ih = 0; ih < istHits.size(); ih++) {
      StIstHit* aHit = dynamic_cast<StIstHit *>(istHits[ih]);
      if(!aHit) continue;
      npattern_ist[ii] = aHit->getSensor() + (aHit->getLadder()-1)*6; 
      Double_t xyz[] = {aHit->position().x(), aHit->position().y(), aHit->position().z()};
      track->SetIstHit(ii, xyz);
      Double_t xyzL[] = {aHit->localPosition(0), aHit->localPosition(1), aHit->localPosition(2)};
      track->SetIstHitLocal(ii, xyzL);
      ii++;
    }
    UInt_t nistpattern = npattern_ist[0] + npattern_ist[1] * 1000;
    track->SetIstHitPattern(nistpattern);

    StPtrVecHit pxlHits = dInfo->hits(kPxlId);
    if(pxlHits.size()>0) cout << " Number of Pxl hits on this track " << pxlHits.size() << endl;
    UInt_t npts_pxl1 = 0;  // first layer
    UInt_t npts_pxl2 = 0;  // second layer
    UInt_t npattern_pxl[3] = {0, 0, 0};
    for(size_t ih = 0; ih < pxlHits.size(); ih++) {
      StPxlHit* aHit = dynamic_cast<StPxlHit *>(pxlHits[ih]);
      if(!aHit) continue;
//      cout << (*aHit) << endl;
      Double_t xyz[] = {aHit->position().x(), aHit->position().y(), aHit->position().z()};
      track->SetPxlHit(ih, xyz);
      Double_t xyzL[] = {aHit->localPosition(0), aHit->localPosition(1), aHit->localPosition(2)};
      track->SetPxlHitLocal(ii, xyzL);

      UInt_t sensorId = aHit->sensor() + (aHit->ladder()-1)*10 + (aHit->sector()-1)*40;      
      if(aHit->ladder()==1) {
        npts_pxl1++;
        npattern_pxl[0] = sensorId;
      }
      if(aHit->ladder()>=2 && aHit->ladder()<=4) {
        npts_pxl2++;
        npattern_pxl[npts_pxl2] = sensorId;
      }      
    }
    UInt_t npxlpattern = npattern_pxl[0] + npattern_pxl[1]*1000 + npattern_pxl[2]*1000000;
    track->SetPxlHitPattern(npxlpattern);
/*
    UInt_t npoints = dInfo->numberOfPoints(kTpcId) + 
                     dInfo->numberOfPoints(kSsdId) * 100 +
                     dInfo->numberOfPoints(kIstId) * 1000 +
                     npts_pxl2 * 10000 + 
                     npts_pxl1 * 100000;
*/
    UInt_t npoints = dInfo->numberOfPoints(kTpcId) +
                     ssdHits.size() * 100 +
                     istHits.size() * 1000 +
                     npts_pxl2 * 10000 +
                     npts_pxl1 * 100000;

//    if(npoints>100) 
//      cout << " NPoints = " << npoints << endl;

/*
    cout << " Number of Possible Points: tpc/ssd/ist/pxl = " << gTrackT->numberOfPossiblePoints(kTpcId)
         << "/" << gTrackT->numberOfPossiblePoints(kSsdId) << "/" << gTrackT->numberOfPossiblePoints(kIstId)
         << "/" << gTrackT->numberOfPossiblePoints(kPxlId) << endl;
    cout << " Number of Points: tpc/ssd/ist/pxl = " << dInfo->numberOfPoints(kTpcId) << "/"
         << dInfo->numberOfPoints(kSsdId) << "/" << dInfo->numberOfPoints(kIstId) << "/"
         << dInfo->numberOfPoints(kPxlId) << endl;
*/
//    cout << " Number of Points vector size: tpc/ssd/ist/pxl = " << tpcHits.size() << "/"
//         << ssdHits.size() << "/" << istHits.size() << "/" << pxlHits.size() << endl;
//    cout << " Hit Patter: ssd/ist/pxl = " << nssdpattern << "/" << nistpattern << "/" << npxlpattern << endl;

    StPhysicalHelixD dcaG_helix = dcaGeometry->helix();
    StThreeVectorF dcaG_origin = dcaGeometry->origin();
    StThreeVectorF dcaG_mom = dcaGeometry->momentum();
    int dcaG_q = dcaGeometry->charge();

    StPhysicalHelixD gHelix = gTrackT->geometry()->helix();
    StThreeVectorD origin = gHelix.origin();
    StThreeVectorD gmom = gHelix.momentum(field*kilogauss);
    int q = gHelix.charge(field*kilogauss);

//    cout << " global momentum = " << gmom << " charge = " << q << endl;
//    cout << " global momentum direct = " << gTrackT->geometry()->momentum() << " charge = " << gTrackT->geometry()->charge () << endl;
    

/*
    StSPtrVecTrackPidTraits &traits = gTrackT->pidTraits();
    UInt_t size = traits.size();
    StDedxPidTraits *pid;
    for (UInt_t i = 0; i < size; i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (! pid || pid->method() != kTruncatedMeanId) continue;
    }
*/
    static StTpcDedxPidAlgorithm PidAlgorithm;
    static StPionPlus *Pion = StPionPlus::instance();
    const StParticleDefinition *pd = gTrackT->pidTraits(PidAlgorithm);
    double nsigmaPi = -999.;
    if(pd) {
      nsigmaPi = PidAlgorithm.numberOfSigma(Pion);
    }
//    cout << " nsigma pion = " << nsigmaPi << endl;

//    double dca2d = gHelix.curvatureSignedDistance(xyzP.x(), xyzP.y());
//    double dca3d = gHelix.curvatureSignedDistance(xyzP);
    double dca2d = dcaG_helix.curvatureSignedDistance(xyzP.x(), xyzP.y());  
    double dca3d = dcaG_helix.curvatureSignedDistance(xyzP);

    track->SetPxPyPz(gmom.x(), gmom.y(), gmom.z());
    track->SetDcaPxPyPz(dcaG_mom.x(), dcaG_mom.y(), dcaG_mom.z());
    if(pTrackT) {
      StThreeVectorD pmom = pTrackT->geometry()->momentum();
      track->SetPPxPyPz(pmom.x(), pmom.y(), pmom.z());
    } else {
      track->SetPPxPyPz(0.,0.,0.);
    }
    track->SetOxOyOz(origin.x(), origin.y(), origin.z());
    track->SetDcaOxOyOz(dcaG_origin.x(), dcaG_origin.y(), dcaG_origin.z());
//    track->SetNpoint(npoints, q);
    track->SetNpoint(npoints, dcaG_q);
    track->SetNsigmaPi(nsigmaPi);
    track->SetDca2D(dca2d);
    track->SetDca3D(dca3d);

    StThreeVectorF firstP = dInfo->firstPoint();
#if 0
 // do Projections first SSD
      for (Int_t i_ladder = 0; i_ladder < 20; i_ladder++) {
	for (Int_t i_sensor = 0; i_sensor < 16; i_sensor++) {
	  UInt_t id = 7000 + 100*(i_sensor+1) + (i_ladder+1);
	  TGeoHMatrix *comb = (TGeoHMatrix *)ssdRot->FindObject(Form("R%04i", id));
	  //std::cout <<" id : "<< id << std::endl;
	  //comb->Print();
	  Double_t *rot = comb->GetRotationMatrix();
	  Double_t *tra = comb->GetTranslation();
	  /*
	  const StThreeVectorD normal(mySsd->mLadders[i_ladder]->mWafers[i_sensor]->n(0),	    
				      mySsd->mLadders[i_ladder]->mWafers[i_sensor]->n(1),
				      mySsd->mLadders[i_ladder]->mWafers[i_sensor]->n(2));				      
	  const StThreeVectorD middle(mySsd->mLadders[i_ladder]->mWafers[i_sensor]->x(0),	    
				      mySsd->mLadders[i_ladder]->mWafers[i_sensor]->x(1),
				      mySsd->mLadders[i_ladder]->mWafers[i_sensor]->x(2));
	  */				      
	  const StThreeVectorD normal(rot[2], rot[5], rot[8]);
	  const StThreeVectorD middle(tra);
	  const StThreeVectorD &o = helixI.origin();
	  const StThreeVectorD  n = helixI.cat(0.);
	  
	  Double_t sh = helixI.pathLength(middle, normal);
	  
	  if (sh <= 0 || sh > 1e3) continue; // dcaG geometry, projection pathLength should be positive
	  //if (TMath::Abs(sh) > 1e3) continue;
	  StThreeVectorD xyzG = helixI.at(sh);
	  Double_t xyzGPred[3] = {xyzG.x(), xyzG.y(), xyzG.z()};
	  Double_t uvPred[3];
	  //mySsd->mLadders[i_ladder]->mWafers[i_sensor]->MasterToLocal(xyzGPred,uvPred);
	  comb->MasterToLocal(xyzGPred, uvPred);
	  if (TMath::Abs(uvPred[0]) > SSD_Width_X / 2. ) continue;	 
	  if (TMath::Abs(uvPred[2]) > SSD_Width_Z / 2. ) continue;
	  
	  onSSD++;
	  
	  Double_t dirGPred[3] = {helixI.cx(sh), helixI.cy(sh), helixI.cz(sh)};
	  Double_t dxyzL[3];
	  //mySsd->mLadders[i_ladder]->mWafers[i_sensor]->MasterToLocal(dirGPred,dxyzL);
	  comb->MasterToLocalVect(dirGPred, dxyzL);
	  Double_t tuvPred[2] = {dxyzL[0] / dxyzL[1], dxyzL[2] / dxyzL[1]};
	  
	  if (!pEventT->ssdHitCollection()) continue;
	  
	  if (!pEventT->ssdHitCollection()->ladder(i_ladder)) continue;
	  
	  if (!pEventT->ssdHitCollection()->ladder(i_ladder)->wafer(i_sensor)) continue;
	  
	  StSPtrVecSsdHit &vec = pEventT->ssdHitCollection()->ladder(i_ladder)->wafer(i_sensor)->hits();
	  
	  if (vec.size() <= 0) continue;
	  
	  for (size_t ih = 0; ih < vec.size(); ih++) {
	    StSsdHit *hit = (StSsdHit *)vec[ih];
	    
	    if (!hit) continue;
	    
	    Double_t global[3] = {hit->position().x(), hit->position().y(), hit->position().z()};
	    Double_t local[3]  = {hit->localPosition(0), hit->localPosition(1), hit->localPosition(2)};

	    //cout <<" Prediction --> xGP :" << xyzGPred[0] <<" yGP :" << xyzGPred[1] <<" zGP :" << xyzGPred[2] << endl;  
	    //cout <<" Prediction --> uvPred[0] :" << uvPred[0] <<" uvPred[1] :" << uvPred[1] <<" uvPred[2] :" << uvPred[2] <<endl;
	    //cout <<" id/i_ladder/i_sensor : " << id <<" " << i_ladder << " " << i_sensor << std::endl;
	    //comb->Print();	    
	    cout << (*hit) << endl;
	    double rr[3];
	    comb->MasterToLocal(global,rr);
	    //std::cout <<" hitCol --> xg : " << global[0] <<"yl : " << global[1] <<" zl : " << global[2] << std::endl;
	    //std::cout <<" hitCol --> xl : " << local[0] <<"yl : " << local[1] <<" zl : " << local[2] << std::endl;
	    //std::cout <<" check  --> xl : " << rr[0] <<"yl : " << rr[1] <<" zl : " << rr[2] << std::endl;
	    comb->MasterToLocal(xyzGPred, uvPred);
	    HitMatchT *h = AddHitMatchT();
	    h->Set(global, local);
	    h->SetPred(xyzGPred, uvPred);
	    h->SettuvPred(tuvPred[0], tuvPred[1]);
	    h->SetDetId(id);
	    //h->SetNRawHits((UInt_t)hit->getNRawHits());
	    h->SetIndex2Track(nT);
	    (hit->idTruth() == gTrackT->idTruth())?h->SetIndex2Hit(1):h->SetIndex2Hit(0);  // need to map out later
	    h->SetTrackMom(dcaG_mom.perp(), dcaG_mom.pseudoRapidity(), dcaG_mom.phi());
	    h->SetTrackOrigin(dcaG_origin.x(), dcaG_origin.y(), dcaG_origin.z());
	    h->SetTrackNpoint(npoints * dcaG_q);
	    h->SetTrackFirstPointR(firstP.perp());
	    h->SetTrackFirstPointZ(firstP.z());
	    h->SetLadderWafer(i_ladder,i_sensor);
	    h->SetADC((int)hit->getADC(0),(int)hit->getADC(1));
	  }
	}
      }
#endif
    // do Projections
    // first IST
      //      std::cout << " start IST projections " << std::endl;
    for(int i_ladder = 0; i_ladder<24; i_ladder++) {
      for(int i_sensor = 0; i_sensor<6; i_sensor++) {
        UInt_t id = 1000 + i_ladder * 6 + i_sensor + 1;
	//TGeoHMatrix *comb = (TGeoHMatrix *) fIstDb->FindObject(Form("R%04i", id));
	//	TGeoHMatrix *comb = (TGeoHMatrix *) fIstDb->getHMatrixSensorOnGlobal(i_ladder,i_sensor+1);
        TGeoHMatrix *comb = (TGeoHMatrix *)istRot->FindObject(Form("R%04i",id));
//        if(t==0) comb->Print();
	//cout<<"Lomnitz: Starting projection on sensor "<<id<<" "<<endl;
	//cout<<"Rotation Matrix"<<endl;
	//comb->Print();
        Double_t *rot = comb->GetRotationMatrix();
        Double_t *tra = comb->GetTranslation();
        const StThreeVectorD normal(rot[1], rot[4], rot[7]);
        const StThreeVectorD middle(tra);
	//cout<<"Lomnitz: Looking for path length sensor "<<endl;
        Double_t sh = helixI.pathLength(middle, normal);
        if(sh<=0 || sh > 1e3) continue;  // dcaG geometry, projection pathLength should be positive
        StThreeVectorD xyzG = helixI.at(sh);
	//cout<<"Lomnitz: Global pred hit "<<endl;
        Double_t xyzGPred[3] = {xyzG.x(), xyzG.y(), xyzG.z()};
        Double_t uvPred[3];
        comb->MasterToLocal(xyzGPred,uvPred);
        if (TMath::Abs(uvPred[0]) > IST_Width_X/2. ) continue;
        if (TMath::Abs(uvPred[2]) > IST_Width_Z/2. ) continue;
        onIST++;
        Double_t dirGPred[3] = {helixI.cx(sh),helixI.cy(sh),helixI.cz(sh)};
        Double_t dxyzL[3];
        comb->MasterToLocalVect(dirGPred,dxyzL);
        Double_t tuvPred[2] = {dxyzL[0]/dxyzL[1], dxyzL[2]/dxyzL[1]};

        if(!pEventT->istHitCollection()) continue;
        if(!pEventT->istHitCollection()->ladder(i_ladder)) continue;
        if(!pEventT->istHitCollection()->ladder(i_ladder)->sensor(i_sensor)) continue;                                
        StSPtrVecIstHit& vec = pEventT->istHitCollection()->ladder(i_ladder)->sensor(i_sensor)->hits();
        if(vec.size()<=0) continue;
        for(size_t ih=0;ih<vec.size();ih++) {
          StIstHit *hit = (StIstHit*)vec[ih];
          if(!hit) continue;

          Double_t global[3] = {hit->position().x(),hit->position().y(),hit->position().z()};
          Double_t local[3]  = {hit->localPosition(0), hit->localPosition(1), hit->localPosition(2)};

//          cout << (*hit) << endl;

          HitMatchT *h = AddHitMatchT();
          h->Set(global, local);
          h->SetPred(xyzGPred, uvPred);
          h->SettuvPred(tuvPred[0], tuvPred[1]);
          h->SetDetId(id);
          h->SetNRawHits((UInt_t)hit->getNRawHits());
          h->SetIndex2Track(nT);
          h->SetIndex2Hit(0);  // need to map out later
          h->SetTrackMom(dcaG_mom.perp(), dcaG_mom.pseudoRapidity(), dcaG_mom.phi());
          h->SetTrackOrigin(dcaG_origin.x(), dcaG_origin.y(), dcaG_origin.z());
          h->SetTrackNpoint(npoints*dcaG_q);
          h->SetTrackFirstPointR(firstP.perp());
          h->SetTrackFirstPointZ(firstP.z());
        }
      }
    }
    // do projection to PXL
    for(int i_sector = 0; i_sector<10; i_sector++) {
      for(int i_ladder = 0; i_ladder<4; i_ladder++) {
        for(int i_sensor = 0; i_sensor<10; i_sensor++) {
          UInt_t id = i_sector * 40 + i_ladder * 10 + i_sensor + 1;
          TGeoHMatrix *comb = (TGeoHMatrix *)pxlDb->geoHMatrixSensorOnGlobal(i_sector+1, i_ladder+1, i_sensor+1);
//          if(t==0) comb->Print();

          Double_t *rot = comb->GetRotationMatrix();
          Double_t *tra = comb->GetTranslation();
          const StThreeVectorD normal(rot[1], rot[4], rot[7]);
          const StThreeVectorD middle(tra);
          Double_t sh = helixI.pathLength(middle, normal);
          if(sh<=0 || sh > 1e3) continue;  // dcaG geometry, projection pathLength should be positive
          StThreeVectorD xyzG = helixI.at(sh);
          Double_t xyzGPred[3] = {xyzG.x(), xyzG.y(), xyzG.z()};
          Double_t uvPred[3];
          comb->MasterToLocal(xyzGPred,uvPred);
          if (TMath::Abs(uvPred[0]) > PXL_Width_X/2. ) continue;
          if (TMath::Abs(uvPred[2]) > PXL_Width_Z/2. ) continue;
          if(i_ladder==0) onPXL1++;
          else onPXL2++;

          Double_t dirGPred[3] = {helixI.cx(sh),helixI.cy(sh),helixI.cz(sh)}; 
          Double_t dxyzL[3];
          comb->MasterToLocalVect(dirGPred,dxyzL);                         
          Double_t tuvPred[2] = {dxyzL[0]/dxyzL[1], dxyzL[2]/dxyzL[1]};

          if(!pEventT->pxlHitCollection()) continue;
          if(!pEventT->pxlHitCollection()->sector(i_sector)) continue;
          if(!pEventT->pxlHitCollection()->sector(i_sector)->ladder(i_ladder)) continue;
          if(!pEventT->pxlHitCollection()->sector(i_sector)->ladder(i_ladder)->sensor(i_sensor)) continue;
          StSPtrVecPxlHit& vec = pEventT->pxlHitCollection()->sector(i_sector)->ladder(i_ladder)->sensor(i_sensor)->hits();
          if(vec.size()<=0) continue;

          for(size_t ih=0;ih<vec.size();ih++) {
            StPxlHit *hit = (StPxlHit*)vec[ih];
            if(!hit) continue;

            Double_t global[3] = {hit->position().x(),hit->position().y(),hit->position().z()};
            Double_t local[3]  = {hit->localPosition(0), hit->localPosition(1), hit->localPosition(2)};

            HitMatchT *h = AddHitMatchT();
            h->Set(global, local);
            h->SetPred(xyzGPred, uvPred);
            h->SettuvPred(tuvPred[0], tuvPred[1]);
            h->SetDetId(id);
            h->SetNRawHits((UInt_t)hit->nRawHits());
            h->SetIndex2Track(nT);
            h->SetIndex2Hit(0);  // need to map out later
            h->SetTrackMom(dcaG_mom.perp(), dcaG_mom.pseudoRapidity(), dcaG_mom.phi());
            h->SetTrackOrigin(dcaG_origin.x(), dcaG_origin.y(), dcaG_origin.z());
            h->SetTrackNpoint(npoints*dcaG_q);
            h->SetTrackFirstPointR(firstP.perp());
            h->SetTrackFirstPointZ(firstP.z());
          }

        }
      }
    }

    nT++;
  }

  UInt_t val[4] = {onPXL1, onPXL2, onIST, onSSD};
  SetNPredHFT(val);
  //SetNPredHits(onPXL1+onPXL2, onIST, onSSD);
  //if(!onIST && !onPXL2 && !onPXL1) return iok;  // only fill events with tracks that can be projected to HFT
  LOG_INFO << " Number of predicted hits on PXL1/PXL2/IST/SSD = " << onPXL1 << "/" << onPXL2 << "/" << onIST << "/" << onSSD << endm;


  iok = 0;
  return iok;
}
//______________________________________________________________________________
TrackT *EventT::AddTrackT()
{
  // Add a new track to the list of tracks for this event.
  // To avoid calling the very time consuming operator new for each track,
  // the standard but not well know C++ operator "new with placement"
  // is called. If tracks[i] is 0, a new TrackT object will be created
  // otherwise the previous TrackT[i] will be overwritten.
  
  TClonesArray &tracks = *fTracks;
  TrackT *track = new(tracks[fNtrack++]) TrackT();
  //Save reference to last TrackT in the collection of Tracks
  return track;
}
//______________________________________________________________________________
HitT *EventT::AddHitT()
{
  // Add a new hit to the list of hits for this event.
  // To avoid calling the very time consuming operator new for each hit,
  // the standard but not well know C++ operator "new with placement"
  // is called. If hits[i] is 0, a new HitT object will be created
  // otherwise the previous HitT[i] will be overwritten.
  
  TClonesArray &hits = *fHits;
  HitT *hit = new(hits[fNhit++]) HitT();
  //Save reference to last HitT in the collection of Hits
  return hit;
}
//______________________________________________________________________________
HitMatchT *EventT::AddHitMatchT()
{
  // Add a new hit to the list of hits for this event.
  // To avoid calling the very time consuming operator new for each hit,
  // the standard but not well know C++ operator "new with placement"
  // is called. If hits[i] is 0, a new HitT object will be created
  // otherwise the previous HitT[i] will be overwritten.

  TClonesArray &matchhits = *fMatchHits;
  HitMatchT *hitmatch = new(matchhits[fNmatchhit++]) HitMatchT();
  //Save reference to last HitMatchT in the collection of Hits

  //cout << " Test -- create a new HitMatch here " << endl;
  return hitmatch;
}
//______________________________________________________________________________
VertexT *EventT::AddVertexT()
{
  // Add a new hit to the list of vertices for this event.
  // To avoid calling the very time consuming operator new for each hit,
  // the standard but not well know C++ operator "new with placement"
  // is called. If vertex[i] is 0, a new VertexT object will be created
  // otherwise the previous VertexT[i] will be overwritten.
  
  TClonesArray &vertices = *fVertices;
  VertexT *vertex = new(vertices[fNvertex++]) VertexT();
  //Save reference to last VertexT in the collection of Hits
  return vertex;
}
//______________________________________________________________________________
void EventT::Clear(Option_t * /*option*/)
{
  fTracks->Clear("C"); //will also call TrackT::Clear
  fHits->Clear("C"); //will also call HitT::Clear
  fVertices->Clear("C");
  fMatchHits->Clear("C");
}

//______________________________________________________________________________
void EventT::Reset(Option_t * /*option*/)
{
  // Static function to reset all static objects for this event
  //   fgTracks->Delete(option);
  
  delete fgTracks; fgTracks = 0;
  delete fgHits; fgHits = 0;
  delete fgVertices; fgVertices = 0;
  delete fgMatchHits; fgMatchHits = 0;
}

//______________________________________________________________________________
void EventT::SetHeader(Int_t i, Int_t run, Int_t date, Double32_t field)
{
  fNtrack = 0;
  fNhit = 0;
  fNvertex = 0;
  fNmatchhit = 0;
  fEvtHdr.Set(i, run, date, field);
}
//________________________________________________________________________________
void EventT::Print(Option_t *opt) const {
  cout << "Run/EventT\t" << fEvtHdr.GetRun() << "/" << fEvtHdr.GetEvtNum() << "\tDate " << fEvtHdr.GetDate() 
       << "\tField " << fEvtHdr.GetField() << endl;
  cout << "Total no. tracks " << GetTotalNoTracks() << "\tRecorded tracks " << GetNtrack() 
       << "\tRecorded hits " << GetNhit() << endl;
  TRVector vertex(3,GetVertex());
  TRSymMatrix cov(3,GetCovMatrix());
  cout << "Primary vertex " << vertex << endl;
  cout << "Its cov. matrix " << cov << endl;
  for (UInt_t i = 0; i < GetNtrack(); i++) {cout << i << "\t"; GetTrackT(i)->Print();}
  for (UInt_t i = 0; i < GetNhit(); i++) {cout << i << "\t"; GetHitT(i)->Print();}
//  for (UInt_t i = 0; i < GetNvertex(); i++) {cout << i << "\t"; GetVertexT(i)->Print();}
  
}
//________________________________________________________________________________
void TrackT::Print(Option_t *opt) const {
  cout << endl;
}
//________________________________________________________________________________
void HitT::Print(Option_t *opt) const {
}
//________________________________________________________________________________
void VertexT::Print(Option_t *opt) const {
}
//________________________________________________________________________________
void HitMatchT::Print(Option_t *opt) const {
}
