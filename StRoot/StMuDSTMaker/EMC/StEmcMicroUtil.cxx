#include "StEmcMicroUtil.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StEmcMicroEvent.h"
#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/filters/StEmcFilter.h"

ClassImp(StEmcMicroUtil)

StEmcMicroUtil::StEmcMicroUtil()
{
  for(Int_t i =0;i<4;i++) mGeo[i]=StEmcGeom::getEmcGeom(detname[i].Data());
}
StEmcMicroUtil::~StEmcMicroUtil()
{
	//if(mV0Tracks) delete mV0Tracks;
}
void StEmcMicroUtil::clearGarbage()
{
	for(Int_t i=0;i<mNV0Tracks;i++)
	{
		if(mV0Tracks[i]) { delete mV0Tracks[i]; mV0Tracks[i]=NULL; }
	}
	mNV0Tracks=0;
}
StEmcMicroEvent* StEmcMicroUtil::getMicroEvent(StEvent *event)
{
  if(!event) return NULL;
  
  mStEvent = event;
  processStEvent();
  
  return mMicroEvent;
}
StEvent* StEmcMicroUtil::getStEvent(StEmcMicroEvent* event)
{
  if(!event) return NULL;
  
  mMicroEvent = event;  
  processMicroEvent();
  
  return mStEvent;
}
void StEmcMicroUtil::processStEvent()
{
  mMicroEvent = new StEmcMicroEvent();
  processStEventInfo();
  processStEventTracks();
  if(mDoSaveV0) processStEventV0();
  if(mDoSaveEmc) processStEventEMC();
  if(mDoSaveFpd) processStEventFPD();
}
void StEmcMicroUtil::processStEventInfo()
{
  cout <<"filling general info\n";
  
  //Magnetic field
  Float_t BF=0.5;
  StEventSummary* summary = mStEvent->summary();
  if(summary) BF = summary->magneticField()/10.;
  mMicroEvent->setBField(BF);
  
  StL0Trigger* pTrigger = mStEvent->l0Trigger();
  if(pTrigger) 
  {
    mMicroEvent->setL0TriggerWord(pTrigger->triggerWord());
    mMicroEvent->setToken(pTrigger->triggerToken());
    mMicroEvent->setBunchCrossing(pTrigger->bunchCrossingId());
    mMicroEvent->setBunchCrossing7bit(pTrigger->bunchCrossingId7bit(mStEvent->runId()));
    //mMicroEvent->setBunchCrossing7bit(pTrigger->bunchCrossingId7bit());
    mMicroEvent->setSpinBits(pTrigger->spinBits());
  }

  // Get event id 
  mMicroEvent->setEventID((Int_t)(mStEvent->id()));
  mMicroEvent->setRunID((Int_t)(mStEvent->runId()));
  mMicroEvent->setEventTime((Int_t)mStEvent->time());
    
  // Get primary vertex position (if any)
  StPrimaryVertex* primaryVertex = mStEvent->primaryVertex(0);
  if(primaryVertex)
    {
      const StThreeVectorF& vertex = primaryVertex->position();
      mMicroEvent->setVertexPos(vertex.x(),vertex.y(),vertex.z());
      // Get initial multiplicity before TrackCuts 
      UInt_t origMult = primaryVertex->numberOfDaughters(); 
      mMicroEvent->setOrigMult(origMult);
    }

  // include trigger (ZDC, CTB and BBC)
  Int_t ctb  = 0;
  Int_t zdce = 0;
  Int_t zdcw = 0;
  Float_t zdcvz = 0;
  
  Int_t bbcw = 0;
  Int_t bbce = 0;
  Int_t bbcnh = 0;
  Float_t bbcvz = 0; 
  
  StTriggerDetectorCollection *triggers = mStEvent->triggerDetectorCollection();
  if (triggers)	
  {
    StCtbTriggerDetector &CTB = triggers->ctb();
    StZdcTriggerDetector &ZDC = triggers->zdc();
    StBbcTriggerDetector &BBC = triggers->bbc();
    StEmcTriggerDetector &EMC = triggers->emc();
    // get CTB
    for (UInt_t slat=0; slat<CTB.numberOfSlats(); slat++) 
      for (UInt_t tray=0; tray<CTB.numberOfTrays();tray++) 
        ctb += (Int_t)CTB.mips(tray,slat,0);

    //get ZDCe and ZDCw        
    zdce = (Int_t)ZDC.adcSum(east);
    zdcw = (Int_t)ZDC.adcSum(west);
    zdcvz = (Float_t)ZDC.vertexZ();
    
    bbcw = (Int_t)BBC.adcSumWest();
    bbce = (Int_t)BBC.adcSumEast();
    bbcnh = (Int_t)BBC.nHitAll();
    bbcvz = (Float_t)BBC.zVertex();
    
    for(Int_t i=0;i<300;i++)
    {
      Int_t HT = EMC.highTower(i);
      Int_t PA = EMC.patch(i);
      mMicroEvent->setHighTower(i,HT);
      mMicroEvent->setPatch(i,PA);
    }
  } 
  mMicroEvent->setCTB(ctb);
  mMicroEvent->setZDCe(zdce);
  mMicroEvent->setZDCw(zdcw);
  mMicroEvent->setZVertexZDC(zdcvz);
  mMicroEvent->setBBCw(bbcw);
  mMicroEvent->setBBCw(bbce);
  mMicroEvent->setBBCNHits(bbcnh);
  mMicroEvent->setZVertexBBC(bbcvz);
}
void StEmcMicroUtil::processStEventTracks()
{
  cout <<"filling tracks\n";  
  int totalp=0,goodp=0,totalg=0,goodg=0;

  StSPtrVecTrackNode& trackNode = mStEvent->trackNodes();
  for (unsigned int j=0; j < trackNode.size(); j++) 
  {
    StTrack*  gTrack = (trackNode[j]->track(global));
    StTrack*  pTrack = (trackNode[j]->track(primary));
    
    // first primary track
    if (pTrack && mDoSavePrimaries)// && pTrack->flag() > 0) 
    {
      totalp++;      
      if(mPFilter->accept(pTrack))
      {
        // Instantiate new StEmcMicroTrack
        StEmcMicroTrack* MicroPTrack = new StEmcMicroTrack();
        createTrack(pTrack,MicroPTrack);
        if(MicroPTrack) 
        {
          MicroPTrack->setTrackNodeNumber(j);
          mMicroEvent->addPrimaryTrack(MicroPTrack); 
          goodp++;
        }
      }
    }

    if (gTrack && mDoSaveGlobals)
    {
      totalg++;
      if (mGFilter->accept(gTrack)) 
      {
        StEmcMicroTrack* MicroGTrack=new StEmcMicroTrack();
        createTrack(gTrack,MicroGTrack);
        if(MicroGTrack) 
        {
          MicroGTrack->setTrackNodeNumber(j);
          mMicroEvent->addGlobalTrack(MicroGTrack); 
          goodg++;
        }
      }
    }    
  }
  cout <<"Total primary tracks = "<<totalp<<"   good = "<<goodp<<endl;
  cout <<"Total global  tracks = "<<totalg<<"   good = "<<goodg<<endl;  
  cout <<"finished filing tracks\n";
}
void StEmcMicroUtil::createTrack(StTrack* track,StEmcMicroTrack* MicroTrack)
{
  StThreeVectorF p = track->geometry()->momentum();
  if(p.mag()==0) return;

  StThreeVectorF o = track->geometry()->origin();

  MicroTrack->setP(p.mag());
  MicroTrack->setEta(p.pseudoRapidity());
  MicroTrack->setPhi(p.phi());
  MicroTrack->setCurvature(track->geometry()->curvature());

  MicroTrack->setFlag(track->flag());
  
  MicroTrack->setOrigin(o.x(),o.y(),o.z());
  
  MicroTrack->setCharge(track->geometry()->charge());
  
  MicroTrack->setDca(track->impactParameter());

  Float_t dcaSigned = calcDcaSigned(track);
  MicroTrack->setDcaSigned(dcaSigned);

  MicroTrack->setChi2((Float_t)(track->fitTraits().chi2()));
  MicroTrack->setFitPts(track->fitTraits().numberOfFitPoints());
  MicroTrack->setMaxPts(track->numberOfPossiblePoints());

  if(track->detectorInfo())
    MicroTrack->setNhits(track->detectorInfo()->numberOfPoints(kTpcId));
  
  // dE/dx
  StPtrVecTrackPidTraits traits = track->pidTraits(kTpcId);
  unsigned int size = traits.size();

  if (size) 
  {
    StDedxPidTraits* pid;
    for (unsigned int i = 0; i < traits.size(); i++) 
    {
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid && pid->method() == kTruncatedMeanId) break;
    }
    assert(pid); 
    
    double dEdX = (double)pid->mean();
    double npt = (double)pid->numberOfPoints();
    double dEdXErr = (double)pid->errorOnMean();
    MicroTrack->setDedxErr(dEdXErr);
    MicroTrack->setDedx((Float_t) dEdX);
    MicroTrack->setNdedxPts((Int_t)npt);  
  }

  MicroTrack->setTrackLength(track->length());

  return;
}
Float_t StEmcMicroUtil::calcDcaSigned(StTrack* track) 
{

  // find the distance between the center of the circle and pos (vertex).
  // if the radius of curvature > distance, then call it positive
  // Bum Choi
  StThreeVectorF pos = mStEvent->primaryVertex(0)->position();

  double xCenter = track->geometry()->helix().xcenter();
  double yCenter = track->geometry()->helix().ycenter();
  double radius = 1.0/track->geometry()->helix().curvature();

  double dPosCenter = ::sqrt( (pos.x() - xCenter) * (pos.x() - xCenter) +
			    (pos.y() - yCenter) * (pos.y() - yCenter));

  return (Float_t) (radius - dPosCenter);
}
void StEmcMicroUtil::processStEventV0()
{
  cout <<"Filling V0´s\n";
  Int_t nV=0;
  StSPtrVecV0Vertex& v0=mStEvent->v0Vertices();
  if(v0.size()==0) return;
  
  for(Int_t i = 0;i<(Int_t)v0.size();i++)
  {
    StV0Vertex *v = v0[i];
    if(v) if(mGFilter->accept(v))
    {
      nV++;
      StEmcMicroV0* mv = new StEmcMicroV0();
      
      for(Int_t j=0;j<2;j++)
      {
        StTrack* tr = v->daughter(j);
        if(tr)
        {
          StEmcMicroTrack *mtr = new StEmcMicroTrack();
          createTrack(tr,mtr);
          if(mtr) mv->setDaughter(j,mtr);          
        }
      }
      StThreeVectorF pos=v->position();
      mv->setVertex(pos.x(),pos.y(),pos.z());
      mMicroEvent->addV0(mv);
    }
  }
  cout <<"Total V0´s accepted = "<<nV<<endl;
}
void StEmcMicroUtil::processStEventEMC()
{
  StEmcCollection* emccol=mStEvent->emcCollection();
  if(!emccol) return;
  StEmcMicroCollection* Microemc=new StEmcMicroCollection();
  
  Int_t HitsId[18000];
      
  // starting by hits;
    
  for(Int_t d=0; d<4; d++)
  {  
    Int_t EmcDet=d+1;
    for(Int_t i=0;i<18000;i++) HitsId[i]=-1;
    
    StDetectorId id = static_cast<StDetectorId>(d+kBarrelEmcTowerId);
    StEmcDetector* detector=emccol->detector(id);
    if(!detector) cout <<detname[d].Data()<<" not loaded\n";
    if(detector)
    {
      // hits first
      cout <<"Filling hits for detector "<<detname[d].Data()<<endl;
                          
      Int_t HitIndex=0;
      for(UInt_t j=1;j<121;j++)
      {
        StEmcModule* module = detector->module(j);
        if(module)
        {
          StSPtrVecEmcRawHit& rawHit=module->hits();
          Int_t nhits=(Int_t)rawHit.size();
          if(nhits>0)
            for(Int_t k=0;k<nhits;k++)
            {
              Int_t m = rawHit[k]->module();
              Int_t e = rawHit[k]->eta();
              Int_t s = abs(rawHit[k]->sub());
              Int_t adc = rawHit[k]->adc();
              Float_t energy = rawHit[k]->energy();
              
              StEmcMicroHit* Microhit = new StEmcMicroHit();
          
              Microhit->setModule(m);
              Microhit->setEta(e);
              Microhit->setSub(s);
              Microhit->setAdc(adc);
              Microhit->setEnergy(energy);
                      
              Microemc->addHit(EmcDet,Microhit);
              Int_t rid;
              mGeo[d]->getId(m,e,s,rid);
              HitsId[rid-1] = HitIndex;
              HitIndex++;
              //delete Microhit; Microhit=NULL;
            }
          }      
        } 
      
      // now clusters
      if(detector->cluster())
      {
        StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
        Int_t totalcluster=(Int_t)cluster.size();
        cout <<"Filling clusters for detector "<<detname[d].Data()<<endl;
        cout <<"Number of clusters = "<<totalcluster<<endl;
        if(totalcluster>0)
          for(Int_t j=0;j<totalcluster;j++)
          {
            StEmcMicroCluster *Microcl=new StEmcMicroCluster();
          
            Microcl->setEta(cluster[j]->eta());
            Microcl->setPhi(cluster[j]->phi());
            Microcl->setSigmaEta(cluster[j]->sigmaEta());
            Microcl->setSigmaPhi(cluster[j]->sigmaPhi());
            Microcl->setEnergy(cluster[j]->energy());
        
            StPtrVecEmcRawHit& rawHit=cluster[j]->hit();
            Int_t nhit=(Int_t)rawHit.size();

            for(Int_t k=0;k<nhit;k++)
            {
              Int_t m = rawHit[k]->module();
              Int_t e = rawHit[k]->eta();
              Int_t s = abs(rawHit[k]->sub());
              Int_t rid;
              mGeo[d]->getId(m,e,s,rid);
              Int_t index = HitsId[rid-1];
              if(index!=-1) Microcl->addHit(Microemc->getHit(EmcDet,index));
            }
            Microemc->addCluster(EmcDet,Microcl);
            //delete Microcl; Microcl=NULL;
          }
      }  // if detector->cluster
    } // if detector
  } // loop detector
  
  // now going to points.... 
  cout <<"Filling points "<<endl;
  
  StSPtrVecEmcPoint& points=emccol->barrelPoints();
  Int_t npoints=points.size();
  if(npoints>0)
  {
    for(Int_t p=0;p<npoints;p++)
    {
      StEmcPoint* point=points[p];
      StThreeVectorF position=point->position();
      StEmcMicroPoint *Micropt=new StEmcMicroPoint();
      Micropt->setEta(position.pseudoRapidity());
      Micropt->setPhi(position.phi());
      Micropt->setDeltaEta(point->deltaEta());
      Micropt->setDeltaPhi(point->deltaPhi());
      Micropt->setEnergy(point->energy());
      Micropt->setChiSquare(point->chiSquare());
      
      for(Int_t d=0;d<4;d++)
      {
        Int_t det =d+1;
        StDetectorId detid=static_cast<StDetectorId>(d+kBarrelEmcTowerId);
        StPtrVecEmcCluster& cluster=point->cluster(detid);
        Int_t ptnc=0;
        ptnc=cluster.size();
        for(Int_t i=0;i<ptnc;i++) if(cluster[i])
        {
          Float_t eta = cluster[i]->eta();
          Float_t phi = cluster[i]->phi();
          for(Int_t j=0;j<Microemc->getNClusters(det);j++)
          {
            StEmcMicroCluster *cl=Microemc->getCluster(det,j);
            if(eta == cl->getEta() && phi==cl->getPhi())
            {
              Micropt->addCluster(det,cl);
              goto cont;
            }
          }
          cont: continue;
        }
      } // loop detector
      Microemc->addPoint(Micropt);
      //delete Micropt; Micropt=NULL;
    } // loop points

  }// npoint >0
  
  mMicroEvent->setEmc(Microemc);
  cout <<"Finished filling EMC\n";
  return;

}
void StEmcMicroUtil::processStEventFPD()
{
  cout <<"Filling FPD ...\n";
  StFpdCollection * fpd = mStEvent->fpdCollection();
  if(!fpd) return;
  
  StFpdMicroCollection *Microfpd = new StFpdMicroCollection();
  
  Microfpd->setToken(fpd->token());             
  Microfpd->setSumAdcNorth(fpd->sumAdcNorth());       
  Microfpd->setSumAdcSouth(fpd->sumAdcSouth());
  Microfpd->setSumAdcTop(fpd->sumAdcTop());
  Microfpd->setSumAdcBottom(fpd->sumAdcBottom());
  Microfpd->setSumAdcPreShower1(fpd->sumAdcPreShower1());
  Microfpd->setSumAdcPreShower2(fpd->sumAdcPreShower2());
  Microfpd->setSumAdcSmdX(fpd->sumAdcSmdX());
  Microfpd->setSumAdcSmdY(fpd->sumAdcSmdY());
  
  mMicroEvent->setFpd(Microfpd);
  cout <<"Finished filling FPD\n";       

}
void StEmcMicroUtil::processMicroEvent()
{
  clearGarbage();
	mStEvent = new StEvent;
  processMicroEventInfo();
  processMicroEventTracks();
  processMicroEventV0();
  processMicroEventEMC();
  return;
}
void StEmcMicroUtil::processMicroEventInfo()
{
  StEventSummary *summary=new StEventSummary();
  summary->setMagneticField(mMicroEvent->getBField()*10.);
  
  mStEvent->setSummary(summary);
  
  mStEvent->setRunId(mMicroEvent->getRunID());
  mStEvent->setId(mMicroEvent->getEventID());
  mStEvent->setTime(mMicroEvent->getEventTime());
   
  StL0Trigger *L0=new StL0Trigger();
  L0->setTriggerWord(mMicroEvent->getL0TriggerWord());
  mStEvent->setL0Trigger(L0);
  
  StTriggerDetectorCollection *triggers = new StTriggerDetectorCollection();
  
  //StCtbTriggerDetector &CTB = triggers->ctb();
  StZdcTriggerDetector &ZDC = triggers->zdc();
  //StBbcTriggerDetector &BBC = triggers->bbc();
  StEmcTriggerDetector &EMC = triggers->emc();
  
  ZDC.setAdcSum(east,mMicroEvent->getZDCe());
  ZDC.setAdcSum(west,mMicroEvent->getZDCw());
	ZDC.setAdcSum((Float_t)(mMicroEvent->getZDCe()+mMicroEvent->getZDCw()));
  ZDC.setVertexZ(mMicroEvent->getZVertexZDC());
  
  for(Int_t i=0;i<300;i++)
  {
    EMC.setHighTower(i,mMicroEvent->getHighTower(i));
    EMC.setPatch(i,mMicroEvent->getPatch(i));
  }
  
  mStEvent->setTriggerDetectorCollection(triggers);
   
  StPrimaryVertex *vertex=new StPrimaryVertex();
  StThreeVectorF p(mMicroEvent->getVertexX(),mMicroEvent->getVertexY(),mMicroEvent->getVertexZ());
  vertex->setPosition(p);
  mStEvent->addPrimaryVertex(vertex);
  return;
}
void StEmcMicroUtil::processMicroEventTracks()
{
  //filling tracks
  // restoring original StEvent track node
  StSPtrVecTrackNode& node=mStEvent->trackNodes();
  StPrimaryVertex* vertex = mStEvent->primaryVertex(0);
  
  Int_t maxNode=0,gnode,pnode;
  
  Int_t ng=mMicroEvent->getNGlobalTrack();
  Int_t np=mMicroEvent->getNPrimaryTrack();
  
  Int_t max=ng;
  if (np>ng) max=np;
  
  for(Int_t i=0;i<max;i++)
  {
    if(i<ng) gnode=mMicroEvent->getGlobalTrack(i)->getTrackNode(); else gnode=0;
    if(i<np) pnode=mMicroEvent->getPrimaryTrack(i)->getTrackNode(); else pnode=0;
    if(gnode>maxNode) maxNode=gnode;
    if(pnode>maxNode) maxNode=pnode;
  }
  //Int_t nnodes=maxNode+1;
  StTrackNode *trackNode[10000];
  for(Int_t i=0;i<10000;i++) trackNode[i] = NULL;
  // filling tracks
  for(Int_t i=0;i<max;i++)
  {
    if(i<ng) //global tracks
    {
      StEmcMicroTrack *gMicroTrack=mMicroEvent->getGlobalTrack(i);
      gnode = gMicroTrack->getTrackNode();
      if(!trackNode[gnode]) trackNode[gnode]=new StTrackNode();

      StGlobalTrack *gTrack = new StGlobalTrack(); 
      createTrack(gMicroTrack,gTrack);              
      trackNode[gnode]->addTrack(gTrack);
    }
    if(i<np) //primary tracks
    {
      StEmcMicroTrack *pMicroTrack=mMicroEvent->getPrimaryTrack(i);
      pnode = pMicroTrack->getTrackNode();
      if(!trackNode[pnode]) trackNode[pnode]=new StTrackNode();

      StPrimaryTrack *pTrack = new StPrimaryTrack();
      createTrack(pMicroTrack,pTrack);      
      pTrack->setVertex(vertex);
      vertex->addDaughter(pTrack);
      trackNode[pnode]->addTrack(pTrack);
    }
  }
  for(Int_t i=0;i<10000;i++) // cleaning empty nodes
    if(trackNode[i])
    {
      Int_t entries=trackNode[i]->entries();
      if(entries!=0) node.push_back(trackNode[i]);
      else delete trackNode[i];
    }

}
void StEmcMicroUtil::createTrack(StEmcMicroTrack* MicroTrack,StTrack* Track)
{
  Float_t psi = MicroTrack->getPhi();
  Float_t theta=2*atan(exp(-MicroTrack->getEta()));
  Short_t charge = (Short_t)MicroTrack->getCharge();
  Float_t curvature= MicroTrack->getCurvature();
   
  Track->setLength(MicroTrack->getTrackLength());
  Track->setImpactParameter(MicroTrack->getDca());
  StThreeVectorF o(MicroTrack->getOrigin(0),MicroTrack->getOrigin(1),MicroTrack->getOrigin(2));
  StThreeVectorF p(MicroTrack->getPt()*cos(psi),MicroTrack->getPt()*sin(psi),MicroTrack->getP()*cos(theta));
    
  Track->setGeometry(new StHelixModel(charge,psi,curvature,theta,o,p,0));
  Track->addPidTraits(new StDedxPidTraits(kTpcId,kTruncatedMeanId,MicroTrack->getNdedxPts(),MicroTrack->getDedx(),MicroTrack->getDedxErr()));                                              
                                               
  StTrackDetectorInfo *info=new StTrackDetectorInfo();
  info->setNumberOfPoints(MicroTrack->getNhits());
  StSPtrVecTrackDetectorInfo& infoNode = mStEvent->trackDetectorInfo();
  infoNode.push_back(info);
  Track->setDetectorInfo(info);
  
  Float_t a[2],b[15];
  a[0]=MicroTrack->getChi2();
  StTrackFitTraits *traits=new StTrackFitTraits(0,MicroTrack->getFitPts(),a,b);
  Track->setFitTraits(*traits);
  delete traits;
  
  Track->setFlag((Short_t)MicroTrack->getFlag());
  
  return;
}
void StEmcMicroUtil::processMicroEventV0()
{
  mNV0Tracks=0;
	Int_t nv0 = mMicroEvent->getNV0();
  if (nv0==0) return;
  StSPtrVecV0Vertex&  v0=mStEvent->v0Vertices();
  for(Int_t i=0;i<nv0;i++)
  {
    StEmcMicroV0 *mv0 = mMicroEvent->getV0(i);
    if(mv0)
    {
      StV0Vertex *v=new StV0Vertex();
      for(Int_t j=0;j<2;j++)
      {
        StEmcMicroTrack *mt=mv0->getDaughter(j);
        if(mt)
        {
          StGlobalTrack *t=new StGlobalTrack();
          createTrack(mt,t);
          if(t)
          {
            StThreeVectorF p = t->geometry()->momentum();
            v->setMomentumOfDaughter((StChargeSign)j,p);
						v->addDaughter(t);
						if(mNV0Tracks<MAXV0TRACKS)
						{
							mV0Tracks[mNV0Tracks] = t;
							mNV0Tracks++;
						}
          }
        }
      }
      StThreeVectorF pos(mv0->getVertexX(),mv0->getVertexY(),mv0->getVertexZ());
      v->setPosition(pos);
      v0.push_back(v);
    }
  }

}
void StEmcMicroUtil::processMicroEventEMC()
{
  // filling EMC collection
  StEmcMicroCollection* MicroEmc=mMicroEvent->getEmc();
  if(!MicroEmc) return;
  
  StEmcCollection *emc=new StEmcCollection();
  for(Int_t i=0;i<4;i++)
  {
    Int_t det=i+1;
    
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector = new StEmcDetector(id, 120);
    emc->setDetector(detector);
    // hits
    for(Int_t j=0;j<MicroEmc->getNHits(det);j++)
    {
      StEmcMicroHit* hit=MicroEmc->getHit(det,j);
      Int_t m=hit->getModule();
      Int_t e=hit->getEta();
      Int_t s=hit->getSub();
      Int_t a=hit->getAdc();
      Float_t energy=hit->getEnergy();
      StEmcRawHit* rawHit=new StEmcRawHit(id,(UInt_t)m,(UInt_t)e,(UInt_t)s,(UInt_t)a,energy);
      //cout <<"Hit number "<<j<<"  m = "<<m<<"  e = "<<e<<"  s = "<<s<<"  adc = "<<a<<"  en = "<<energy<<"\n";
      detector->addHit(rawHit);
    }
    //clusters
    Int_t nc=MicroEmc->getNClusters(det);
    if(nc>0)
    {
      StEmcClusterCollection* clusters=new StEmcClusterCollection();
      for(Int_t j=0;j<nc;j++)
      {
        StEmcMicroCluster* cl=MicroEmc->getCluster(det,j);
        StEmcCluster* cluster=new StEmcCluster();
        Float_t eta=cl->getEta();
        Float_t seta=cl->getSigmaEta();
        Float_t phi=cl->getPhi();
        Float_t sphi=cl->getSigmaPhi();
        Float_t e=cl->getEnergy();
        cluster->setEta(eta);
        cluster->setPhi(phi);
        cluster->setSigmaEta(seta);
        cluster->setSigmaPhi(sphi);
        cluster->setEnergy(e);
        
        for(Int_t k=0;k<cl->getNHits();k++)
        {
          StEmcMicroHit *hit=cl->getHit(k);
          Int_t m=hit->getModule();
          Int_t e=hit->getEta();
          Int_t s=hit->getSub();
          StEmcModule *module = detector->module(m);
          StSPtrVecEmcRawHit& rawhits=module->hits();
          for(Int_t l=0;l<(Int_t)rawhits.size();l++)
            if(m==(Int_t)rawhits[l]->module() && e==(Int_t)rawhits[l]->eta() && s==(Int_t)abs(rawhits[l]->sub()))
              cluster->addHit(rawhits[l]);
        }
        
        clusters->addCluster(cluster);
      }
      detector->setCluster(clusters);
    }
  }
  // points  
  Float_t mag = mGeo[0]->Radius();
  for(Int_t i=0; i<MicroEmc->getNPoints();i++)
  {
    StEmcMicroPoint *point=MicroEmc->getPoint(i);
    Float_t eta=point->getEta();
    Float_t deta=point->getDeltaEta();
    Float_t phi=point->getPhi();
    Float_t dphi=point->getDeltaPhi();
    Float_t en=point->getEnergy();
    Float_t chi=point->getChiSquare();
    Float_t theta=2*atan(exp(-eta));
    Float_t x,y,z;
    x = mag*sin(theta)*cos(phi);
    y = mag*sin(theta)*sin(phi);
    z = mag*cos(theta);
    StThreeVectorF p(x,y,z);
    StEmcPoint *pt=new StEmcPoint();
    pt->setEnergy(en);
    pt->setChiSquare(chi);
    pt->setDeltaEta(deta);
    pt->setDeltaPhi(dphi);
    pt->setPosition(p);
    for(Int_t j=0;j<4;j++) // looking for clusters
    {
      Int_t det = j+1;
      if(point->getNClusters(det)>0) for(Int_t l=0;l<point->getNClusters(det);l++)
      {
        StEmcMicroCluster *cl=point->getCluster(det,l);
        Float_t eta=cl->getEta();
        Float_t phi=cl->getPhi();
        Float_t e=cl->getEnergy();
        StDetectorId id = static_cast<StDetectorId>(j+kBarrelEmcTowerId);
        StEmcDetector *detector=emc->detector(id);
        StSPtrVecEmcCluster& clusters=detector->cluster()->clusters();
        for(Int_t k=0;k<(Int_t)clusters.size();k++)
          if(eta==clusters[k]->eta() && phi==clusters[k]->phi() && e==clusters[k]->energy())
            pt->addCluster(id,clusters[k]);
      }
    }
    emc->addBarrelPoint(pt);
  }
  // set emc collection
  mStEvent->setEmcCollection(emc);
}
