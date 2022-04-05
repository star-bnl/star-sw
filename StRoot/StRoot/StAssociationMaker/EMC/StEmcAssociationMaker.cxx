/********************************************************************************
 *
 * Author: Marcia Maria de Moura
 *
 * See README file for description
 *
 ********************************************************************************/

#include "StEmcAssociationMaker.h"

#include <Stiostream.h>
#include <math.h>

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"
#include "StEventTypes.h"
#include <StMessMgr.h>

ClassImp(StEmcAssociation)
ClassImp(StEmcClusterAssociation)
ClassImp(StEmcPointAssociation)
ClassImp(StEmcAssociationMaker)
//------------------------------------------------------------------------------
StEmcAssociation::StEmcAssociation(StMcTrack *t) 
{ mTrack = t; }
StEmcAssociation::~StEmcAssociation() 
{ }
//------------------------------------------------------------------------------
StEmcClusterAssociation::StEmcClusterAssociation(StMcTrack* t, StEmcCluster* c, float ft,float fe):StEmcAssociation(t) 
{ mCluster=c;  mFTrack = ft; mFEmc = fe;}
StEmcClusterAssociation::~StEmcClusterAssociation()
{ }
//------------------------------------------------------------------------------
StEmcPointAssociation::StEmcPointAssociation(StMcTrack* t, StEmcPoint* p, int at):StEmcAssociation(t) 
{ mPoint=p; mAssocType = at; }
StEmcPointAssociation::~StEmcPointAssociation() 
{ }
//------------------------------------------------------------------------------
StEmcAssociationMaker::StEmcAssociationMaker(const char* name):StMaker(name)
{
  for(Int_t i=0;i<NDETECTORS;i++)
  {
    mTrackCluster[i] = NULL;
    mClusterTrack[i] = NULL;
  }
  mTrackPoint = NULL;
  mPointTrack = NULL;
  
  mPrint = kTRUE;
}
//------------------------------------------------------------------------------
StEmcAssociationMaker::~StEmcAssociationMaker()
{
}
void StEmcAssociationMaker::Clear(const char* a)
{
  if(mPrint) cout <<"Cleaning old stuff from EMC association\n";
  for(Int_t i=0;i<NDETECTORS;i++)
  {
    if(mPrint) cout <<"Cleaning for detector "<<i<<endl;
    if(mTrackCluster[i]) 
    { 
      for(multiEmcTrackClusterIter j=mTrackCluster[i]->begin(); j!=mTrackCluster[i]->end(); j++) 
      {
        SafeDelete((*j).second);
      }
      mTrackCluster[i]->clear(); 
      SafeDelete(mTrackCluster[i]); 
      mTrackCluster[i]=NULL; 
    }
    if(mClusterTrack[i]) 
    { 
      mClusterTrack[i]->clear(); 
      SafeDelete(mClusterTrack[i]); 
      mClusterTrack[i]=NULL;
    }
  }
  if (mPrint) cout <<"Cleaning points Association\n";
  if(mTrackPoint) 
  { 
    for(multiEmcTrackPointIter j=mTrackPoint->begin(); j!=mTrackPoint->end(); j++) 
    {
      SafeDelete((*j).second);
    }
    mTrackPoint->clear(); 
    SafeDelete(mTrackPoint); 
    mTrackPoint=NULL;
  }
  if(mPointTrack) 
  { 
    mPointTrack->clear(); 
    SafeDelete(mPointTrack); 
    mPointTrack=NULL;
  }
  return;
}
//------------------------------------------------------------------------------
Int_t StEmcAssociationMaker::Init()
{
  return StMaker::Init();
}
//------------------------------------------------------------------------------
Int_t StEmcAssociationMaker::Make()
{
  if(mPrint) gMessMgr->Info() << "StEmcAssociationMaker::Make()" << endm;
  
  // Getting McEvent object
  StMcEvent* mcEvent= (StMcEvent*) GetDataSet("StMcEvent");
  if (!mcEvent) return kStWarn;
  StSPtrVecMcTrack& tracks=mcEvent->tracks();
  if(tracks.size()==0) return kStWarn;

  // Getting Event object
  StEvent* event=(StEvent*) GetDataSet("StEvent");
  if (!event) return kStWarn;
  
  StEmcCollection* emcCollection=event->emcCollection();
  if(!emcCollection) return kStWarn;

  // Starting doing cluster association for each detector
  for (Int_t detnum=0; detnum<NDETECTORS; detnum++) // For detnum<4, only barrel EMC
  {
    if(mPrint) cout <<"Doing association for detector "<<detnum<<endl;
    StDetectorId detId=static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
    StEmcDetector* detector=emcCollection->detector(detId);
    StEmcClusterCollection* clusterColl=NULL;
    if (detector) clusterColl=detector->cluster();
    if (clusterColl)
    { 
      StSPtrVecEmcCluster& clusters=clusterColl->clusters();

      // Allocate cluster matching maps
      if(mPrint) cout<<"Track size = "<<tracks.size()<<"  cluster size = "<<clusters.size()<<endl;
      if(!mTrackCluster[detnum]) mTrackCluster[detnum]=new multiEmcTrackCluster;
      if(!mClusterTrack[detnum]) mClusterTrack[detnum]=new multiEmcClusterTrack;
      
      for (UInt_t i=0; i<tracks.size(); i++)
      {
        StPtrVecMcCalorimeterHit hits;
        switch (detnum)
        {
          case 0: { hits=tracks[i]->bemcHits(); break; }
          case 1: { hits=tracks[i]->bprsHits(); break; }
          case 2: { hits=tracks[i]->bsmdeHits(); break; }
          case 3: { hits=tracks[i]->bsmdpHits(); break; }
          default: { gMessMgr->Warning()<<"Detector does not exist"<<endm; break; }
        }

        if (hits.size()!=0){
          // Calculating track energy by summing over hits energies
          Float_t energy=0;
          for (UInt_t i2=0; i2<hits.size(); i2++)
	    {
	      Float_t hitEnergy=dEToEnergy(hits[i2],detnum);
	      energy+=hitEnergy;
	    } 
          
          for (UInt_t j=0; j<clusters.size(); j++){
	    StPtrVecEmcRawHit& clHits=clusters[j]->hit();
	    Float_t trackEnergyFraction=0;
	    Float_t clusterEnergyFraction=0;
	    Int_t assoc=0;
	    for (UInt_t k=0; k<hits.size(); k++){
	      UInt_t module=hits[k]->module();
	      UInt_t eta=hits[k]->eta();
	      Int_t sub=hits[k]->sub();
	      
	      for (UInt_t l=0; l<clHits.size(); l++){
		UInt_t clModule=clHits[l]->module();
		UInt_t clEta=clHits[l]->eta();
		Int_t clSub=abs(clHits[l]->sub());
		//Doing comparision between hit track and hit cluster
		if (module==clModule && eta==clEta && sub==clSub){
		  assoc=1;
		  Float_t hitEnergy=dEToEnergy(hits[k],detnum);
		  trackEnergyFraction+=hitEnergy/energy;
		  Float_t clEnergy=clusters[j]->energy();
		  clusterEnergyFraction+=hitEnergy/clEnergy;
		}
	      }
	    }
	    if (assoc) {
	      StEmcClusterAssociation *c=new StEmcClusterAssociation(tracks[i],clusters[j],trackEnergyFraction,clusterEnergyFraction);
	      mTrackCluster[detnum]->insert(multiEmcTrackClusterValue(tracks[i],c));
	      
	      mClusterTrack[detnum]->insert(multiEmcClusterTrackValue(clusters[j],c));
	    }		  
	  }
        }
      }
    }
  }
  // Finishing doing cluster association
  
  // Starting doing point association 
  if(mPrint) cout <<"Doing point association\n";
  StSPtrVecEmcPoint& barrelPoints=emcCollection->barrelPoints();
  if(barrelPoints.size()!=0)
  {
    // Allocate point matching maps
    if(!mTrackPoint) mTrackPoint = new multiEmcTrackPoint;
    if(!mPointTrack) mPointTrack = new multiEmcPointTrack;
    
    for (UInt_t i=0; i<tracks.size(); i++)
    {
      StPtrVecMcCalorimeterHit hits;

      for (UInt_t j=0; j<barrelPoints.size(); j++)
      {
        Int_t assoc=0;
        for(Int_t detnum=0;detnum<NDETECTORS;detnum++)
        {
          switch (detnum)
          {
            case 0: { hits=tracks[i]->bemcHits(); break; }
            case 1: { hits=tracks[i]->bprsHits(); break; }
            case 2: { hits=tracks[i]->bsmdeHits(); break; }
            case 3: { hits=tracks[i]->bsmdpHits(); break; }
            default: { gMessMgr->Warning()<<"Detector does not exist"<<endm; break; }
          }
          StDetectorId detId=static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
          StPtrVecEmcCluster& cluster=barrelPoints[j]->cluster(detId);
          for (UInt_t k=0; k<hits.size(); k++) if(hits[k])
          {
            UInt_t module=hits[k]->module();
            UInt_t eta=hits[k]->eta();
            Int_t sub=hits[k]->sub();

            for (UInt_t j2=0; j2<cluster.size(); j2++) if(cluster[j2])
            {
              StPtrVecEmcRawHit& clHit=cluster[j2]->hit();
              for (UInt_t l=0; l<clHit.size(); l++) if(clHit[l])
              {
                UInt_t clModule=clHit[l]->module();
                UInt_t clEta=clHit[l]->eta();
                Int_t clSub=abs(clHit[l]->sub());

                //Doing comparision between hit track and hit cluster from a point
                if (module==clModule && eta==clEta && sub==clSub) { assoc+=(1<<(detnum)); goto nextDetector;}
              }
            }
          }
          nextDetector: continue;
        } 
	if (assoc) {
	  StEmcPointAssociation *p= new StEmcPointAssociation(tracks[i],barrelPoints[j],assoc);
	  mTrackPoint->insert(multiEmcTrackPointValue(tracks[i],p));
	  mPointTrack->insert(multiEmcPointTrackValue(barrelPoints[j],p));
	}
      }
    }   
  }  
  // Finishing doing point association 
  return kStOK;
}
//------------------------------------------------------------------------------
Int_t StEmcAssociationMaker::Finish()
{
  // Finishing the maker
  gMessMgr->Info() << "StEmcAssociationMaker::Finish()" << endm;
  return kStOK;
}
//------------------------------------------------------------------------------
Float_t StEmcAssociationMaker::dEToEnergy(StMcCalorimeterHit* hit, Int_t detnum)
{
  // Get hit deposited energy and convert to energy for each EMC detector. 
  
  Float_t P0[]={14.69,559.7,0.1185e6,0.1260e6};
  Float_t P1[]={-0.1022,-109.9,-0.3292e5,-0.1395e5};
  Float_t P2[]={0.7484,-97.81,0.3113e5,0.1971e5};

  UInt_t m=hit->module();
  UInt_t e=hit->eta();
  Float_t dE=hit->dE();
  Float_t hitEnergy = 0;

  StEmcGeom* emcGeom=StEmcGeom::instance(detnum+1);
  Float_t Eta;
  emcGeom->getEta(m,e,Eta);
  Float_t x=fabs(Eta);
  Float_t sf=P0[detnum]+P1[detnum]*x+P2[detnum]*x*x;
  hitEnergy=dE*sf;
  if (hitEnergy<=0) hitEnergy=0;
  return hitEnergy;
}
//---------------------------------------------------------------------
multiEmcTrackCluster* StEmcAssociationMaker::getTrackClusterMap(const char* detname)
{
  Int_t det = getDetNum(detname);
  if(det>0) return mTrackCluster[det-1];
  return NULL;
}     
//---------------------------------------------------------------------
multiEmcClusterTrack* StEmcAssociationMaker::getClusterTrackMap(const char* detname)
{
  Int_t det = getDetNum(detname);
  if(det>0) return mClusterTrack[det-1];
  return NULL;
}     
//---------------------------------------------------------------------
Int_t StEmcAssociationMaker::getDetNum(const char* detname)
{
  const TString det[] = {"bemc","bprs","bsmde","bsmdp"};
  Int_t detnum = 0;
  for (Int_t i=0; i<NDETECTORS; i++)  
    if (!strcmp(detname,det[i])) detnum = i+1;
  return detnum;
}     
//---------------------------------------------------------------------
void StEmcAssociationMaker::printMaps()
{
  const TString det[] = {"bemc","bprs","bsmde","bsmdp"};
  for(int i=0;i<NDETECTORS;i++)
  {
    cout <<"-----------------------------------------------------------\n";
    cout <<"ASSOCIATION FOR DETECTOR "<<det[i].Data()<<endl;
    multiEmcTrackCluster* map = getTrackClusterMap(i+1);
    if(map) 
    { 
      cout <<"Track->Cluster Association Map\n";
      for(multiEmcTrackClusterIter j=map->begin(); j!=map->end(); j++) 
      {
        StMcTrack* track = (StMcTrack*)(*j).first;
        StEmcClusterAssociation* value = (StEmcClusterAssociation*)(*j).second;
        if(track && value)
        {
          StEmcCluster *c = (StEmcCluster*) value->getCluster();
          
          if(c) 
          {
            cout <<" McTrack = "<<track<<" GeantId = "<<track->geantId()<<" pt = "<<track->pt()<<" TReta = "<<track->pseudoRapidity()
                 <<" Cl = "<<c<<" E = "<<c->energy()<<" eta = "<<c->eta()<<" phi = "<<c->phi()
                 <<" FrTr = "<<value->getFractionTrack()<<" FrCl = "<<value->getFractionCluster()<<endl;
          }
        }
      }      
    }
    cout <<endl;
    multiEmcClusterTrack* map1 = getClusterTrackMap(i+1);
    if(map1) 
    { 
      cout <<"Cluster->Track Association Map\n";
      for(multiEmcClusterTrackIter j=map1->begin(); j!=map1->end(); j++) 
      {
        StEmcCluster *c = (StEmcCluster*)(*j).first;
        StEmcClusterAssociation* value = (StEmcClusterAssociation*)(*j).second;
        if(c && value)
        {               
          StMcTrack* track = (StMcTrack*)value->getTrack();
          if(track) 
          {
            cout <<" Cl = "<<c<<" E = "<<c->energy()<<" eta = "<<c->eta()<<" phi = "<<c->phi()
                 <<" McTrack = "<<track<<" GeantId = "<<track->geantId()<<" pt = "<<track->pt()<<" TReta = "<<track->pseudoRapidity()
                 <<" FrTr = "<<value->getFractionTrack()<<" FrCl = "<<value->getFractionCluster()<<endl;
          }
        }
      }      
    }
  }
  multiEmcTrackPoint *mapPoint = getTrackPointMap();
  if(mapPoint)
  {
    cout <<"Track->Point Association Map\n";
    for(multiEmcTrackPointIter j = mapPoint->begin(); j!=mapPoint->end(); j++)
    {
      StMcTrack *track = (StMcTrack*)(*j).first;
      StEmcPointAssociation *value = (StEmcPointAssociation*)(*j).second;
      if(value)
      {
        StEmcPoint *point = (StEmcPoint*)value->getPoint(); 
        if(track && point)
        {
          cout <<" McTrack = "<<track<<" GeantId = "<<track->geantId()<<" pt = "<<track->pt()<<" TReta = "<<track->pseudoRapidity()
               <<" Point = "<<point<<" E = "<<point->energy()
               <<" Assoc. = "<<value->getAssociation();
          for(int i=1;i<=4;i++) cout <<" det "<<i<<" A = "<< value->getAssociation(i);
          cout <<endl;
        }
      }
    }
  }
  multiEmcPointTrack *mapPoint1 = getPointTrackMap();
  if(mapPoint1)
  {
    cout <<"Point->Track Association Map\n";
    for(multiEmcPointTrackIter j = mapPoint1->begin(); j!=mapPoint1->end(); j++)
    {
      StEmcPoint *point = (StEmcPoint*)(*j).first;
      StEmcPointAssociation *value = (StEmcPointAssociation*)(*j).second;
      if(value)
      {
        StMcTrack *track = (StMcTrack*)value->getTrack();
        if(track && point)
        {
          cout <<" McTrack = "<<track<<" GeantId = "<<track->geantId()<<" pt = "<<track->pt()<<" TReta = "<<track->pseudoRapidity()
               <<" Point = "<<point<<" E = "<<point->energy()
               <<" Assoc. = "<<value->getAssociation();
          for(int i=1;i<=4;i++) cout <<" det "<<i<<" A = "<< value->getAssociation(i);
          cout <<endl;
        }
      }
    }
  }
}
void StEmcAssociationMaker::printTracks()
{
  StMcEvent* mcEvent=(StMcEvent*) GetDataSet("StMcEvent");
  if (!mcEvent) return;
  StSPtrVecMcTrack& tracks=mcEvent->tracks();
  if(tracks.size()==0) return;

  StMcVertex *v = mcEvent->primaryVertex();
  if(v)
  {
    cout<<"Primary Vertex (x,y,z) = "<<v->position().x()<<"  "<<v->position().y()<<"  "<<v->position().z()<<"  "<<endl;
  }
  for(UInt_t i = 0; i<tracks.size();i++)
  {
    cout <<"Track " <<tracks[i]<<" Geant Id = "<<tracks[i]->geantId()<<"  pT = "<< tracks[i]->pt()
         <<"  eta = "<<tracks[i]->pseudoRapidity()
         <<"  phi = "<<tracks[i]->momentum().phi()<<endl;
    cout <<"     Parent = "<<tracks[i]->parent()<<endl;
    v = tracks[i]->startVertex();
    if(v) cout <<"     Start vertex (x,y,z) = "<<v->position().x()<<"  "<<v->position().y()<<"  "<<v->position().z()<<"  "<<endl;  
    v = tracks[i]->stopVertex();
    if(v) cout <<"     Stop vertex (x,y,z)  = "<<v->position().x()<<"  "<<v->position().y()<<"  "<<v->position().z()<<"  "<<endl;  
  }
}
