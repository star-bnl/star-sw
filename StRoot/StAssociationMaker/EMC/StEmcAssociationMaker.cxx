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
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"
#include "StMcEventMaker/StMcEventMaker.h"
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
StEmcAssociationMaker::StEmcAssociationMaker()
{
  for(Int_t i=0;i<NDETECTORS;i++)
  {
    mTrackCluster[i] = NULL;
    mClusterTrack[i] = NULL;
  }
  mTrackPoint = NULL;
  mPointTrack = NULL;
  
  for (Int_t detnum=0; detnum<NDETECTORS; detnum++) // For detnum<4, only barrel EMC
  {
    mAssocMatrix[detnum].ResizeTo(1,1);
    mAssocMatrix[detnum].Zero();
    mTrackHitEnergyRtMatrix[detnum].ResizeTo(1,1);
    mTrackHitEnergyRtMatrix[detnum].Zero();
    mClHitEnergyRtMatrix[detnum].ResizeTo(1,1);
    mClHitEnergyRtMatrix[detnum].Zero();
  }
  mAssocPointMatrix.ResizeTo(1,1);
  mAssocPointMatrix.Zero();
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
      for(multiEmcClusterTrackIter j=mClusterTrack[i]->begin(); j!=mClusterTrack[i]->end(); j++)
      { 
        SafeDelete((*j).second);
      }
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
    for(multiEmcPointTrackIter j=mPointTrack->begin(); j!=mPointTrack->end(); j++) 
    {
      SafeDelete((*j).second);
    }
    mPointTrack->clear(); 
    SafeDelete(mPointTrack); 
    mPointTrack=NULL;
  }
  for (Int_t detnum=0; detnum<NDETECTORS; detnum++) // For detnum<4, only barrel EMC
  {
    mAssocMatrix[detnum].Zero();
    mTrackHitEnergyRtMatrix[detnum].Zero();
    mClHitEnergyRtMatrix[detnum].Zero();
  }
  mAssocPointMatrix.Zero();
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
  gMessMgr->Info() << "StEmcAssociationMaker::Make()" << endm;
  
  // Getting McEvent object
  if(!GetMaker("StMcEvent")) return kStWarn;
  StMcEvent* mcEvent=((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
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
    StEmcClusterCollection* clusterColl;
    if (detector) clusterColl=detector->cluster();
    if (clusterColl)
    { 
      StSPtrVecEmcCluster& clusters=clusterColl->clusters();

      // Dimensioning Association Matrices
      if(mPrint) cout<<"Track size = "<<tracks.size()<<"  cluster size = "<<clusters.size()<<endl;
      mAssocMatrix[detnum].ResizeTo(tracks.size(),clusters.size());
      mAssocMatrix[detnum].Zero();
      mTrackHitEnergyRtMatrix[detnum].ResizeTo(tracks.size(),clusters.size());
      mTrackHitEnergyRtMatrix[detnum].Zero();
      mClHitEnergyRtMatrix[detnum].ResizeTo(tracks.size(),clusters.size());
      mClHitEnergyRtMatrix[detnum].Zero();
      
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

        if (hits.size()!=0);
        {
          // Calculating track energy by summing over hits energies
          Float_t energy=0;
          for (UInt_t i2=0; i2<hits.size(); i2++)
          {
            Float_t hitEnergy=dEToEnergy(hits[i2],detnum);
            energy+=hitEnergy;
          } 
          
          for (UInt_t j=0; j<clusters.size(); j++)
          {
            StPtrVecEmcRawHit& clHits=clusters[j]->hit();
            for (UInt_t k=0; k<hits.size(); k++)
            {
              UInt_t module=hits[k]->module();
              UInt_t eta=hits[k]->eta();
              Int_t sub=hits[k]->sub();
              
              for (UInt_t l=0; l<clHits.size(); l++)
              {
                UInt_t clModule=clHits[l]->module();
                UInt_t clEta=clHits[l]->eta();
                Int_t clSub=abs(clHits[l]->sub());
                //Doing comparision between hit track and hit cluster
                if (module==clModule && eta==clEta && sub==clSub)
                {
                  mAssocMatrix[detnum](i,j)=1;
                  Float_t hitEnergy=dEToEnergy(hits[k],detnum);
                  mTrackHitEnergyRtMatrix[detnum](i,j)+=hitEnergy/energy;
                  Float_t clEnergy=clusters[j]->energy();
                  mClHitEnergyRtMatrix[detnum](i,j)+=hitEnergy/clEnergy;
                }
              }
            }
          }
        }
      }
      // Starting normalization of Cluster Energy Fraction Matrix elements
      for (UInt_t j2=0; j2<clusters.size(); j2++)
      {
        Float_t norm=0;
        for (UInt_t i3=0; i3<tracks.size(); i3++) norm+=mClHitEnergyRtMatrix[detnum](i3,j2);
        for (UInt_t i3=0; i3<tracks.size(); i3++) mClHitEnergyRtMatrix[detnum](i3,j2)=mClHitEnergyRtMatrix[detnum](i3,j2)/norm;
      }
      // Finishing normalization
    }
  }
  // Finishing doing cluster association
  
  // Starting doing point association 
  StSPtrVecEmcPoint& barrelPoints=emcCollection->barrelPoints();
  mAssocPointMatrix.Zero();
  if(mPrint) cout <<"Doing point association\n";
  if(barrelPoints.size()!=0)
  {
    // Dimensioning Association Matrix
    mAssocPointMatrix.ResizeTo(tracks.size(),barrelPoints.size());
    mAssocPointMatrix.Zero();
    
    for (UInt_t i=0; i<tracks.size(); i++)
    {
      StPtrVecMcCalorimeterHit hits;

      for (UInt_t j=0; j<barrelPoints.size(); j++)
      {
        Float_t assoc=0;
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
                if (module==clModule && eta==clEta && sub==clSub) assoc+=(1<<(detnum));
              }
            }
          }
          mAssocPointMatrix(i,j)=assoc;
        } 
      }
    }   
  }  
  fillMaps();
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
TMatrix StEmcAssociationMaker::getMatrix(const char* detname, const char* matrixtype)
{
  Int_t det = getDetNum(detname);
  if(det>0)
  {
    if(!strcmp(matrixtype,"association")) return mAssocMatrix[det-1];
    if(!strcmp(matrixtype,"trackFraction")) return mTrackHitEnergyRtMatrix[det-1];
    if(!strcmp(matrixtype,"clusterFraction")) return mClHitEnergyRtMatrix[det-1];
  }
  return mAssocMatrix[0];
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
void StEmcAssociationMaker::fillMaps()
{
  StMcEvent* mcEvent=((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
  StSPtrVecMcTrack& tracks=mcEvent->tracks();
  // Getting Event object
  StEvent* event=(StEvent*)GetDataSet("StEvent");
  StEmcCollection* emcCollection=event->emcCollection();
  for(Int_t detnum=0;detnum<NDETECTORS;detnum++)
  {
    StDetectorId detId=static_cast<StDetectorId>(detnum+kBarrelEmcTowerId);
    StEmcDetector* detector=emcCollection->detector(detId);
    StEmcClusterCollection* clusterColl=NULL;
    if (detector) clusterColl=detector->cluster();
    if (clusterColl)
    { 
      StSPtrVecEmcCluster& clusters=clusterColl->clusters();
      for(UInt_t i=0;i<tracks.size();i++)
        for(UInt_t j=0;j<clusters.size();j++)
        {
          if(mAssocMatrix[detnum](i,j)==1) // association made
          {
            StEmcClusterAssociation *c=new StEmcClusterAssociation(tracks[i],clusters[j],
                                           mTrackHitEnergyRtMatrix[detnum](i,j),mClHitEnergyRtMatrix[detnum](i,j));
            StEmcClusterAssociation *c1=new StEmcClusterAssociation(tracks[i],clusters[j],
                                           mTrackHitEnergyRtMatrix[detnum](i,j),mClHitEnergyRtMatrix[detnum](i,j));
            if(!mTrackCluster[detnum]) mTrackCluster[detnum]=new multiEmcTrackCluster;
            if(!mClusterTrack[detnum]) mClusterTrack[detnum]=new multiEmcClusterTrack;
            mTrackCluster[detnum]->insert(multiEmcTrackClusterValue(tracks[i],c));
            mClusterTrack[detnum]->insert(multiEmcClusterTrackValue(clusters[j],c1));
          }
        }
    }
  }
  // points multimaps
  StSPtrVecEmcPoint& points=emcCollection->barrelPoints();
  for(UInt_t i=0;i<tracks.size();i++)
    for(UInt_t j=0;j<points.size();j++)
    {
      if(mAssocPointMatrix(i,j)>0)
      {
        StEmcPointAssociation *p= new StEmcPointAssociation(tracks[i],points[j],(int)mAssocPointMatrix(i,j));
        StEmcPointAssociation *p1= new StEmcPointAssociation(tracks[i],points[j],(int)mAssocPointMatrix(i,j));
        if(!mTrackPoint) mTrackPoint = new multiEmcTrackPoint;
        if(!mPointTrack) mPointTrack = new multiEmcPointTrack;
        mTrackPoint->insert(multiEmcTrackPointValue(tracks[i],p));
        mPointTrack->insert(multiEmcPointTrackValue(points[j],p1));
      }
    }
  return;
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
          StEmcCluster *c = value->getCluster();
          
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
          StMcTrack* track = value->getTrack();
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
        StEmcPoint *point = value->getPoint(); 
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
        StMcTrack *track = value->getTrack();
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
