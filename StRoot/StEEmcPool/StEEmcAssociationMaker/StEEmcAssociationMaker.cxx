/********************************************************************************
 *
 * Author: Marcia Maria de Moura
 *
 * See README file for description
 *
 ********************************************************************************/

#include "StEEmcAssociationMaker.h"

#include <Stiostream.h>
#include <math.h>

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"
#include "StEventTypes.h"
#include <StMessMgr.h>

const TString eemcDetname[] = {"etow","eprs","esmdu","esmdv"};

ClassImp(StEEmcAssociation)
ClassImp(StEEmcClusterAssociation)
ClassImp(StEEmcPointAssociation)
ClassImp(StEEmcAssociationMaker)
//------------------------------------------------------------------------------
StEEmcAssociation::StEEmcAssociation(StMcTrack *t) 
{ mTrack = t; }
StEEmcAssociation::~StEEmcAssociation() 
{ }
//------------------------------------------------------------------------------
StEEmcClusterAssociation::StEEmcClusterAssociation(StMcTrack* t, StEmcCluster* c, float ft,float fe):StEEmcAssociation(t) 
{ mCluster=c;  mFTrack = ft; mFEmc = fe;}
StEEmcClusterAssociation::~StEEmcClusterAssociation()
{ }
//------------------------------------------------------------------------------
StEEmcPointAssociation::StEEmcPointAssociation(StMcTrack* t, StEmcPoint* p, int at):StEEmcAssociation(t) 
{ mPoint=p; mAssocType = at; }
StEEmcPointAssociation::~StEEmcPointAssociation() 
{ }
//------------------------------------------------------------------------------
StEEmcAssociationMaker::StEEmcAssociationMaker(const char* name):StMaker(name)
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
StEEmcAssociationMaker::~StEEmcAssociationMaker()
{
}
void StEEmcAssociationMaker::Clear(const char* a)
{
  if(mPrint) cout <<"Cleaning old stuff from EEMC association\n";
  for(Int_t i=0;i<NDETECTORS;i++)
  {
    if(mPrint) cout <<"Cleaning for detector "<<i<<endl;
    if(mTrackCluster[i]) 
    { 
      for(multiEEmcTrackClusterIter j=mTrackCluster[i]->begin(); j!=mTrackCluster[i]->end(); j++) 
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
    for(multiEEmcTrackPointIter j=mTrackPoint->begin(); j!=mTrackPoint->end(); j++) 
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
Int_t StEEmcAssociationMaker::Init()
{
  return StMaker::Init();
}
//------------------------------------------------------------------------------
Int_t StEEmcAssociationMaker::Make()
{
  if(mPrint) gMessMgr->Info() << "StEEmcAssociationMaker::Make()" << endm;
  
  // Getting McEvent object
  StMcEvent* mcEvent= (StMcEvent*) GetDataSet("StMcEvent");
  if (!mcEvent) return kStWarn;
  StSPtrVecMcTrack& tracks=mcEvent->tracks();
  if(tracks.size()==0) return kStWarn;

  // Getting Event object
  StEvent* event=(StEvent*) GetDataSet("StEvent");
  if (!event) return kStWarn;
  
  if(Debug()==2) if(mPrint) printHits(event);

  StEmcCollection* emcCollection=event->emcCollection();
  if(!emcCollection) return kStWarn;

  // Starting doing cluster association for each detector
  for (Int_t detnum=0; detnum<NDETECTORS; detnum++) // For detnum<4, only endcap EMC
  {
    if(mPrint) cout <<"Doing association for detector "<<detnum<<endl;
    StDetectorId detId=static_cast<StDetectorId>(detnum+kEndcapEmcTowerId);
    StEmcDetector* detector=emcCollection->detector(detId);
    StEmcClusterCollection* clusterColl=NULL;
    if (detector) clusterColl=detector->cluster();
    if (clusterColl)
    { 
      StSPtrVecEmcCluster& clusters=clusterColl->clusters();

      // Allocate cluster matching maps
      if(mPrint) cout<<"Track size = "<<tracks.size()<<"  cluster size = "<<clusters.size()<<endl;
      if(!mTrackCluster[detnum]) mTrackCluster[detnum]=new multiEEmcTrackCluster;
      if(!mClusterTrack[detnum]) mClusterTrack[detnum]=new multiEEmcClusterTrack;
      
      for (UInt_t i=0; i<tracks.size(); i++)
      {
        StPtrVecMcCalorimeterHit hits;
        switch (detnum)
        {
          case 0: { hits=tracks[i]->eemcHits(); break; }
          case 1: { hits=tracks[i]->eprsHits(); break; }
          case 2: { hits=tracks[i]->esmduHits(); break; }
          case 3: { hits=tracks[i]->esmdvHits(); break; }
          default: { gMessMgr->Warning()<<"Detector does not exist"<<endm; break; }
        }

        if (hits.size()!=0);
        {
          // Calculating track energy by summing over hits energies
          Float_t energy=0;
          for (UInt_t i2=0; i2<hits.size(); i2++)
          {
            Float_t hitEnergy=hits[i2]->dE();
            energy+=hitEnergy;
          } 
          
          for (UInt_t j=0; j<clusters.size(); j++)
          {
            StPtrVecEmcRawHit& clHits=clusters[j]->hit();
	    Float_t trackEnergyFraction=0;
	    Float_t clusterEnergyFraction=0;
	    Int_t assoc=0;
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

                //compare between hit track and hit cluster
                if (module==clModule && eta==clEta  
                    && ((detnum > 1)? 1 : sub==clSub))  
                {
		  assoc=1;
                  if(Debug()) if(mPrint) { 
                    cout << eemcDetname[detnum] 
                       << " cluster = " << j  
                       << ", m = " <<module <<" " << clModule
                       << ", e = " << eta << " " << clEta
                       << ", s = " << sub << " " << clSub << endl; 
                  }
                  Float_t hitEnergy=hits[k]->dE();
                  trackEnergyFraction+=hitEnergy/energy;
                  Float_t clEnergy=clusters[j]->energy();
                  clusterEnergyFraction+=hitEnergy/clEnergy;
                }
              }
	    }
	    if (assoc) {
	      StEEmcClusterAssociation *c=new StEEmcClusterAssociation(tracks[i],clusters[j],trackEnergyFraction,clusterEnergyFraction);
	      mTrackCluster[detnum]->insert(multiEEmcTrackClusterValue(tracks[i],c));

	      mClusterTrack[detnum]->insert(multiEEmcClusterTrackValue(clusters[j],c));
	    }		  
          }
        }
      }
    }
  }
  // Finishing doing cluster association
  
  // Starting doing point association 
  if(mPrint) cout <<"Doing point association\n";
  StSPtrVecEmcPoint& endcapPoints=emcCollection->endcapPoints();
  cout <<"points.size() = " << endcapPoints.size() << endl;
  for (UInt_t j=0; j<endcapPoints.size(); j++) {
      int nCl[4];      
      for(Int_t detnum=0;detnum<NDETECTORS;detnum++){
        StDetectorId detId=static_cast<StDetectorId>(detnum+kEndcapEmcTowerId);
        StPtrVecEmcCluster& cluster=endcapPoints[j]->cluster(detId);
        nCl[detnum] = cluster.size();
      }
      if(nCl[0]!=0 && nCl[1]!=0 && nCl[2]!=1 && nCl[3]!=1 &&
         endcapPoints[j]->nTracks()!=0) endcapPoints[j]->print();
  }
  if(endcapPoints.size()!=0)
  {
    // Allocate point matching maps
    if(!mTrackPoint) mTrackPoint = new multiEEmcTrackPoint;
    if(!mPointTrack) mPointTrack = new multiEEmcPointTrack;
    
    for (UInt_t i=0; i<tracks.size(); i++)
    {
      StPtrVecMcCalorimeterHit hits;

      for (UInt_t j=0; j<endcapPoints.size(); j++)
      {
        Int_t assoc=0;
        for(Int_t detnum=0;detnum<NDETECTORS;detnum++)
        {
          switch (detnum)
          {
            case 0: { hits=tracks[i]->eemcHits(); break; }
            case 1: { hits=tracks[i]->eprsHits(); break; }
            case 2: { hits=tracks[i]->esmduHits(); break; }
            case 3: { hits=tracks[i]->esmdvHits(); break; }
            default: { gMessMgr->Warning()<<"Detector does not exist"<<endm; break; }
          }
          StDetectorId detId=static_cast<StDetectorId>(detnum+kEndcapEmcTowerId);
          StPtrVecEmcCluster& cluster=endcapPoints[j]->cluster(detId);
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

                // compare between hit track and hit cluster from a point
                if (module==clModule && eta==clEta 
                      && ((detnum > 1)? 1 : sub==clSub)) 
                { 
                    assoc+=(1<<(detnum)); 
                    if(Debug()) if(mPrint) {
                      cout <<"assoc = " << assoc << " " 
                       << eemcDetname[detnum] 
                       << ", m = " <<module <<" " << clModule
                       << ", e = " << eta << " " << clEta
                       << ", s = " << sub << " " << clSub << endl; 
                    }
                    goto nextDetector;
                }
              }
            }
          }
          nextDetector: continue;
        } 
	if (assoc) {
	  StEEmcPointAssociation *p= new StEEmcPointAssociation(tracks[i],endcapPoints[j],assoc);
	  mTrackPoint->insert(multiEEmcTrackPointValue(tracks[i],p));
	  mPointTrack->insert(multiEEmcPointTrackValue(endcapPoints[j],p));
	}
      }
    }   
  }  
  if(Debug()) if(mPrint) printMaps();
  // Finishing doing point association 
  return kStOK;
}
//------------------------------------------------------------------------------
Int_t StEEmcAssociationMaker::Finish()
{
  // Finishing the maker
  gMessMgr->Info() << "StEEmcAssociationMaker::Finish()" << endm;
  return kStOK;
}
//---------------------------------------------------------------------
multiEEmcTrackCluster* StEEmcAssociationMaker::getTrackClusterMap(const char* detname)
{
  Int_t det = getDetNum(detname);
  if(det>0) return mTrackCluster[det-1];
  return NULL;
}     
//---------------------------------------------------------------------
multiEEmcClusterTrack* StEEmcAssociationMaker::getClusterTrackMap(const char* detname)
{
  Int_t det = getDetNum(detname);
  if(det>0) return mClusterTrack[det-1];
  return NULL;
}     
//---------------------------------------------------------------------
Int_t StEEmcAssociationMaker::getDetNum(const char* detname)
{
  Int_t detnum = 0;
  for (Int_t i=0; i<NDETECTORS; i++)  
    if (!strcmp(detname,eemcDetname[i])) detnum = i+1;
  return detnum;
}     
//---------------------------------------------------------------------
void StEEmcAssociationMaker::printMaps()
{
  for(int i=0;i<NDETECTORS;i++)
  {
    cout <<"-----------------------------------------------------------\n";
    cout <<"ASSOCIATION FOR DETECTOR "<<eemcDetname[i].Data()<<endl;
    multiEEmcTrackCluster* map = getTrackClusterMap(i+1);
    if(map) 
    { 
      cout <<"Track->Cluster Association Map\n";
      for(multiEEmcTrackClusterIter j=map->begin(); j!=map->end(); j++) 
      {
        StMcTrack* track = (StMcTrack*)(*j).first;
        StEEmcClusterAssociation* value = (StEEmcClusterAssociation*)(*j).second;
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
    
    multiEEmcClusterTrack* map1 = getClusterTrackMap(i+1);
    if(map1) 
    { 
      cout <<"Cluster->Track Association Map\n";
      for(multiEEmcClusterTrackIter j=map1->begin(); j!=map1->end(); j++) 
      {
        StEmcCluster *c = (StEmcCluster*)(*j).first;
        StEEmcClusterAssociation* value = (StEEmcClusterAssociation*)(*j).second;
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
  multiEEmcTrackPoint *mapPoint = getTrackPointMap();
  if(mapPoint)
  {
    cout <<"Track->Point Association Map\n";
    for(multiEEmcTrackPointIter j = mapPoint->begin(); j!=mapPoint->end(); j++)
    {
      StMcTrack *track = (StMcTrack*)(*j).first;
      StEEmcPointAssociation *value = (StEEmcPointAssociation*)(*j).second;
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
  multiEEmcPointTrack *mapPoint1 = getPointTrackMap();
  if(mapPoint1)
  {
    cout <<"Point->Track Association Map\n";
    for(multiEEmcPointTrackIter j = mapPoint1->begin(); j!=mapPoint1->end(); j++)
    {
      StEmcPoint *point = (StEmcPoint*)(*j).first;
      StEEmcPointAssociation *value = (StEEmcPointAssociation*)(*j).second;
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
//-------------------------------------------------------------------
void StEEmcAssociationMaker::printHits(StEvent *event)
{
  StEmcCollection* emccol=(StEmcCollection*)event->emcCollection();

    gMessMgr->Info()<<"****** EEMC Association printHits() *******" <<endm;
  for(Int_t i=0; i<NDETECTORS; i++)
  {
    StDetectorId id = static_cast<StDetectorId>(i+kEndcapEmcTowerId);
    StEmcDetector* detector=emccol->detector(id);
    gMessMgr->Info()<<"****************** hits in detector "
                                 << eemcDetname[i].Data()<<endm;
    if(detector) for(int j=1;j<=kEEmcNumSectors;j++)
    {
      StEmcModule* sector = detector->module(j);
      StSPtrVecEmcRawHit& rawHit=sector->hits();
      if(rawHit.size()>0)
          gMessMgr->Info()<<"Number of hits for module "
                                      <<j<<" = "<<rawHit.size()<<endm;
      for(UInt_t k=0;k<rawHit.size();k++)
        if(i < 2) // for Tower and Prs
          gMessMgr->Info()<<"Hit number = "<<k
                     <<"  sector = " << rawHit[k]->module()
                     <<"  eta = "<<rawHit[k]->eta()
                     <<"  sub = "<< rawHit[k]->sub()
                     <<"  adc = "<< rawHit[k]->adc()
                     <<"  energy = "<<rawHit[k]->energy()<<endm;
        else
          gMessMgr->Info()<<"Hit number = "<<k
                     <<"  sector = " << rawHit[k]->module()
                     <<"  strip = "<<rawHit[k]->eta()
                     <<"  adc = "<< rawHit[k]->adc()
                     <<"  energy = "<<rawHit[k]->energy()<<endm;
    }
  }
}

///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcAssociationMaker.cxx,v 1.2 2005/09/29 14:58:10 fisyak Exp $
// $Log: StEEmcAssociationMaker.cxx,v $
// Revision 1.2  2005/09/29 14:58:10  fisyak
// Persistent StMcEvent
//
// Revision 1.1.1.1  2005/05/31 18:54:13  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////
