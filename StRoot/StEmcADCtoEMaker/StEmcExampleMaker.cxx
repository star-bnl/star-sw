//*-- Author : Alexandre Suaide and Marcia Moura
// 
// $Id: StEmcExampleMaker.cxx,v 1.2 2003/04/30 20:36:41 perev Exp $
// $Log: StEmcExampleMaker.cxx,v $
// Revision 1.2  2003/04/30 20:36:41  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.1  2003/02/14 16:30:54  suaide
// added example maker
//
// Revision 1.13  2000/05 16:07:01  
// Add README
//
#include "StEmcExampleMaker.h"
#include "StChain.h"
#include "StEventTypes.h"
#include "StEvent.h"
#include <iostream.h>
#include "StEmcUtil/geometry/StEmcGeom.h"

ClassImp(StEmcExampleMaker)

//_____________________________________________________________________________
StEmcExampleMaker::StEmcExampleMaker(const char *name):StMaker(name)
{
}
//_____________________________________________________________________________
StEmcExampleMaker::~StEmcExampleMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcExampleMaker::Init()
{
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StEmcExampleMaker::Make()
{
  cout <<"**********************************************************\n";
  cout <<"***** ENTERING EMC EXAMPLE MAKER \n";
  const TString detname[] = {"bemc", "bprs", "bsmde", "bsmdp","eemc", "eprs", "esmde", "esmdp"};
  
  StEvent *event = (StEvent*)GetInputDS("StEvent");
  if(!event)
  {
    cout << "***** Can not get Event pointer\n";
    return kStOK;
  }
  cout << "***** Event pointer Ok\n";
  
  StEmcCollection* emccol=(StEmcCollection*)event->emcCollection();

  cout<<"\n**********************************************************************\n";
  cout<<"STEVENT HITS\n\n ";
  
  for(Int_t i=0; i<4; i++)
  {  
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    cout<<"id = "<<id<<"\n";
    StEmcDetector* detector=emccol->detector(id);
    if(!detector) cout <<"detector not loaded\n";
    cout <<"****************** hits in detector "<< detname[i].Data()<<"\n";
    if(detector) for(UInt_t j=1;j<121;j++)
    {
      StEmcModule* module = detector->module(j);
      StSPtrVecEmcRawHit& rawHit=module->hits();
      if(rawHit.size()>0) cout <<"Module = "<<j<<" RawHit size = "<<rawHit.size()<<"\n";
      /*for(Int_t k=0;k<rawHit.size();k++)
      {
        cout <<"Hit number = "<<k<<"  module = " << rawHit[k]->module()<<"  eta = "<<rawHit[k]->eta() << "  sub = "<< rawHit[k]->sub() <<"  adc = "<<rawHit[k]->adc()<<"  energy = "<<rawHit[k]->energy()<<"\n";
      }*/
    }
  }
      
  cout<<"\n**********************************************************************\n";
  cout<<"STEVENT CLUSTERS\n\n ";
  for(Int_t i=0; i<4; i++)
  {  
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector=emccol->detector(id);
    if(detector) if(detector->cluster())
    {
      cout <<"****************** clusters in detector "<< detname[i].Data()<<"\n";
      cout <<"number of clusters = "<<detector->cluster()->numberOfClusters()<<"\n";
      StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
      for(UInt_t j=0;j<cluster.size();j++)
      {
        StPtrVecEmcRawHit& rawHit=cluster[j]->hit();
        cout <<"********* Cluster number "<<j<<"\n";
        cout <<"Eta = "<<cluster[j]->eta()<<" +- "<<cluster[j]->sigmaEta()<<"\n";
        cout <<"Phi = "<<cluster[j]->phi()<<" +- "<<cluster[j]->sigmaPhi()<<"\n";
        cout <<"Energy = "<<cluster[j]->energy()<<"\n";
        cout <<"********* Hits for this cluster\n";

        for(Int_t k=0;k<(int)rawHit.size();k++)
        {
          cout <<"Hit number = "<<k<<"  module = " << rawHit[k]->module()<<"  eta = "<<rawHit[k]->eta() << "  sub = "<< rawHit[k]->sub() <<"  adc = "<<rawHit[k]->adc()<<"  energy = "<<rawHit[k]->energy()<<"\n";
        }
        cout<<"---------------------------------------------------\n";
      }
    }
  }
            
  cout<<"\n**********************************************************************\n";
  cout<<"STEVENT POINTS\n\n ";
  StSPtrVecEmcPoint& points = emccol->barrelPoints();
  cout <<"barrel points size = "<<points.size()<<"\n";
  for(Int_t i=0;i<(int)points.size();i++)
  {
    cout <<"Point number "<<i<<endl;
    cout <<"Energy = "<<points[i]->energy()<<"  chi = "<<points[i]->chiSquare()<<endl;
    StThreeVectorF a =points[i]->position();
    cout <<"Eta = "<<a.pseudoRapidity()<<"  phi = "<<a.phi()<<"  R = "<<a.mag()<<"  dEta = "<<points[i]->deltaEta()<<"  dPhi = "<<points[i]->deltaPhi()<<endl;
    for(Int_t d=0; d<4; d++)
    {  
      StDetectorId id = static_cast<StDetectorId>(d+kBarrelEmcTowerId);
      cout <<"****************** clusters in detector "<< detname[d].Data()<<"\n";
      StPtrVecEmcCluster& cluster = points[i]->cluster(id);
      cout <<"number of clusters = "<<cluster.size()<<"\n";
      for(UInt_t j=0;j<cluster.size();j++) if(cluster[j])
      {
        StPtrVecEmcRawHit& rawHit=cluster[j]->hit();
        cout <<"********* Cluster number "<<j<<"\n";
        cout <<"Eta = "<<cluster[j]->eta()<<" +- "<<cluster[j]->sigmaEta()<<"\n";
        cout <<"Phi = "<<cluster[j]->phi()<<" +- "<<cluster[j]->sigmaPhi()<<"\n";
        cout <<"Energy = "<<cluster[j]->energy()<<"\n";
        cout <<"********* Hits for this cluster\n";

        for(Int_t k=0;k<(int)rawHit.size();k++)
        {
          cout <<"Hit number = "<<k<<"  module = " << rawHit[k]->module()<<"  eta = "<<rawHit[k]->eta() << "  sub = "<< rawHit[k]->sub() <<"  adc = "<<rawHit[k]->adc()<<"  energy = "<<rawHit[k]->energy()<<"\n";
        }

        cout<<"---------------------------------------------------\n";
      }      
    }
  }

  return kStOK;
}
//_____________________________________________________________________________
Int_t StEmcExampleMaker::Finish()
{  
  return StMaker::Finish();
}
