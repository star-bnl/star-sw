#include "StMuEmcUtil.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StMuEmcCollection.h"
#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"

ClassImp(StMuEmcUtil)

StMuEmcUtil::StMuEmcUtil()
{
  for(Int_t i =0;i<4;i++) mGeo[i]=StEmcGeom::getEmcGeom(detname[i].Data());
}
StMuEmcUtil::~StMuEmcUtil()
{
}
StMuEmcCollection* StMuEmcUtil::getMuEmc(StEmcCollection *emccol)
{
  if(!emccol) return NULL;
  StMuEmcCollection* muEmc=new StMuEmcCollection();
  fillMuEmc(muEmc,emccol);
  return muEmc;
}  
StEmcCollection* StMuEmcUtil::getEmc(StMuEmcCollection* muEmc)
{
  if(!muEmc) return NULL;
  
  StEmcCollection *emc=new StEmcCollection();
  fillEmc(emc,muEmc);
  return emc;
}
void StMuEmcUtil::fillMuEmc(StMuEmcCollection *muEmc,StEmcCollection *emccol)
{
  if(!emccol) return;
  if(!muEmc) return;
  Int_t HitsId[18000];
      
  // starting by hits;    
  for(Int_t d=0; d<4; d++)
  {  
    Int_t EmcDet=d+1;
    for(Int_t i=0;i<18000;i++) HitsId[i]=-1;
    
    StDetectorId id = static_cast<StDetectorId>(d+kBarrelEmcTowerId);
    StEmcDetector* detector=emccol->detector(id);
    if(detector)
    {                          
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
              Int_t cal = rawHit[k]->calibrationType();              
              Int_t rid;
              mGeo[d]->getId(m,e,s,rid);
              HitsId[rid-1] = HitIndex;
              HitIndex++;
              
              if(EmcDet == 1) // towers save only ADC
              {
                muEmc->setTowerADC(rid,adc);
              }              
              if(EmcDet==2) //pre shower
              {
                muEmc->addPrsHit();
                StMuEmcHit* muHit = muEmc->getPrsHit(muEmc->getNPrsHits()-1);          
                muHit->setId(rid);
                muHit->setAdc(adc);
                muHit->setEnergy(energy);
                muHit->setCalType(cal);      
              }
              if(EmcDet==3|| EmcDet==4)
              {
                muEmc->addSmdHit(EmcDet);
                StMuEmcHit* muHit = muEmc->getSmdHit(EmcDet,muEmc->getNSmdHits(EmcDet)-1);          
                muHit->setId(rid);
                muHit->setAdc(adc);
                muHit->setEnergy(energy);
                muHit->setCalType(cal);      
              }
            }
          }      
        } 
      
      // now clusters
      if(detector->cluster())
      {
        StSPtrVecEmcCluster& cluster=detector->cluster()->clusters();
        Int_t totalcluster=(Int_t)cluster.size();
        if(totalcluster>0)
          for(Int_t j=0;j<totalcluster;j++)
          {
            muEmc->addCluster(EmcDet);
            StMuEmcCluster *muCl=muEmc->getCluster(EmcDet,muEmc->getNClusters(EmcDet)-1);
          
            muCl->setEta(cluster[j]->eta());
            muCl->setPhi(cluster[j]->phi());
            muCl->setSigmaEta(cluster[j]->sigmaEta());
            muCl->setSigmaPhi(cluster[j]->sigmaPhi());
            muCl->setEnergy(cluster[j]->energy());
        
            StPtrVecEmcRawHit& rawHit=cluster[j]->hit();
            Int_t nhit=(Int_t)rawHit.size();
            muCl->setNHits(nhit);

            for(Int_t k=0;k<nhit;k++)
            {
              Int_t m = rawHit[k]->module();
              Int_t e = rawHit[k]->eta();
              Int_t s = abs(rawHit[k]->sub());
              Int_t rid;
              mGeo[d]->getId(m,e,s,rid);
              Int_t index = HitsId[rid-1];
              if(EmcDet==1) index=rid;
              if(index!=-1) muCl->setHitId(k,index);
            }
          }
      }  // if detector->cluster
    } // if detector
  } // loop detector
    
  StSPtrVecEmcPoint& points=emccol->barrelPoints();
  Int_t npoints=points.size();
  if(npoints>0)
  {
    for(Int_t p=0;p<npoints;p++)
    {
      StEmcPoint* point=points[p];
      StThreeVectorF position=point->position();
      muEmc->addPoint();
      StMuEmcPoint *muPt=muEmc->getPoint(muEmc->getNPoints()-1);
      muPt->setEta(position.pseudoRapidity());
      muPt->setPhi(position.phi());
      muPt->setRadius(sqrt(position.x()*position.x()+position.y()*position.y()));
      muPt->setDeltaEta(point->deltaEta());
      muPt->setDeltaPhi(point->deltaPhi());
      muPt->setEnergy(point->energy());
      muPt->setChiSquare(point->chiSquare());
      
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
          for(Int_t j=0;j<muEmc->getNClusters(det);j++)
          {
            StMuEmcCluster *cl=muEmc->getCluster(det,j);
            if(eta == cl->getEta() && phi==cl->getPhi())
            {
              muPt->setCluster(det,cl);
              goto cont;
            }
          }
          cont: continue;
        }
      } // loop detector
    } // loop points

  }// npoint >0
  
  return;

}
void StMuEmcUtil::fillEmc(StEmcCollection* emc,StMuEmcCollection* muEmc)
{
  if(!muEmc) return;
  if(!emc) return;
  
  for(Int_t i=0;i<4;i++)
  {
    Int_t det=i+1;
    
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    StEmcDetector* detector = new StEmcDetector(id, 120);
    emc->setDetector(detector);
    // hits
    Int_t nh=0;
    if (det==1) nh = 4800; 
    if (det==2) nh=muEmc->getNPrsHits();
    if (det==3||det==4) nh=muEmc->getNSmdHits(det);
    //cout <<"Number of hits for detector "<<det<<" = "<<nh<<endl;
    for(Int_t j=0;j<nh;j++)
    {
      Bool_t save = kTRUE;
      Int_t m,e,s,a,cal,rid;
      Float_t energy=0;
      if(det==1) // towers have only ADC
      {
        a = muEmc->getTowerADC(j+1);
        mGeo[det-1]->getBin(j+1,m,e,s);
        energy = 0;
        cal = 0;
        if(a==0) save = kFALSE;
      }      
      if(det==2) //prs
      {
        StMuEmcHit* hit=muEmc->getPrsHit(j);
        rid=hit->getId();
        mGeo[det-1]->getBin(rid,m,e,s);
        a=hit->getAdc();
        cal=hit->getCalType();
        energy=hit->getEnergy();
      }

      if(det==3||det==4) //smd
      {
        StMuEmcHit* hit=muEmc->getSmdHit(det,j);
        rid=hit->getId();
        mGeo[det-1]->getBin(rid,m,e,s);
        a=hit->getAdc();
        cal=hit->getCalType();
        energy=hit->getEnergy();
      }
      if(save)
      {
        StEmcRawHit* rawHit=new StEmcRawHit(id,(UInt_t)m,(UInt_t)e,(UInt_t)s,(UInt_t)a,energy);
        rawHit->setCalibrationType(cal);
        //cout <<"det = "<<det<<"  Hit number "<<j<<"  m = "<<m<<"  e = "<<e<<"  s = "<<s<<"  adc = "<<a<<"  en = "<<energy<<"\n";
        detector->addHit(rawHit);
      }
    }
    //clusters
    Int_t nc=muEmc->getNClusters(det);
    //cout <<"Number of clusters for det "<<det<<" = "<<nc<<endl;
    if(nc>0)
    {
      StEmcClusterCollection* clusters=new StEmcClusterCollection();
      clusters->setDetector(id);
      detector->setCluster(clusters);
      for(Int_t j=0;j<nc;j++)
      {
        StMuEmcCluster* cl=muEmc->getCluster(det,j);
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
          Int_t hid = cl->getHitId(k);
          Int_t m,e,s,rid;
          if(det==1) // towers
          {
            rid = hid;
          }          
          if(det==2) //prs
          {
            StMuEmcHit *hit=muEmc->getPrsHit(hid);
            rid = hit->getId();
          }
          if(det==3||det==4)
          {
            StMuEmcHit *hit=muEmc->getSmdHit(det,hid);
            rid = hit->getId();
          }
          mGeo[det-1]->getBin(rid,m,e,s);
          StEmcModule *module = detector->module(m);
          StSPtrVecEmcRawHit& rawhits=module->hits();
          //cout <<"Cl = "<<j<<"  m = "<<m<<"  e = "<<e<<"  s = "<<s<<"  eta = "<<eta<<"  phi = "<<phi<<"  E = "<<e<<"  nhits = "<<cl->getNHits()<<endl;
          for(Int_t l=0;l<(Int_t)rawhits.size();l++)
          {
            if(rawhits[l])
            {
              if(m==(Int_t)rawhits[l]->module() && e==(Int_t)rawhits[l]->eta() && s==(Int_t)abs(rawhits[l]->sub()))
                cluster->addHit(rawhits[l]);
            }
          }
        }
        clusters->addCluster(cluster);
      }
    }
  }
  // points  
  //cout <<"Number of points = "<<muEmc->getNPoints()<<endl;
  for(Int_t i=0; i<muEmc->getNPoints();i++)
  {
    StMuEmcPoint *point=muEmc->getPoint(i);
    Float_t eta=point->getEta();
    Float_t deta=point->getDeltaEta();
    Float_t phi=point->getPhi();
    Float_t dphi=point->getDeltaPhi();
    Float_t en=point->getEnergy();
    Float_t chi=point->getChiSquare();
    Float_t theta=2*atan(exp(-eta));
    Float_t mag = point->getRadius();
    if(mag==0) mag = 225.40;
    //cout <<"Po = "<<i<<" eta = "<<eta<<" phi = "<<phi<<" E = "<<en<<" chi = "<<chi<<endl;
    Float_t x,y,z;
    //AAPSUAIDE BUG POINT RADIUS CORRECTED 20030612
    x = mag*cos(phi);
    y = mag*sin(phi);
    z = mag/tan(theta);
    ///////////////////////////////////////////////
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
      StMuEmcCluster *cl=point->getCluster(det);
      if(cl)
      {
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
  
  return;
}
