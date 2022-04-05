#include "StMuEmcUtil.h"
#include "StEvent.h"
#include "StMessMgr.h"
#include "StEventTypes.h"
#include "StMuEmcCollection.h"
#include "SystemOfUnits.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StMuEmcTowerData.h"

ClassImp(StMuEmcUtil)

#define __EMC_HITS_ID_DIM__ 18000


StMuEmcUtil::StMuEmcUtil()
    : TObject()
{
  for(Int_t i =0;i<4;i++) mGeo[i]=StEmcGeom::getEmcGeom(i+1);
}
StMuEmcUtil::~StMuEmcUtil()
{
}
StMuEmcCollection* StMuEmcUtil::getMuEmc(const StEmcCollection *emccol)
{
  if(!emccol) return NULL;
  StMuEmcCollection* muEmc=new StMuEmcCollection();
  fillMuEmc(muEmc,emccol);
  return muEmc;
}  
StEmcCollection* StMuEmcUtil::getEmc(const StMuEmcCollection* muEmc)
{
  if(!muEmc) return NULL;
  StEmcCollection *emc=new StEmcCollection();
  fillEmc(emc,muEmc);
  return emc;
}
void StMuEmcUtil::fillMuEmc(StMuEmcCollection *muEmc, const StEmcCollection *emccol)
{
  if(!emccol) return;
  if(!muEmc) return;
      
  // starting by hits;    
  //cout <<"Filling hits and clusters \n";
  for(Int_t d=0; d<8; d++)
  {  
    Int_t EmcDet=d+1;
    
    StDetectorId id = static_cast<StDetectorId>(d+kBarrelEmcTowerId);
    const StEmcDetector* detector=emccol->detector(id);
    if(detector)
    {                          
      Int_t maxMod = 121;
      if(d>3) maxMod = 14;
      //cout <<"Filling hits for detetor "<<EmcDet<<endl;
      for(Int_t j=1;j<maxMod;j++)
      {
        const StEmcModule* module = detector->module(j);
        if(module)
        {
          const StSPtrVecEmcRawHit& rawHit=module->hits();
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
              bool save = true;
              if(d<4 && cal>127) save = false;
              if(save)
              {
                if (d<4) // for the barrel
                {
                  mGeo[d]->getId(m,e,s,rid);
                }
                else
                {
                  if(getEndcapId(EmcDet,m,e,s,rid)) continue;// on error
                }
              
                if(EmcDet == 1 || EmcDet == 5  ) // towers save only ADC
                {
                  muEmc->setTowerADC(rid,adc,EmcDet);
                }              
                if(EmcDet==2 || EmcDet == 6) //pre shower
                {
                  muEmc->addPrsHit(EmcDet);
                  StMuEmcHit* muHit = muEmc->getPrsHit(muEmc->getNPrsHits(EmcDet)-1,EmcDet); 
                  muHit->setId(rid);
                  muHit->setAdc(adc);
                  muHit->setEnergy(energy);
                  muHit->setCalType(cal);      
                }
                if(EmcDet==3 || EmcDet==4 || EmcDet==7 || EmcDet==8)
                {
                  muEmc->addSmdHit(EmcDet);
                  StMuEmcHit* muHit = muEmc->getSmdHit(muEmc->getNSmdHits(EmcDet)-1,EmcDet);          
                  muHit->setId(rid);
                  muHit->setAdc(adc);
                  muHit->setEnergy(energy);
                  muHit->setCalType(cal);
                }      
	      }
	    }
	  
	}
      }
      Int_t n_crate=0;
      switch (EmcDet) {
      case 1:
	n_crate=StMuEmcTowerData::nBTowCrates;
	break;
      case 2:
	n_crate=StMuEmcTowerData::nBPrsCrates;
	break;
      case 3:
	n_crate=StMuEmcTowerData::nBSmdCrates;
	break;
      case 5:
	n_crate=StMuEmcTowerData::nETowCrates;
	break;
      case 6:
	n_crate=StMuEmcTowerData::nEPrsCrates;
	break;
      case 7:
	n_crate=StMuEmcTowerData::nESmdCrates;
	break;
      }
      for (Int_t i_crate=1; i_crate<=n_crate; i_crate++) {
	muEmc->setCrateStatus(detector->crateStatus(i_crate),i_crate,EmcDet);
      }
    }
  } 
  
  return;
}

void StMuEmcUtil::fillEmc(StEmcCollection* emc, const StMuEmcCollection* muEmc)
{
  if(!muEmc) return;
  if(!emc) return;
  //cout <<"FILLING EMC COLLECTION\n";
  
  for(Int_t i=0;i<8;i++)
  {
    Int_t det=i+1;
    
    StDetectorId id = static_cast<StDetectorId>(i+kBarrelEmcTowerId);
    int nMod = 120;
    if(i>=4) nMod = 13;
    StEmcDetector* detector = new StEmcDetector(id, nMod);
    emc->setDetector(detector);
    // hits
    Int_t nh=0;
    if (det==1) nh = 4800; 
    if (det==5) nh = 720; 
    if (det==2 || det ==6) nh=muEmc->getNPrsHits(det);
    if (det==3 || det==4 || det==7 || det==8) nh=muEmc->getNSmdHits(det);
    //cout <<"Number of hits for detector "<<det<<" = "<<nh<<endl;
    for(Int_t j=0;j<nh;j++)
    {
      Bool_t save = kTRUE;
      Int_t m,e,s,rid;
      Int_t a=0,cal=0;
      Float_t energy=0;
      if(det==1 || det==5) // towers have only ADC
      {
        a = muEmc->getTowerADC(j+1,det);
        if(det==1) mGeo[det-1]->getBin(j+1,m,e,s);
        else  {
	  if( getEndcapBin(det,j+1,m,e,s)) continue ;// on error
	}
        energy = 0;
        cal    = 0;
        if(a==0) save = kFALSE;
      }      
      if(det==2 || det ==6) //prs
      {
        const StMuEmcHit* hit=muEmc->getPrsHit(j,det);
        if(hit)
        {
          rid=hit->getId();
          if(det==2) mGeo[det-1]->getBin(rid,m,e,s);
          else {
	    if( getEndcapBin(det,rid,m,e,s)) continue ;// on error
	  }
          a=hit->getAdc();
          cal=hit->getCalType();
          energy=hit->getEnergy();
        } else save = kFALSE;
      }
      if(det==3 || det==4 || det==7 || det==8) //smd
      {
        const StMuEmcHit* hit=muEmc->getSmdHit(j,det);
        if(hit)
        {
          rid=hit->getId();
          if(det<5) mGeo[det-1]->getBin(rid,m,e,s);
          else {
	    if( getEndcapBin(det,rid,m,e,s)) continue ;// on error
	  }
          a=hit->getAdc();
          cal=hit->getCalType();
          energy=hit->getEnergy();
        } else save = kFALSE;
      }
      if(save)
      {
        StEmcRawHit* rawHit=new StEmcRawHit(id,(UInt_t)m,(UInt_t)e,(UInt_t)s,(UInt_t)a,energy);
        rawHit->setCalibrationType(cal);
        //cout <<"det = "<<det<<"  Hit number "<<j<<"  m = "<<m<<"  e = "<<e<<"  s = "<<s<<"  adc = "<<a<<"  en = "<<energy<<"\n";
        detector->addHit(rawHit);
      }
    }
  
    Int_t n_crate=0;
    switch (det) {
    case 1:
      n_crate=StMuEmcTowerData::nBTowCrates;
      break;
    case 2:
      n_crate=StMuEmcTowerData::nBPrsCrates;
      break;
    case 3:
    case 4:
      n_crate=StMuEmcTowerData::nBSmdCrates;
      break;
    case 5:
      n_crate=StMuEmcTowerData::nETowCrates;
      break;
    case 6:
      n_crate=StMuEmcTowerData::nEPrsCrates;
      break;
    case 7:
    case 8:
      n_crate=StMuEmcTowerData::nESmdCrates;
      break;
    }
    for (Int_t i_crate=1; i_crate<=n_crate; i_crate++) {
      detector->setCrateStatus(i_crate,muEmc->getCrateStatus(i_crate,det));
    }
  }
  return;
}

//=================================================
//=================================================
int  StMuEmcUtil::getEndcapId(int d,int m, int e, int s,int &rid) const {
  rid=1;
  /*  first tower or first strip is default,
      I do not like it, but it is the only safe value,JB
  */
 
  TString text;

  if(     m<=0 || m >12 || d<5 || d>8 ) {
    text="m<=0 || m >12 || d<5 || d>8 ";
    goto abort;
  }
  
  switch (d) {

  case 5: 
    if(     e<=0 || e>12 || s<=0 || s>5 ) {
      text="e<=0 || e>12 || s<=0 || s>5 ,towers";
      goto abort;
    } 
    rid = 60*(m-1) + 12*(s-1) + e-1;
    break;

  case 6: 
    if(     e<=0 || e>12 || s<=0 || s>15 ) {
      text="e<=0 || e>12 || s<=0 || s>15 ,  pre/post";
      goto abort;
    } 
    rid = 180*(m-1) + 12*(s-1) + e-1;
    break;

  case 7: 
  case 8:
    if(     s!=1 || e<=0 || e>288 ) {
      text=" s!=1 || e<=0 || e>288,  SMD";
      goto abort;
    } 
    rid = 300*(m-1) + e-1;
    break;
  default:;
  }

  rid++; // to compensate for counting from 1

  if(     rid<=0 ) {
      text=" rid<=0";
      goto abort;
    } 

  return 0;  // all went OK

 abort:
  gMessMgr->Error() <<"StMuEmcUtil::getEndcapId(), FATAL internal error: "
		    <<text<<"\n d="<<d<<" m="<<m<<" e="<<" s="<<s<<" rid "<<rid
		    <<"\n ENDCAP data may be wrong, " << endm;
  return 1;
}


int StMuEmcUtil::getEndcapBin(int d,int rid0,int &m, int &e, int &s) const
{
  m=e=s=1;
  /*  first tower or first strip is default,
      I do not like it, but it is the only safe value,JB
  */
 
  TString text;
  int rid=rid0-1;
  int x;

  if(     rid0<=0 || d<5 || d>8 ) {
    text="rid0<=0 || d<5 || d>8 ";
    goto abort;
  }

  switch (d) {

  case 5: 
    m=1+ rid/60;
    x=rid%60;
    s=1 +x/12;
    e=1 + x%12;
    if     ( m>12 || s<1 || s>5 || e>12 ) {
      text=" m>12 || s<1 || s>5 || e>12 , tower";
      goto abort;
    }
    break;

  case 6:
    m=1+ rid/180;
    x=rid%180;
    s=1 +x/12;
    e=1 + x%12;
    if     ( m>12 || s<1 || s>15 || e>12 ) {
      text=" m>12 || s<1 || s>15 || e>12 , pre/post";
      goto abort;
    }
    break;

  case 7:
  case 8:
    m=1+ rid/300;
    s=1;
    e=1 + rid%300;
    if     ( m>12 || e>288 || e <1 ) {
      text=" m>12 || s<=0 || s>5 || e>12 , smd";
      goto abort;
    }
    break;
  default: ;
  }
  
  return 0; // all went OK
  
 abort:
  gMessMgr->Error() <<"StMuEmcUtil::getEndcapBin(), FATAL internal error: "
		    <<text<<"\n d="<<d<<" m="<<m<<" e="<<" s="<<s<<" rid0 "<<rid0
		    <<"\n ENDCAP data may be wrong, "
		    <<" assert() should be here, JB"<<endm;
  return 1;
}
