#include "StMuRHICfUtil.h"

#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StRHICfCollection.h"
#include "StEvent/StRHICfRawHit.h"
#include "StEvent/StRHICfHit.h"
#include "StEvent/StRHICfPoint.h"

#include "StMuEvent.h"
#include "StMuDst.h"
#include "StMuRHICfCollection.h"
#include "StMuRHICfRawHit.h"
#include "StMuRHICfHit.h"
#include "StMuRHICfPoint.h"


ClassImp(StMuRHICfUtil)

StMuRHICfUtil::StMuRHICfUtil()
{
}

StMuRHICfUtil::~StMuRHICfUtil()
{
}

StMuRHICfCollection* StMuRHICfUtil::getMuRHICf(StRHICfCollection* coll)
{
  if(!coll) return NULL;

  StMuRHICfCollection* muColl = new StMuRHICfCollection();
  fillMuRHICf(muColl, coll);
  return muColl;
}

StRHICfCollection* StMuRHICfUtil::getRHICf(StMuRHICfCollection* muColl)
{
  if(!muColl) return NULL;
  
  StRHICfCollection* coll = new StRHICfCollection();
  fillRHICf(coll, muColl);
  return coll;
}

void StMuRHICfUtil::fillMuRHICf(StMuRHICfCollection* muColl, StRHICfCollection* coll)
{
  if(!coll) return;
  if(!muColl) return;

  StMuRHICfRawHit* muRHICfRawHit = muColl -> addRawHit();
  StRHICfRawHit* rhicfRawHit = coll -> rawHitCollection();

  muRHICfRawHit -> setRHICfRunNumber(coll->getRHICfRunNumber());
  muRHICfRawHit -> setRHICfEventNumber(coll->getRHICfEventNumber());
  muRHICfRawHit -> setRunType(coll->getRunType());
  muRHICfRawHit -> setBunchNumber(coll->getBunchNumber());
  muRHICfRawHit -> setTriggerNumber(coll->getTriggerNumber());
  muRHICfRawHit -> setRunTime(0, coll->getRunTime(0));
  muRHICfRawHit -> setRunTime(1, coll->getRunTime(1));
  muRHICfRawHit -> setRunTRGM(coll->getRunTRGM());  

  fillMuRHICfRawHit(muRHICfRawHit, rhicfRawHit);

  StRHICfHit* rhicfHit = coll -> hitCollection();
  if(rhicfHit){
    StMuRHICfHit* muRHICfHit = muColl -> addHit();
    fillMuRHICfHit(muRHICfHit, rhicfHit);
  }

  for (unsigned i(0); i < coll->numberOfPoints(); ++i){
    StRHICfPoint* rhicfPoint = coll->pointCollection()[i];
    StMuRHICfPoint* muRHICfPoint = muColl->addPoint();

    if(rhicfPoint && muRHICfPoint){
      fillMuRHICfPoint(muRHICfPoint, rhicfPoint);
    }
  }
}

void StMuRHICfUtil::fillRHICf(StRHICfCollection* coll, StMuRHICfCollection* muColl)
{
  if(!muColl) return;
  if(!coll) return;

  StMuRHICfRawHit* muRHICfRawHit = muColl -> getRawHit();
  StRHICfRawHit* rhicfRawHit = coll -> rawHitCollection();

  if(!muRHICfRawHit) return;
  if(!rhicfRawHit) return;

  coll -> setRHICfRunNumber(muRHICfRawHit->getRHICfRunNumber());
  coll -> setRHICfEventNumber(muRHICfRawHit->getRHICfEventNumber());
  coll -> setRunType(muRHICfRawHit->getRunType());
  coll -> setBunchNumber(muRHICfRawHit->getBunchNumber());
  coll -> setTriggerNumber(muRHICfRawHit->getTriggerNumber());
  coll -> setRunTime(0, muRHICfRawHit->getRunTime(0));
  coll -> setRunTime(1, muRHICfRawHit->getRunTime(1));
  coll -> setRunTRGM(muRHICfRawHit->getRunTRGM());

  fillRHICfRawHit(rhicfRawHit, muRHICfRawHit);

  StMuRHICfHit* muRHICfHit = muColl -> getHit();
  if(muRHICfHit){
    StRHICfHit* rhicfHit = coll -> hitCollection();
    fillRHICfHit(rhicfHit, muRHICfHit);
  }

  for(unsigned i(0); i < muColl->numberOfPoints(); ++i){
    StMuRHICfPoint* muRHICfPoint = muColl->getPoint(i);
    StRHICfPoint* rhicfPoint = new StRHICfPoint();
    
    if(muRHICfPoint && rhicfPoint){
      fillRHICfPoint(rhicfPoint, muRHICfPoint);
      coll -> addPoint(rhicfPoint);
    }
  }
}

Int_t StMuRHICfUtil::checkGSOBarSize(Int_t tower)
{
  if(tower==0){return kRHICfNbarSmall;}
  else if(tower==1){return kRHICfNbarLarge;}
  else{return 0;}
}

void StMuRHICfUtil::fillMuRHICfRawHit(StMuRHICfRawHit* muRHICfRawHit, StRHICfRawHit* rhicfRawHit)
{
  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        for(int ich=0; ich<checkGSOBarSize(it); ich++){
          Int_t gsoBarADC = rhicfRawHit -> getGSOBarADC(it, il, ixy, ich);
          muRHICfRawHit -> setGSOBarADC(it, il, ixy, ich, gsoBarADC);
        }
      }
    }
    for(int ip=0; ip<kRHICfNplate; ip++){
      for(int ir=0; ir<2; ir++){              
        Int_t plateADC = rhicfRawHit -> getPlateADC(it, ip, ir);
        Int_t plateADCD = rhicfRawHit -> getPlateADCDelay(it, ip, ir);
        muRHICfRawHit -> setPlateADC(it, ip, ir, plateADC);
        muRHICfRawHit -> setPlateADCDelay(it, ip, ir, plateADCD);
      }
    }
  }
  for(Int_t idx=0; idx<kRHICfNtdc; idx++){
    if(idx<kRHICfNcad0){muRHICfRawHit -> setCAD0(idx, rhicfRawHit->getCAD0(idx));}
    if(idx<kRHICfNgpi0){muRHICfRawHit -> setGPI0(idx, rhicfRawHit->getGPI0(idx));}
    if(idx<kRHICfNgpi1){muRHICfRawHit -> setGPI1(idx, rhicfRawHit->getGPI1(idx));}
    muRHICfRawHit -> setTDC(idx, rhicfRawHit->getTDC(idx));
  }   
}

void StMuRHICfUtil::fillMuRHICfHit(StMuRHICfHit* muRHICfHit, StRHICfHit* rhicfHit)
{
  if(!muRHICfHit->isSaveDataArray()){
    for(int it=0; it<kRHICfNtower; it++){
      for(int il=0; il<kRHICfNlayer; il++){
        for(int ixy=0; ixy<kRHICfNxy; ixy++){
          for(int ich=0; ich<checkGSOBarSize(it); ich++){
            muRHICfHit -> setGSOBarEnergy(it, il, ixy, ich, rhicfHit->getGSOBarEnergy(it, il, ixy, ich));
          }
        }
      }
      for(int ip=0; ip<kRHICfNplate; ip++){
        muRHICfHit -> setPlateEnergy(it, ip, rhicfHit->getPlateEnergy(it, ip));
      }
    }
  }
  // save the detailed hit data
  else if(muRHICfHit->isSaveDataArray()){
    muRHICfHit -> initDataArray();

    for(int it=0; it<kRHICfNtower; it++){
      muRHICfHit -> setL20(it, rhicfHit->getL20(it)); 
      muRHICfHit -> setL90(it, rhicfHit->getL90(it)); 
      muRHICfHit -> setMultiHitNum(it, rhicfHit->getMultiHitNum(it)); 
      muRHICfHit -> setGSOMaxLayer(it, 0, rhicfHit->getGSOMaxLayer(it, 0)); 
      muRHICfHit -> setGSOMaxLayer(it, 1, rhicfHit->getGSOMaxLayer(it, 1)); 

      for(int il=0; il<kRHICfNlayer; il++){
        for(int ixy=0; ixy<kRHICfNxy; ixy++){
          muRHICfHit -> setMaxPeakBin(it, il, ixy, rhicfHit->getMaxPeakBin(it, il, ixy));
          muRHICfHit -> setSingleHitNum(it, il, ixy, rhicfHit->getSingleHitNum(it, il, ixy));
          muRHICfHit -> setSingleHitPos(it, il, ixy, rhicfHit->getSingleHitPos(it, il, ixy));
          muRHICfHit -> setSinglePeakHeight(it, il, ixy, rhicfHit->getSinglePeakHeight(it, il, ixy));
          muRHICfHit -> setSingleFitChi2(it, il, ixy, rhicfHit->getSingleFitChi2(it, il, ixy));
          muRHICfHit -> setMultiFitChi2(it, il, ixy, rhicfHit->getMultiFitChi2(it, il, ixy));

          for(int ich=0; ich<checkGSOBarSize(it); ich++){
            muRHICfHit -> setGSOBarEnergy(it, il, ixy, ich, rhicfHit->getGSOBarEnergy(it, il, ixy, ich));
          }

          for(int io=0; io<2; io++){
            muRHICfHit -> setMultiHitPos(it, il, ixy, io, rhicfHit->getMultiHitPos(it, il, ixy, io));
            muRHICfHit -> setMultiPeakHeight(it, il, ixy, io, rhicfHit->getMultiPeakHeight(it, il, ixy, io));
            muRHICfHit -> setMultiPeakRaw(it, il, ixy, io, rhicfHit->getMultiPeakRaw(it, il, ixy, io));
            muRHICfHit -> setMultiEnergySum(it, il, ixy, io, rhicfHit->getMultiEnergySum(it, il, ixy, io));
          }
        }
      }
      for(int ip=0; ip<kRHICfNplate; ip++){
        muRHICfHit -> setPlateEnergy(it, ip, rhicfHit->getPlateEnergy(it, ip));
      }
    }
  }
}

void StMuRHICfUtil::fillMuRHICfPoint(StMuRHICfPoint* muRHICfPoint, StRHICfPoint* rhicfPoint)
{
  muRHICfPoint -> setTowerIdx(rhicfPoint->getTowerIdx());
  muRHICfPoint -> setPID(rhicfPoint->getPID());
  muRHICfPoint -> setTowerSumEnergy(rhicfPoint->getTowerSumEnergy(0), rhicfPoint->getTowerSumEnergy(1));
  muRHICfPoint -> setPointPos(rhicfPoint->getPointPos(0), rhicfPoint->getPointPos(1));
  muRHICfPoint -> setPointEnergy(rhicfPoint->getPointEnergy(0), rhicfPoint->getPointEnergy(1));
}

void StMuRHICfUtil::fillRHICfRawHit(StRHICfRawHit* rhicfRawHit, StMuRHICfRawHit* muRHICfRawHit)
{
  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        for(int ich=0; ich<checkGSOBarSize(it); ich++){
          Int_t gsoBarADC = muRHICfRawHit -> getGSOBarADC(it, il, ixy, ich);
          rhicfRawHit -> setGSOBarADC(it, il, ixy, ich, gsoBarADC);
        }
      }
    }
    for(int ip=0; ip<kRHICfNplate; ip++){
      for(int ir=0; ir<2; ir++){         
        Int_t plateADC = muRHICfRawHit -> getPlateADC(it, ip, ir);
        Int_t plateADCD = muRHICfRawHit -> getPlateADCDelay(it, ip, ir);
        rhicfRawHit -> setPlateADC(it, ip, ir, plateADC);
        rhicfRawHit -> setPlateADCDelay(it, ip, ir, plateADCD);
      }
    }
  }
  for(Int_t idx=0; idx<kRHICfNtdc; idx++){
    if(idx<kRHICfNcad0){rhicfRawHit -> setCAD0(idx, muRHICfRawHit->getCAD0(idx));}
    if(idx<kRHICfNgpi0){rhicfRawHit -> setGPI0(idx, muRHICfRawHit->getGPI0(idx));}
    if(idx<kRHICfNgpi1){rhicfRawHit -> setGPI1(idx, muRHICfRawHit->getGPI1(idx));}
    rhicfRawHit -> setTDC(idx, muRHICfRawHit->getTDC(idx));
  }   
}

void StMuRHICfUtil::fillRHICfHit(StRHICfHit* rhicfHit, StMuRHICfHit* muRHICfHit)
{
  if(!rhicfHit->isSaveDataArray()){
    for(int it=0; it<kRHICfNtower; it++){
      for(int il=0; il<kRHICfNlayer; il++){
        for(int ixy=0; ixy<kRHICfNxy; ixy++){
          for(int ich=0; ich<checkGSOBarSize(it); ich++){
            rhicfHit -> setGSOBarEnergy(it, il, ixy, ich, muRHICfHit->getGSOBarEnergy(it, il, ixy, ich));
          }
        }
      }
      for(int ip=0; ip<kRHICfNplate; ip++){
        rhicfHit -> setPlateEnergy(it, ip, muRHICfHit->getPlateEnergy(it, ip));
      }
    }
  }
  // save the detailed hit data
  else if(rhicfHit->isSaveDataArray()){
    rhicfHit -> initDataArray();

    for(int it=0; it<kRHICfNtower; it++){
      rhicfHit -> setL20(it, muRHICfHit->getL20(it)); 
      rhicfHit -> setL90(it, muRHICfHit->getL90(it)); 
      rhicfHit -> setMultiHitNum(it, muRHICfHit->getMultiHitNum(it)); 
      rhicfHit -> setGSOMaxLayer(it, 0, muRHICfHit->getGSOMaxLayer(it, 0)); 
      rhicfHit -> setGSOMaxLayer(it, 1, muRHICfHit->getGSOMaxLayer(it, 1)); 

      for(int il=0; il<kRHICfNlayer; il++){
        for(int ixy=0; ixy<kRHICfNxy; ixy++){
          rhicfHit -> setMaxPeakBin(it, il, ixy, muRHICfHit->getMaxPeakBin(it, il, ixy));
          rhicfHit -> setSingleHitNum(it, il, ixy, muRHICfHit->getSingleHitNum(it, il, ixy));
          rhicfHit -> setSingleHitPos(it, il, ixy, muRHICfHit->getSingleHitPos(it, il, ixy));
          rhicfHit -> setSinglePeakHeight(it, il, ixy, muRHICfHit->getSinglePeakHeight(it, il, ixy));
          rhicfHit -> setSingleFitChi2(it, il, ixy, muRHICfHit->getSingleFitChi2(it, il, ixy));
          rhicfHit -> setMultiFitChi2(it, il, ixy, muRHICfHit->getMultiFitChi2(it, il, ixy));

          for(int ich=0; ich<checkGSOBarSize(it); ich++){
            rhicfHit -> setGSOBarEnergy(it, il, ixy, ich, muRHICfHit->getGSOBarEnergy(it, il, ixy, ich));
          }

          for(int io=0; io<2; io++){
            rhicfHit -> setMultiHitPos(it, il, ixy, io, muRHICfHit->getMultiHitPos(it, il, ixy, io));
            rhicfHit -> setMultiPeakHeight(it, il, ixy, io, muRHICfHit->getMultiPeakHeight(it, il, ixy, io));
            rhicfHit -> setMultiPeakRaw(it, il, ixy, io, muRHICfHit->getMultiPeakRaw(it, il, ixy, io));
            rhicfHit -> setMultiEnergySum(it, il, ixy, io, muRHICfHit->getMultiEnergySum(it, il, ixy, io));
          }
        }
      }
      for(int ip=0; ip<kRHICfNplate; ip++){
        rhicfHit -> setPlateEnergy(it, ip, muRHICfHit->getPlateEnergy(it, ip));
      }
    }
  }
}

void StMuRHICfUtil::fillRHICfPoint(StRHICfPoint* rhicfPoint, StMuRHICfPoint* muRHICfPoint)
{
  rhicfPoint -> setTowerIdx(muRHICfPoint->getTowerIdx());
  rhicfPoint -> setPID(muRHICfPoint->getPID());
  rhicfPoint -> setTowerSumEnergy(muRHICfPoint->getTowerSumEnergy(0), muRHICfPoint->getTowerSumEnergy(1));
  rhicfPoint -> setPointPos(muRHICfPoint->getPointPos(0), muRHICfPoint->getPointPos(1));
  rhicfPoint -> setPointEnergy(muRHICfPoint->getPointEnergy(0), muRHICfPoint->getPointEnergy(1));
}
