/***************************************************************************
 *
 * $Id: StMuFmsUtil.cxx,v 1.1 2010/01/25 03:57:39 tone421 Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: FMS Util to convert between StEvent and MuDst
 *
 ***************************************************************************
 *
 * $Log: StMuFmsUtil.cxx,v $
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#include "StMuFmsHit.h"
#include "StMuFmsUtil.h"
#include "StMuFmsCollection.h"
#include "StEvent.h"
#include "StMessMgr.h"
#include "StEventTypes.h"

ClassImp(StMuFmsUtil)


StMuFmsUtil::StMuFmsUtil()
{
}
StMuFmsUtil::~StMuFmsUtil()
{
}

StMuFmsCollection* StMuFmsUtil::getMuFms(StFmsCollection *fmscol)
{
  if(!fmscol) return NULL;
  StMuFmsCollection* muFms=new StMuFmsCollection();
  fillMuFms(muFms,fmscol);
  return muFms;
}  

StFmsCollection* StMuFmsUtil::getFms(StMuFmsCollection* muFms)
{
  if(!muFms) return NULL;
  
  StFmsCollection *fms=new StFmsCollection();
  fillFms(fms,muFms);
  return fms;
}

void StMuFmsUtil::fillMuFms(StMuFmsCollection *muFms,StFmsCollection *fmscol)
{
  if(!fmscol) return;
  if(!muFms) return;
      
  // starting by hits;
  //cout <<"Filling StMuFmsCollection hits\n";
  StSPtrVecFmsHit vecHit = fmscol->hits();
  for(unsigned int i=0; i<fmscol->numberOfHits(); i++){
    unsigned short detId = vecHit[i]->detectorId();
    unsigned short ch    = vecHit[i]->channel();
    unsigned short crate = vecHit[i]->qtCrate();
    unsigned short slot  = vecHit[i]->qtSlot();
    unsigned short qtch  = vecHit[i]->qtChannel();
    unsigned short adc   = vecHit[i]->adc();
    unsigned short tdc   = vecHit[i]->tdc();
    float          ene   = vecHit[i]->energy();
    muFms->addHit();
    StMuFmsHit* muFmsHit = muFms->getHit(i);
    muFmsHit->setDetectorId(detId);
    muFmsHit->setChannel(ch);
    muFmsHit->setQtCrate(crate);
    muFmsHit->setQtSlot(slot);
    muFmsHit->setQtChannel(qtch);
    muFmsHit->setAdc(adc);
    muFmsHit->setTdc(tdc);
    muFmsHit->setEnergy(ene);
  }

  return;
}

void StMuFmsUtil::fillFms(StFmsCollection* fmscol,StMuFmsCollection* muFms)
{
  if(!muFms) return;
  if(!fmscol) return;
  //cout <<"Filling StMuFmsCollection hits\n";
  TClonesArray* arrHit = muFms->getHitArray();
  for(unsigned int i=0; i<muFms->numberOfHits(); i++){
    unsigned short detId = ((StMuFmsHit*)(arrHit->At(i)))->detectorId();
    unsigned short ch    = ((StMuFmsHit*)(arrHit->At(i)))->channel();
    unsigned short crate = ((StMuFmsHit*)(arrHit->At(i)))->qtCrate();
    unsigned short slot  = ((StMuFmsHit*)(arrHit->At(i)))->qtSlot();
    unsigned short qtch  = ((StMuFmsHit*)(arrHit->At(i)))->qtChannel();
    unsigned short adc   = ((StMuFmsHit*)(arrHit->At(i)))->adc();
    unsigned short tdc   = ((StMuFmsHit*)(arrHit->At(i)))->tdc();
    float          ene   = ((StMuFmsHit*)(arrHit->At(i)))->energy();

    StFmsHit* hit = new StFmsHit();
    hit->setDetectorId(detId);
    hit->setChannel(ch);
    hit->setQtCrate(crate);
    hit->setQtSlot(slot);
    hit->setQtChannel(qtch);
    hit->setAdc(adc);
    hit->setTdc(tdc);
    hit->setEnergy(ene);
    fmscol->addHit(hit);
  }


  return;
}

