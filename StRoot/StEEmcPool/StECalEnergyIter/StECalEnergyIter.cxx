//*-- Author : Robert Cadman


#include "StECalEnergyIter.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcHit.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"



StECalEnergyIter::StECalEnergyIter(StMuEmcCollection *emCol, int det,
				   StEEmcDbMaker *eedb) 
  : mEmCol(emCol), mEEdb(eedb), mdetector(det), mIhits(0) {

  char cuv = 'U';
  switch (mdetector)
    {
    case eemc:
      mNhits = mEmCol->getNEndcapTowerADC();
      break;
    case eprs:
      mNhits = mEmCol->getNEndcapPrsHits();
      break;
    case esmdv:
      cuv = 'V'; // and fall through
    case esmdu:
      mNhits = mEmCol->getNEndcapSmdHits(cuv);
      break;
    default:
      mNhits = 0;
    }
}


bool StECalEnergyIter::next(float &e, int &adc, int &adclessped, 
			    int &sec, int &eta, int &phi, char &cdet)
{
  if ( mNhits <= mIhits )  return false;
  const EEmcDbItem *dbitem;
  cdet = 'U';
  switch (mdetector)
    {
    case eemc:
      mEmCol->getEndcapTowerADC(mIhits++, adc, sec, phi, eta);
      cdet = 'T';
      dbitem = mEEdb->getTile(sec, phi-1+'A', eta, cdet);
      break;
    case eprs:
      int prsId;
      adc = mEmCol->getEndcapPrsHit(mIhits++, sec, phi, eta, prsId)->getAdc();
      cdet = prsId-1+'P';
      dbitem = mEEdb->getTile(sec, phi-1+'A', eta, cdet);
      break;
    case esmdv:
      cdet = 'V'; // and fall through
    case esmdu:
      adc = mEmCol->getEndcapSmdHit(cdet, mIhits++, sec, eta)->getAdc();
      dbitem = mEEdb->getByStrip(sec, cdet, eta);
      break;
    default:
      return false;
    }
  if  ( !dbitem || dbitem->fail ) 
    {
      e = - 100.0;
      adclessped = adc;
    }
  else 
    {
      float acorr = adc - dbitem->ped;
      e = acorr * dbitem->gain;
      adclessped = (int) floor(acorr + 0.5);
    }

  return true;
}
