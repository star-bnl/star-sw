//*-- Author : Robert Cadman


#include "StECalEnergyIter.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcHit.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"


bool StECalEnergyIter::mIsSimu = false;

StECalEnergyIter::StECalEnergyIter(StMuEmcCollection *emCol, int det,
				   StEEmcDbMaker *eedb, bool flag) 
  : mEmCol(emCol), mEEdb(eedb), mdetector(det), mIhits(0), mSuppBad(flag) {

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
  const EEmcDbItem *dbitem;
  cdet = 'U';

  do {
    if ( mNhits <= mIhits )  return false;    
    switch (mdetector)
      {
      case eemc:
	mEmCol->getEndcapTowerADC(mIhits++, adc, sec, phi, eta);
	cdet = 'T';
	dbitem = mEEdb->getTile(sec, phi-1+'A', eta, cdet);
	break;
      case eprs:
	int prsId;
	adc = mEmCol->getEndcapPrsHit(mIhits++, sec, phi, eta,
				      prsId)->getAdc();
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
  } while ( mSuppBad && ( !dbitem || dbitem->fail || dbitem->gain < 0.5 ) );

  if  ( !dbitem || dbitem->fail || dbitem->ped < 1 ) 
    {
      e = - 100.0;
      adclessped = adc;
    }
  else
    {
      float acorr = adc - dbitem->ped;
      adclessped = (int) floor(acorr + 0.5);
      if ( dbitem->gain < 0.0001 )
	e = -100.0;
      else
	{
	  e = acorr/dbitem->gain;
	  if ( mIsSimu && mdetector == eemc ) e *= 1.25;
	}
    }

  return true;
}
