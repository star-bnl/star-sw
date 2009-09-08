//*-- Author : Robert Cadman


#include "StECalEnergyIter.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcHit.h"
#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "TMath.h"

bool StECalEnergyIter::mIsSimu = false;

StECalEnergyIter::StECalEnergyIter(StMuEmcCollection *emCol, int det, StEEmcDb *db, bool flag) 
  : mEmCol(emCol), mEEdb(db), mdetector(det), mIhits(0), mSuppBad(flag) {

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
  bool dbfail;

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
    dbfail = ( !dbitem || dbitem->fail || dbitem->gain < 0.5 );
  } while ( mSuppBad && dbfail );

  if  ( dbfail ) 
    {
      e = - 100.0;
      adclessped = adc;
    }
  else 
    {
      float acorr = adc - dbitem->ped;
      e = acorr/dbitem->gain;
      if ( mIsSimu && mdetector == eemc ) e *= 1.25;
      adclessped = (int) TMath::Floor(acorr + 0.5);
    }

  return true;
}
