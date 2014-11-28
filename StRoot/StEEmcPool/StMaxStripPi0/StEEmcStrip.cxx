#include "StEEmcStrip.h"

ClassImp(StEEmcStrip);

// ----------------------------------------------------------------------------
StEEmcStrip::StEEmcStrip()
{
  mIndex=-1;
  mPlane=-1;
  mSector=-1;
}

// ----------------------------------------------------------------------------
void StEEmcStrip::print()
{

}

// ----------------------------------------------------------------------------
void StEEmcStrip::index(Int_t i)
{ 
  mIndex=i; 
  mName="";

  const Char_t *secs[]={"01","02","03","04","05","06","07","08","09","10","11","12"};
  const Char_t *plns[]={"U","V"};

  mName="";
  mName += secs[mSector];
  mName += plns[mPlane];

  if ( mIndex+1<100 ) mName += "0";
  if ( mIndex+1<10 ) mName += "0";
  mName+=mIndex+1;

}
