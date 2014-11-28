// Hey Emacs this is -*-c++-*-
// $Id: EEmcTower.cxx,v 1.7 2007/07/12 19:27:23 fisyak Exp $

/**
 * \class  EEmcTower
 * \brief EEmcTower holds information about an EEMC tower 'hit'
 *
 * EEmcTower holds information about an EEMC tower 'hit'.
 * The information includes tower description (sector, subsector and eta), 
 * pedestal adjusted ADC value and (optionally) calibrated energy loss
 *
 * \author Piotr A. Zolnierczuk
 *
 * $Date: 2007/07/12 19:27:23 $ 
 * $Revision: 1.7 $
 *
 * \section towerremarks Remarks
 * \bug  implicitly assumed that labels are no longer than 16 characters
 */



#include <iostream>
#include <ostream>
#include <cctype>

#include "TList.h"
#include "EEmcTower.h"

#if !defined(ST_NO_NAMESPACES)
using std::ostream;
#endif

ClassImp(EEmcTower);


static const int kMaxLabelLen = 16;  /// FIXME - assumed max tower label length

//
EEmcTower::EEmcTower(const char *label , float adc, float ene) 
{
 mSec  = -1;
 mSub  = -1;
 mEta  = -1;
 mADC  = 0.0;
 mEdep = 0.0;
 if( ! ParseLabel(label) ) return;
 mADC  = adc;
 mEdep = ene;
}
//                                

//=============================================================================
bool
EEmcTower::ParseLabel(const char *label)
{
  if(label==NULL) return false;
  char const *p = label;
  int  s=0;
  char ss=0;
  char t=0;
  int  e=0;
  while( *p != 0x00 && !islower(*p) && !isupper(*p) && !isdigit(*p) ) p++;
  int  r=sscanf(p,"%02d%1c%1c%02d",&s,&t,&ss,&e);
  if(toupper(t)!='T') return false;
  if(r!=4) return false;
  // adjust indices
  mSec = s  - 1;
  mSub = ss - 'A';
  mEta = e  - 1;  
  return true; 
  
}

//=============================================================================
const char* 
EEmcTower::TowerLabel() const 
{
  static char labelBuffer[kMaxLabelLen];
  sprintf(labelBuffer,"%02dT%1c%02d",SecLabel(),SubSecLabel(),EtaLabel());
  return labelBuffer;
}


//=============================================================================
ostream& 
EEmcTower::Out(ostream &out ) const
{
  out << "<EEmcTower";
  out << " TOWER=\"" << TowerLabel() << "\"";
  out << " ADC=\""   << mADC   << "\"";
  out << " EDEP=\""  << mEdep  << "\"";
  out << " />\n";
  return out;
}


//=============================================================================
ostream&  operator<<(ostream &out, const EEmcTower    &t  )  {
  return t.Out(out);
}

