// Hey Emacs this is -*-c++-*-
// $Id: EEmcTower.cxx,v 1.3 2004/05/06 15:42:47 zolnie Exp $

/**
 * \class  EEmcTower
 * \author Piotr A. Zolnierczuk
 * \date    created 2003/04/30, last modified $Date: 2004/05/06 15:42:47 $
 * \version $Revision: 1.3 $
 *
 * \brief EEmcTower holds information about an EEMC tower 'hit'
 *
 *
 * The information includes tower description (sector, subsector and eta), 
 * pedestal adjusted ADC value and (optionally) calibrated energy loss
 *
 *
 * \bug  implicitly assumed that labels are no longer than 16 characters
 * 
 * \todo 
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
 mLabel= "";
 if( ! ParseLabel(label) ) return;
 mADC  = adc;
 mEdep = ene;
 WriteLabel();
};
//                                

//=============================================================================
bool
EEmcTower::ParseLabel(const char *label)
{
  if(label==NULL) return false;
  char const *p = label;
  int  s=0;
  char ss=0;
  int  e=0;
  while( *p != 0x00 && !islower(*p) && !isupper(*p) && !isdigit(*p) ) p++;
  int  r=sscanf(p,"%02dT%1c%02d",&s,&ss,&e);
  if(r!=3) return false;
  // adjust indices
  mSec = s - 1;
  mSub = ss -1;
  mEta = e-1;  
  // format label 
  return true; 
  
}

//=============================================================================
bool 
EEmcTower::WriteLabel()
{
  if(mLabel) delete mLabel;
  mLabel = new char[kMaxLabelLen];
  int r = sprintf(mLabel,"%02dT%1c%02d",SecLabel(),SubSecLabel(),EtaLabel());
  return (r==3);
}

//=============================================================================
ostream& 
EEmcTower::Out(ostream &out ) const
{
  out << "<EEmcTower";
  out << " TOWER=\"" << mLabel << "\"";
  out << " ADC=\""   << mADC   << "\"";
  out << " EDEP=\""  << mEdep  << "\"";
  out << " />\n";
  return out;
}


//=============================================================================
ostream&  operator<<(ostream &out, const EEmcTower    &t  )  {
  return t.Out(out);
}

