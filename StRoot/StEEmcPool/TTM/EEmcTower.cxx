// Hey Emacs this is -*-c++-*-
// $Id: EEmcTower.cxx,v 1.2 2004/05/05 23:00:57 zolnie Exp $

/**
 * \class  EEmcTower
 *
 * \brief EEmcTower holds information about an EEMC tower 'hit'
 *
 *
 * It holds tower description (sector, subsector and eta) plus pedestal adjusted ADC value
 * and optionally calibrated energy loss
 *
 * \author Piotr A. Zolnierczuk
 * \date   2004/04/30

 * \bug  implicitly assumed that labels are no longer than kMaxLabelLen (16 characters)

 * \todo implement EEmcTower::ParseLabel
 */



#include <ostream>

#include "TList.h"
#include "EEmcTower.h"

#if !defined(ST_NO_NAMESPACES)
using std::ostream;
#endif

ClassImp(EEmcTower);


static const int kMaxLabelLen = 16;  /// FIXME - assumed max tower label length



//=============================================================================
void 
EEmcTower::ParseLabel(const char *label)
{
  if(label==NULL) return;
  //int len = strnlen(label,kMaxLabelLen);
  // TODO implement ParseLabel 
  assert(0); // not yet implemented
}

//=============================================================================
void
EEmcTower::WriteLabel()
{
  if(mLabel) delete mLabel;
  mLabel = new char[kMaxLabelLen];
  sprintf(mLabel,"%02dT%1c%02d",SecLabel(),SubSecLabel(),EtaLabel());
}

//=============================================================================
ostream& 
EEmcTower::Out(ostream &out ) const
{
  out << "<EEmcTower";
  out << " TOWER=" << mLabel ;
  out << " ADC="   << mADC ;
  out << " EDEP="  << mEdep;
  out << " />\n";
  return out;
}


//=============================================================================
ostream&  operator<<(ostream &out, const EEmcTower    &t  )  {
  return t.Out(out);
}

