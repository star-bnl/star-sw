/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtTrivia
 *
 ***************************************************************************
 *
 * Description: See header file.
 *
 ***************************************************************************/

#include "StGmtTrivia.h"


StGmtTrivia::~StGmtTrivia() { /* no op */ }

StGmtTrivia::StGmtTrivia() : StObject(), mGeoId(-1) 
{
}

StGmtTrivia::StGmtTrivia(const StGmtTrivia& h) :
  StObject(),                                  // copy the parent
  mGeoId( h.mGeoId )
{
  for (int i=0;i<15;i++) mAdc[i]=-9998;
}

StGmtTrivia:: StGmtTrivia(int GeoId, int moduleX, int layerX, int stripX) : StObject() 
{
  mGeoId = GeoId;
  mModuleX = moduleX;
  mLayerX = layerX;
  mStripX = stripX;
  for (int i=0;i<15;i++) mAdc[i]=-9998;
  
  mCX = -77.;  mCY = -77.;  mCAX = -77.;  mCAY = -77.;
  mClustSize = 0;
  mRunId=0;
}

// note: there is no risk in assigning an StGmtTrivia to equal itself.
StGmtTrivia& StGmtTrivia::operator=( const StGmtTrivia& h) {
  mGeoId = h.mGeoId;
  return *this;
}

ClassImp(StGmtTrivia)
