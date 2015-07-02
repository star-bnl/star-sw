//#include "StDetectorDbMaker/StDetectorDbEmcRDOMasks.h"
#include "StiEmc/StiEmcIsActiveFunctor.h"
#include "Stiostream.h"

//StDetectorDbEmcRDOMasks *StiEmcIsActiveFunctor::_rdoMasks = 0;

StiEmcIsActiveFunctor::StiEmcIsActiveFunctor(int iSector, int iLayer)
{
  //if(_rdoMasks==0)
  //  _rdoMasks = StDetectorDbEmcRDOMasks::instance();
  //int iRdo = rdo(iSector,iLayer);
  _westActive = false;//_rdoMasks->isOn(iSector, iRdo);
  _eastActive = false;//_rdoMasks->isOn(24 - iSector%12, iRdo);

} // StiEmcIsActiveFunctor

StiEmcIsActiveFunctor::~StiEmcIsActiveFunctor()
{} // ~StiEmcIsActiveFunctor

bool StiEmcIsActiveFunctor::operator()(double dYlocal, double dZlocal) const
{
  return (_westActive && dZlocal>-2.0 ||_eastActive && dZlocal< 2.0);
} // operator()

