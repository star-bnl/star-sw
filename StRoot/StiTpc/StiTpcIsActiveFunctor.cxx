#include <iostream>
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StiTpcIsActiveFunctor.h"

StDetectorDbTpcRDOMasks *StiTpcIsActiveFunctor::s_pRdoMasks = 0;

StiTpcIsActiveFunctor::StiTpcIsActiveFunctor(int iSector, int iPadrow){
  if(s_pRdoMasks==0){
    s_pRdoMasks = StDetectorDbTpcRDOMasks::instance();
  }
  ++iSector;
  ++iPadrow;
  int iRdo = rdoForPadrow(iPadrow);
  m_bWestActive = s_pRdoMasks->isOn(iSector, iRdo);
  m_bEastActive = s_pRdoMasks->isOn(24 - iSector%12, iRdo);

} // StiTpcIsActiveFunctor

StiTpcIsActiveFunctor::~StiTpcIsActiveFunctor(){
} // ~StiTpcIsActiveFunctor

bool StiTpcIsActiveFunctor::operator()(double dYlocal, double dZlocal)
{
  return true;
  //return (m_bWestActive && dZlocal>-2.0 ||
  //        m_bEastActive && dZlocal< 2.0);
} // operator()

