#include <iostream>
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StiTpcIsActiveFunctor.h"

StDetectorDbTpcRDOMasks *StiTpcIsActiveFunctor::s_pRdoMasks = 0;

StiTpcIsActiveFunctor::StiTpcIsActiveFunctor(int iSector, int iPadrow)
{
  if(s_pRdoMasks==0){
    s_pRdoMasks = StDetectorDbTpcRDOMasks::instance();
  }
  ++iSector;
  ++iPadrow;
  int iRdo = rdoForPadrow(iPadrow);
  m_bWestActive = s_pRdoMasks->isOn(iSector, iRdo);
  m_bEastActive = s_pRdoMasks->isOn(24 - iSector%12, iRdo);
  //cout << "TPC Sector :"<< iSector << " Row : "<< iPadrow<< "   "<< m_bWestActive<<"   " << m_bEastActive<< endl;

} // StiTpcIsActiveFunctor

StiTpcIsActiveFunctor::~StiTpcIsActiveFunctor(){
} // ~StiTpcIsActiveFunctor

bool StiTpcIsActiveFunctor::operator()(double dYlocal, double dZlocal)
{
  if (dZlocal<0.)
    return m_bWestActive && dZlocal>=-200.0;
  else
    return m_bEastActive && dZlocal<= 200.0;
} // operator()

