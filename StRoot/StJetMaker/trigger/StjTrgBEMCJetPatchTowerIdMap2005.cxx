// $Id: StjTrgBEMCJetPatchTowerIdMap2005.cxx,v 1.1 2008/08/12 04:07:01 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgBEMCJetPatchTowerIdMap2005.h"

ClassImp(StjTrgBEMCJetPatchTowerIdMap2005)

int StjTrgBEMCJetPatchTowerIdMap2005::getJetPatchIdForTower(int towerId)
{
  if(2101 <= towerId && towerId <= 2400) return 0;
  if(   1 <= towerId && towerId <=  100) return 0;
  if( 101 <= towerId && towerId <=  500) return 1;
  if( 501 <= towerId && towerId <=  900) return 2;
  if( 901 <= towerId && towerId <= 1300) return 3;
  if(1301 <= towerId && towerId <= 1700) return 4;
  if(1701 <= towerId && towerId <= 2100) return 5;
  return -1;
}

