// $Id: StjTrgBEMCJetPatchTowerIdMap2006.cxx,v 1.1 2008/10/16 19:47:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgBEMCJetPatchTowerIdMap2006.h"

ClassImp(StjTrgBEMCJetPatchTowerIdMap2006)

int StjTrgBEMCJetPatchTowerIdMap2006::getJetPatchIdForTower(int towerId)
{
  if(   1 <= towerId && towerId <=  100) return 1;
  if( 101 <= towerId && towerId <=  500) return 2;
  if( 501 <= towerId && towerId <=  900) return 3;
  if( 901 <= towerId && towerId <= 1300) return 4;
  if(1301 <= towerId && towerId <= 1700) return 5;
  if(1701 <= towerId && towerId <= 2100) return 0;
  if(2101 <= towerId && towerId <= 2400) return 1;
  if(2401 <= towerId && towerId <= 2500) return 7;
  if(2501 <= towerId && towerId <= 2900) return 6;
  if(2901 <= towerId && towerId <= 3300) return 11;
  if(3301 <= towerId && towerId <= 3700) return 10;
  if(3701 <= towerId && towerId <= 4100) return 9;
  if(4101 <= towerId && towerId <= 4500) return 8;
  if(4501 <= towerId && towerId <= 4800) return 7;
  return -1;
}

