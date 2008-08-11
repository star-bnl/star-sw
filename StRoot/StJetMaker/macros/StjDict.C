// -*- mode: c++;-*-
// $Id: StjDict.C,v 1.4 2008/08/11 03:50:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackList.h" 
#include "StjTowerEnergyList.h" 
#include "StjFourVecList.h" 
#include "StjJetList.h" 
#include "StjTreeIndex.h" 
#include <vector> 

#ifdef __MAKECINT__ 
#pragma link C++ class vector<StjTrack>+; 
#pragma link C++ class vector<StjTowerEnergy>+; 
#pragma link C++ class vector<StjFourVec>+; 
#pragma link C++ class vector<StjJet>+; 
#pragma link C++ class vector<StjTreeIndex>+; 
#endif 
