//!#include "stdafx.h"

//StJetFinder.cxx
//M.L. Miller (Yale Software)
//4/02

//std
#include <cmath>
#include <float.h>

//JetFinder
#include "FourVec.h"
#include "StProtoJet.h"
#include "StProtoJetPair.h"
#include "StJetFinder.h"

StJetFinder::StJetFinder() : mDebug(false)
{
    cout <<"StJetFinder::StJetFinder()"<<endl;
}

StJetFinder::~StJetFinder()
{
    cout <<"StJetFinder::~StJetFinder()"<<endl;
}

