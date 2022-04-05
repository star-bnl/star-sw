#ifdef WIN32
#include "stdafx.h"
#endif

//StProtoJetPair.cxx
//M.L. Miller (Yale Software)
//4/02

//std
#include <iostream>
#include <algorithm>
#include <utility>

//JetFinder
#include "StProtoJet.h"
#include "StProtoJetPair.h"

double deltaphi(double p1, double p2)
{
    float dp = p1 - p2;
    while(dp >  M_PI) {dp -= 2.0 * M_PI;}
    while(dp < -1.*M_PI) {dp += 2.0 * M_PI;}
    return dp;
};


StProtoJetPair::StProtoJetPair(const StProtoJet& j1, const StProtoJet& j2, double r)
    : mJet1(j1), mJet2(j2), mR(r)
{
}

