//StEmcTrackMatcher.cxx
//M.L. Miller (MIT Software)
//9/04

//std
using namespace std;
#include <string>
#include <iostream>
#include <algorithm>
#include <cmath>

//root
#include "TFile.h"
#include "TNtuple.h"

//star
#include "StChain.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//Endcap
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

//Barrel
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"

//local
#include "StJetMaker/StTrackMatchers/StEmcTrackMatcher.h"

ClassImp(StEmcTrackMatcher)
  
    StEmcTrackMatcher::StEmcTrackMatcher(const char* name,  StMuDstMaker* m, StBarrelHitMaker* b, StEndcapHitMaker* e)
	: StMaker(name), mMuDstMaker(m), mBarrel(b), mEndcap(e)
{
    mPrint=false;
    mDeltaEta = 0.1; //cut to search by...
}

void StEmcTrackMatcher::Clear(Option_t* opt)
{
    mHits.clear();
    return StMaker::Clear();
}

Int_t StEmcTrackMatcher::Init()
{
    return kStOk;
}

Int_t StEmcTrackMatcher::Make()
{
    cout <<" Start StEmcTrackMatcher :: "<< GetName() <<" mode="<<m_Mode<<endl;
    EemcHitVec& ehits = mEndcap->hits();
    BemcHitVec& bhits = mBarrel->hits();

    for (EemcHitVec::iterator i1=ehits.begin(); i1!=ehits.end(); ++i1) {
	EemcHit& h = (*i1);
	mHits.push_back(&h);
	
    }
    for (BemcHitVec::iterator i2=bhits.begin(); i2!=bhits.end(); ++i2) {
	BemcHit& h = (*i2);
	mHits.push_back(&h);
    }

    //now sort first by eta, then by phi:
    sort(mHits.begin(), mHits.end(), EmcHitSorter() );

    if (mPrint) {
	cout <<"\n----------------------- Emc Hits ------------------------"<<endl;
	for (EmcHitVec::iterator it=mHits.begin(); it!=mHits.end(); ++it) {
	    EmcHit* h = *it;
	    cout <<"\t"<< *h <<endl;
	}
    }

    match();
    
    return kStOk;
}

double niceDeltaPhi(double p1, double p2)
{
    double dp = p1 - p2;
    while(dp >  M_PI) {dp -= 2.0 * M_PI;}
    while(dp < -1.*M_PI) {dp += 2.0 * M_PI;}
    return dp;
}

double gThetaFromEta(double eta)
{
    return 2*atan(exp(-eta));
}

void StEmcTrackMatcher::match()
{
    TClonesArray& tracks = *(mMuDstMaker->muDst()->primaryTracks());
    int ntracks = tracks.GetLast()+1;
    for (int i=0; i<ntracks; ++i) {
	StMuTrack* track = static_cast<StMuTrack*>( tracks[i] );
	
	if (track && track->flag()>0 && track->pt()>0.2) {
	    matchToBarrel(track);
	    matchToEndcap(track);
	}
    }
}

StThreeVectorD matchToPlane(StMuTrack* track, const double z)
{
    const StPhysicalHelixD& h = track->outerHelix();

    StThreeVectorD fail(-999., -999., -999.);

    
    //loook for intersection with plane:
    StThreeVectorD point(0., 0., z);
    StThreeVectorD normal(0., 0., 1.);
    const double NoSolution = 3.e+33; //taken from StRoot/StarClassLibrary/StHelixD.cc
    double s = h.pathLength( point, normal );
    if (s>=NoSolution || s<0) return fail;
    
    //location of intersection with plane
    StThreeVectorD where = h.at(s);
    
    //quick check:
    if (where.perp()>500.) return fail; //way out of bounds radially

    return where;    
}


StThreeVectorD matchToCylinder(StMuTrack* track, const double radius)
{
    StPhysicalHelixD h = track->outerHelix();
    pairD VALUE(999999999.,999999999.); //taken from StRoot/StarClassLibrary/StHelixD.cc
    StThreeVectorD fail(-999., -999., -999.);
    
    //look for intersection with cylinder
    pairD spair = h.pathLength( radius );
    if (spair.first==VALUE.first && spair.second==VALUE.second) return fail; //no match with cylinder
    
    //else, choose the more reasonable of the two solutions.
    double s = 0.;
    if (spair.first>0 && spair.first<VALUE.first) {
	s = spair.first;
    }
    else if (spair.second>0 && spair.second<VALUE.second) {
	s = spair.second;
    }
    else { //no match to cylinder...
	return fail;
    }

    //location of intersection with cylinder
    StThreeVectorD where = h.at(s);

    //rough check to see if it's inbounds z-wise...
    if (fabs(where.z())>350.) return fail;

    return where;

}

void StEmcTrackMatcher::matchToEndcap(StMuTrack* track)
{
    const double zs[3] = { 270.19, 279.542, 306.1580}; //mega1, smd, and mega24
    StThreeVectorD intersections[3];
    for (int i=0; i<3; ++i) {
	intersections[i] = matchToPlane(track, zs[i]);
    }
    
    const StThreeVectorD& where = intersections[1];  //smd point.
    if (where.x()==-999.) return; //no intersection with smd, forget it.
    
    //ok, now look for towers nearby...
    EmcHit lower, upper; //temporary hits for retreival
    StThreeVectorD temp;
    
    double theta1 = gThetaFromEta( where.pseudoRapidity()-mDeltaEta );
    double theta2 = gThetaFromEta( where.pseudoRapidity()+mDeltaEta );
    temp.setPhi(where.phi());
    temp.setTheta( theta1 );
    temp.setMag(where.mag());
    lower.setCorrectedPosition(temp);
    
    temp.setPhi(where.phi());
    temp.setTheta( theta2 );
    temp.setMag(where.mag());
    upper.setCorrectedPosition(temp);
    
    EmcHitVec::iterator first = lower_bound(mHits.begin(), mHits.end(), &lower, EmcHitSorter() );
    EmcHitVec::iterator second = upper_bound(mHits.begin(), mHits.end(), &upper, EmcHitSorter() );

    cout <<"\n searching around intersection eta:\t"<<where.pseudoRapidity()<<"\tphi:\t"<<where.phi()<<endl;
	    
    for (EmcHitVec::iterator it=first; it!=second; ++it) {
		
	EmcHit* hit = *it;
	if (hit->detectorId() != kEndcapEmcTowerId) continue; //projection not valid for endcap...
	fillMap(intersections[1], track, hit);
    }
}


void StEmcTrackMatcher::matchToBarrel(StMuTrack* track)
{
    
    const double radii[3] = { 231.23, 231.23, 231.23};
    StThreeVectorD intersections[3];
    for (int i=0; i<3; ++i) {
	intersections[i] = matchToCylinder(track, radii[i]);
    }
    
    const StThreeVectorD& where = intersections[1];  //smd point.
    if (where.x()==-999.) return; //no intersection with smd, forget it.

    //ok, now look for towers nearby...
    EmcHit lower, upper; //temporary hits for retreival
    StThreeVectorD temp;
    
    double theta1 = gThetaFromEta( where.pseudoRapidity()-mDeltaEta );
    double theta2 = gThetaFromEta( where.pseudoRapidity()+mDeltaEta );
    temp.setPhi(where.phi());
    temp.setTheta( theta1 );
    temp.setMag(where.mag());
    lower.setCorrectedPosition(temp);

    temp.setPhi(where.phi());
    temp.setTheta( theta2 );
    temp.setMag(where.mag());
    upper.setCorrectedPosition(temp);
    
    EmcHitVec::iterator first = lower_bound(mHits.begin(), mHits.end(), &lower, EmcHitSorter() );
    EmcHitVec::iterator second = upper_bound(mHits.begin(), mHits.end(), &upper, EmcHitSorter() );
    
    cout <<"\n searching around intersection eta:\t"<<where.pseudoRapidity()<<"\tphi:\t"<<where.phi()<<endl;
    
    for (EmcHitVec::iterator it=first; it!=second; ++it) {
	
	EmcHit* hit = *it;
	if (hit->detectorId() != kBarrelEmcTowerId) continue; //projection not valid for endcap...
	fillMap(intersections[1], track, hit);
    }
    
}

void StEmcTrackMatcher::fillMap(const StThreeVectorD& smd, StMuTrack* t, EmcHit* hit)
{
    const StThreeVectorD& emcpos = hit->correctedPosition();

    double deta = emcpos.pseudoRapidity() - smd.pseudoRapidity();
    double dphi = niceDeltaPhi( emcpos.phi(), smd.phi() );
    double dr = sqrt(deta*deta + dphi*dphi );

    Projection p;
    p.setTrack(t);
    p.setHit(hit);
    p.setDeltaR(dr);

    TrackToTowerMap::value_type v1(t, p);
    TowerToTrackMap::value_type v2(hit, p);
    
    mTrackToTowerMap.insert(v1);
    mTowerToTrackMap.insert(v2);
}


Int_t StEmcTrackMatcher::Finish()
{
    return kStOk;
}
