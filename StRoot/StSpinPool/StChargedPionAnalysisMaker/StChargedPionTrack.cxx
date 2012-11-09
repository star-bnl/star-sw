// $Id: StChargedPionTrack.cxx,v 1.6 2012/11/09 03:31:34 perev Exp $

#include "StChargedPionTrack.h"
#include "StPionPlus.hh"

#include "TClass.h"

ClassImp(StChargedPionTrack)

StChargedPionTrack::StChargedPionTrack() : TLorentzVector(),
    mId(0),mFlag(0),mVertexIndex(0),mVertex(0.,0.,0.),mNHits(0),mNHitsPoss(0),mNHitsDedx(0),mNHitsFit(0),
    mNSigmaElectron(0),mNSigmaPion(0),mNSigmaKaon(0),mNSigmaProton(0),mdEdx(0),mChi2(0),mChi2Prob(0),
    mCharge(0),mB(0.),mGlobalLastPoint(0.,0.,0.),
    mGlobalHelix(StThreeVectorF(0.,0.,0.),StThreeVectorF(0.,0.,0.),0.,0.)
{ }

StChargedPionTrack::~StChargedPionTrack() { /* no-op */ }

StChargedPionTrack::StChargedPionTrack(const StChargedPionTrack& t) : TLorentzVector(t)
{    
    this->mId               = t.mId;
    this->mFlag             = t.mFlag;
    
    this->mVertexIndex      = t.mVertexIndex;
    this->mVertex           = t.mVertex;
    
    this->mNHits            = t.mNHits;
    this->mNHitsPoss        = t.mNHitsPoss;
    this->mNHitsDedx        = t.mNHitsDedx;
    this->mNHitsFit         = t.mNHitsFit;
    
    this->mNSigmaElectron   = t.mNSigmaElectron;
    this->mNSigmaPion       = t.mNSigmaPion;
    this->mNSigmaKaon       = t.mNSigmaKaon;
    this->mNSigmaProton     = t.mNSigmaProton;
    this->mdEdx             = t.mdEdx;
    
    this->mChi2             = t.mChi2;
    this->mChi2Prob         = t.mChi2Prob;
    
    this->mCharge           = t.mCharge;
    this->mB                = t.mB;
    
    this->mGlobalLastPoint  = t.mGlobalLastPoint;
    this->mGlobalHelix      = t.mGlobalHelix;
}

void StChargedPionTrack::setPtEtaPhi(float aPt, float aEta, float aPhi) {
    this->SetPtEtaPhiM(aPt,aEta,aPhi,StPionPlus::instance()->mass());
}

void StChargedPionTrack::setPt(float aPt) {
    this->SetPerp(aPt);
}

void StChargedPionTrack::setPhi(float aPhi) {
    this->SetPhi(aPhi);
}

void StChargedPionTrack::setEta(float aEta) {
    this->SetTheta(2*TMath::ATan(TMath::Exp(-1*aEta)));
}

double StChargedPionTrack::globalPt() const {
    return this->globalPt(mGlobalHelix.origin());
}

double StChargedPionTrack::globalPt(StThreeVectorF position) const {
    double pathLength = mGlobalHelix.pathLength(position,false);
    StThreeVector<double> gP = mGlobalHelix.momentumAt(pathLength,mB);
    return gP.perp();
}

double StChargedPionTrack::globalPhi() const {
    return this->globalPhi(mGlobalHelix.origin());
}

double StChargedPionTrack::globalPhi(StThreeVectorF position) const {
    double pathLength = mGlobalHelix.pathLength(position,false);
    StThreeVector<double> gP = mGlobalHelix.momentumAt(pathLength,mB);
    return gP.phi();
}

double StChargedPionTrack::globalEta() const {
    return this->globalEta(mGlobalHelix.origin());
}

double StChargedPionTrack::globalEta(StThreeVectorF position) const {
    double pathLength = mGlobalHelix.pathLength(position,false);
    StThreeVector<double> gP = mGlobalHelix.momentumAt(pathLength,mB);
    return gP.pseudoRapidity();
}

TLorentzVector StChargedPionTrack::globalP() const {
    return this->globalP(mGlobalHelix.origin());
}

TLorentzVector StChargedPionTrack::globalP(StThreeVectorF position) const {
    double pathLength = mGlobalHelix.pathLength(position,false);
    StThreeVector<double> gP = mGlobalHelix.momentumAt(pathLength,mB);
    TLorentzVector vec;
    vec.SetPtEtaPhiM(gP.perp(),gP.pseudoRapidity(),gP.phi(),StPionPlus::instance()->mass());
    return vec;
}

StThreeVectorF StChargedPionTrack::globalDca() const {
    return this->globalDca(mVertex);
}

StThreeVectorF StChargedPionTrack::globalDca(StThreeVectorF position) const {
    double pathlength = mGlobalHelix.pathLength(position,false);
    return (mGlobalHelix.at(pathlength) - position);
}

double StChargedPionTrack::length() const 
{ 
    double end      = mGlobalHelix.pathLength(StThreeVectorD(mGlobalLastPoint));
    double begin    = mGlobalHelix.pathLength(StThreeVectorD(mVertex));
    return fabs(end - begin); 
}

double StChargedPionTrack::lengthMeasured() const 
{ 
    double end      = mGlobalHelix.pathLength(StThreeVectorD(mGlobalLastPoint));
    return fabs(end); 
}

/*****************************************************************************
 * $Log: StChargedPionTrack.cxx,v $
 * Revision 1.6  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.5  2008/12/29 15:58:31  kocolosk
 * removed commented code and added $Id: StChargedPionTrack.cxx,v 1.6 2012/11/09 03:31:34 perev Exp $/$Log: StChargedPionTrack.cxx,v $
 * removed commented code and added $Id$/Revision 1.6  2012/11/09 03:31:34  perev
 * removed commented code and added $Id$/Cleanup
 * removed commented code and added $Id$/ as needed
 *
 *****************************************************************************/

