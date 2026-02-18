#include "StRHICfSimTrack.h"

ClassImp(StRHICfSimTrack)

StRHICfSimTrack::StRHICfSimTrack()
{
    Clear();
}

StRHICfSimTrack::~StRHICfSimTrack()
{
}

void StRHICfSimTrack::Clear(Option_t *option)
{
    mIsPrimary = false;
    mIsSimPropagate = false;
    mIsFinal = false;
    mIsRHICfHit = false;
    mId = -999;
    mPid = -999;
    mParentTrkId = -999;
    mDaughterNum = -999;
    mEnergy = -999.; 
    fill_n(&mMom[0], 3, -999);
    fill_n(&mVertexStart[0], 3, -999);
    fill_n(&mVertexEnd[0], 3, -999);
}

void StRHICfSimTrack::SetIsPrimary(){mIsPrimary = true;}
void StRHICfSimTrack::SetIsSimPropagate(){mIsSimPropagate = true;}
void StRHICfSimTrack::SetIsFinal(){mIsFinal = true;}
void StRHICfSimTrack::SetIsRHICfHit(){mIsRHICfHit = true;}
void StRHICfSimTrack::SetId(int id){mId = id;}
void StRHICfSimTrack::SetPid(int pid){mPid = pid;}
void StRHICfSimTrack::SetDaughterNum(int num){mDaughterNum = num;}
void StRHICfSimTrack::SetParentId(int id){mParentTrkId = id;}
void StRHICfSimTrack::SetEnergy(float energy){mEnergy = energy;}

void StRHICfSimTrack::SetMomentum(float px, float py, float pz)
{
    mMom[0] = px;
    mMom[1] = py;
    mMom[2] = pz;
}

void StRHICfSimTrack::SetVertexStart(float x, float y, float z)
{
    mVertexStart[0] = x;
    mVertexStart[1] = y;
    mVertexStart[2] = z;
}

void StRHICfSimTrack::SetVertexEnd(float x, float y, float z)
{
    mVertexEnd[0] = x;
    mVertexEnd[1] = y;
    mVertexEnd[2] = z;
}

Bool_t StRHICfSimTrack::IsPrimary(){return mIsPrimary;}
Bool_t StRHICfSimTrack::IsSimPropagate(){return mIsSimPropagate;}
Bool_t StRHICfSimTrack::IsFinal(){return mIsFinal;}
Bool_t StRHICfSimTrack::IsRHICfHit(){return mIsRHICfHit;}
Int_t StRHICfSimTrack::GetId(){return mId;}
Int_t StRHICfSimTrack::GetPid(){return mPid;}
Int_t StRHICfSimTrack::GetParentId(){return mParentTrkId;}
Int_t StRHICfSimTrack::GetDaughterNum(){return mDaughterNum;}
Double_t StRHICfSimTrack::GetE(){return mEnergy;}
Double_t StRHICfSimTrack::GetPx(){return mMom[0];}
Double_t StRHICfSimTrack::GetPy(){return mMom[1];}
Double_t StRHICfSimTrack::GetPz(){return mMom[2];}
Double_t StRHICfSimTrack::GetPt(){return sqrt(mMom[0]*mMom[0] + mMom[1]*mMom[1]);}

Double_t StRHICfSimTrack::GetEta()
{
    Double_t p = sqrt(mMom[0]*mMom[0] + mMom[1]*mMom[1] + mMom[2]*mMom[2]);
    Double_t cosTheta = (p == 0.)? 1. : mMom[2]/p;
    return -0.5*TMath::Log((1. - cosTheta)/(1. + cosTheta));
}

Double_t StRHICfSimTrack::GetVxStart(){return mVertexStart[0];}
Double_t StRHICfSimTrack::GetVyStart(){return mVertexStart[1];}
Double_t StRHICfSimTrack::GetVzStart(){return mVertexStart[2];}
Double_t StRHICfSimTrack::GetVxEnd(){return mVertexEnd[0];}
Double_t StRHICfSimTrack::GetVyEnd(){return mVertexEnd[1];}
Double_t StRHICfSimTrack::GetVzEnd(){return mVertexEnd[2];}