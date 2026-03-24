#include "StRHICfSimEvent.h"

ClassImp(StRHICfSimEvent)

StRHICfSimEvent::StRHICfSimEvent()
{
    Clear();
}

StRHICfSimEvent::~StRHICfSimEvent()
{
}

void StRHICfSimEvent::Clear(Option_t *option)
{
    mEventNumber = -999;
    mProcessId = -999;
    mRHICfRunType = -999;
    mGeneratorIdx = -999;

    mRHICfShowerTrig = false;
    mRHICfType1Pi0Trig = false;
    mRHICfHighEMTrig = false;

    mXParton1 = -999.; 
    mXParton2 = -999.; 
    mXPdf1 = -999.; 
    mXPdf2 = -999.; 
    mQ2Fac = -999.; 
    mQ2Ren = -999.; 
    m_sHat = -999.; 
    m_tHat = -999.; 
    m_uHat = -999.; 
    mPtHat = -999.;
    mDiffAMass = -999.; 
    mDiffBMass = -999.; 
    mDiffCMass = -999.; 
    mDiffAEta = -999.; 
    mDiffBEta = -999.; 
    mDiffCEta = -999.; 
    
    mGenFinalParNum = -999;
    mGenFinalChargedParNum = -999; 
    mPrimaryTrkNum = -999;
}

void StRHICfSimEvent::SetEventNumber(int idx){mEventNumber = idx;}
void StRHICfSimEvent::SetProcessId(int id){mProcessId = id;}
void StRHICfSimEvent::SetRHICfRunType(int type){mRHICfRunType = type;}
void StRHICfSimEvent::SetGeneratorIdx(int idx){mGeneratorIdx = idx;}

void StRHICfSimEvent::SetIsShowerTrigger(){mRHICfShowerTrig = true;}
void StRHICfSimEvent::SetIsType1Pi0Trigger(){mRHICfType1Pi0Trig = true;}
void StRHICfSimEvent::SetIsHighEMTrigger(){mRHICfHighEMTrig = true;}

void StRHICfSimEvent::SetXParton1(double val){mXParton1 = val;}
void StRHICfSimEvent::SetXParton2(double val){mXParton2 = val;}
void StRHICfSimEvent::SetXPdf1(double val){mXPdf1 = val;}
void StRHICfSimEvent::SetXPdf2(double val){mXPdf2 = val;}
void StRHICfSimEvent::SetQ2Fac(double val){mQ2Fac = val;}
void StRHICfSimEvent::SetQ2Renorm(double val){mQ2Ren = val;}
void StRHICfSimEvent::SetsHat(double val){m_sHat = val;}
void StRHICfSimEvent::SettHat(double val){m_tHat = val;}
void StRHICfSimEvent::SetuHat(double val){m_uHat = val;}
void StRHICfSimEvent::SetPtHat(double val){mPtHat = val;}
void StRHICfSimEvent::SetDiffractionAMass(double val){mDiffAMass = val;}
void StRHICfSimEvent::SetDiffractionBMass(double val){mDiffBMass = val;}
void StRHICfSimEvent::SetDiffractionCMass(double val){mDiffCMass = val;}
void StRHICfSimEvent::SetDiffractionAEta(double val){mDiffAEta = val;}
void StRHICfSimEvent::SetDiffractionBEta(double val){mDiffBEta = val;}
void StRHICfSimEvent::SetDiffractionCEta(double val){mDiffCEta = val;}

void StRHICfSimEvent::SetGenFinalParNum(int num){mGenFinalParNum = num;}
void StRHICfSimEvent::SetGenFinalChargedParNum(int num){mGenFinalChargedParNum = num;}
void StRHICfSimEvent::SetPrimaryTrkNum(int num){mPrimaryTrkNum = num;}

Int_t StRHICfSimEvent::GetEventNumber(){return mEventNumber;}
Int_t StRHICfSimEvent::GetProcessId(){return mProcessId;}
Int_t StRHICfSimEvent::GetRHICfRunType(){return mRHICfRunType;}
Int_t StRHICfSimEvent::GetGeneratorIdx(){return mGeneratorIdx;}

Bool_t StRHICfSimEvent::IsShowerTrigger(){return mRHICfShowerTrig;}
Bool_t StRHICfSimEvent::IsType1Pi0Trigger(){return mRHICfType1Pi0Trig;}
Bool_t StRHICfSimEvent::IsHighEMTrigger(){return mRHICfHighEMTrig;}

Double_t StRHICfSimEvent::GetXParton1(){return mXParton1;}
Double_t StRHICfSimEvent::GetXParton2(){return mXParton2;}
Double_t StRHICfSimEvent::GetXPdf1(){return mXPdf1;}
Double_t StRHICfSimEvent::GetXPdf2(){return mXPdf2;}
Double_t StRHICfSimEvent::GetQ2Fac(){return mQ2Fac;}
Double_t StRHICfSimEvent::GetQ2Renorm(){return mQ2Ren;}
Double_t StRHICfSimEvent::GetsHat(){return m_sHat;}
Double_t StRHICfSimEvent::GettHat(){return m_tHat;}
Double_t StRHICfSimEvent::GetuHat(){return m_uHat;}
Double_t StRHICfSimEvent::GetPtHat(){return mPtHat;}
Double_t StRHICfSimEvent::GetDiffractionAMass(){return mDiffAMass;}
Double_t StRHICfSimEvent::GetDiffractionBMass(){return mDiffBMass;}
Double_t StRHICfSimEvent::GetDiffractionCMass(){return mDiffCMass;}
Double_t StRHICfSimEvent::GetDiffractionAEta(){return mDiffAEta;}
Double_t StRHICfSimEvent::GetDiffractionBEta(){return mDiffBEta;}
Double_t StRHICfSimEvent::GetDiffractionCEta(){return mDiffCEta;}

Int_t StRHICfSimEvent::GetGenFinalParNum(){return mGenFinalParNum;}
Int_t StRHICfSimEvent::GetGenFinalChargedParNum(){return mGenFinalChargedParNum;}
Int_t StRHICfSimEvent::GetPrimaryTrkNum(){return mPrimaryTrkNum;}