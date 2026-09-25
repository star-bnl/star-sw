#include "StRHICfSimZDC.h"

ClassImp(StRHICfSimZDC)

StRHICfSimZDC::StRHICfSimZDC()
{
    Clear();
}

StRHICfSimZDC::~StRHICfSimZDC()
{
}

void StRHICfSimZDC::Clear(Option_t *option)
{
    fill_n(&mPhotonNum[0], rZDCPMTNum, -1);
    fill_n(&mPmtdE[0], rZDCPMTNum, -999.);
    fill_n(&mSMDXdE[0], rSMDXNum, -999.);
    fill_n(&mSMDYdE[0], rSMDYNum, -999.);

    mSimTrkId.clear();
    mSimTrkIncidentPosX.clear();
    mSimTrkIncidentPosY.clear();
    mSimTrkIncidentE.clear();
}

void StRHICfSimZDC::SetPmtPhotonNum(int idx, int val){mPhotonNum[idx] = val;}
void StRHICfSimZDC::SetPmtdE(int idx, float val){mPmtdE[idx] = val;}

void StRHICfSimZDC::SetSMDdE(int xy, int idx, float val)
{
    if(xy == 0){mSMDXdE[idx] = val;}
    if(xy == 1){mSMDYdE[idx] = val;}
}

void StRHICfSimZDC::SetSimTrkId(int id, double incidentPosX, double incidentPosY, double incidentEnergy)
{
    mSimTrkId.push_back(id);
    mSimTrkIncidentPosX.push_back(incidentPosX);
    mSimTrkIncidentPosY.push_back(incidentPosY);
    mSimTrkIncidentE.push_back(incidentEnergy);
}

Int_t StRHICfSimZDC::GetPmtPhotonNum(int idx){return mPhotonNum[idx];}
Float_t StRHICfSimZDC::GetPmtdE(int idx){return mPmtdE[idx];}

Float_t StRHICfSimZDC::GetSMDdE(int xy, int idx)
{
    if(xy == 0){return mSMDXdE[idx];}
    if(xy == 1){return mSMDYdE[idx];}
    return -999.;
}

Int_t StRHICfSimZDC::GetSimTrkNum(){return mSimTrkId.size();}
Int_t StRHICfSimZDC::GetSimTrkId(int idx){return mSimTrkId[idx];}
Double_t StRHICfSimZDC::GetSimTrkIncidentPos(int idx, int xy)
{
    if(xy == 0){return mSimTrkIncidentPosX[idx];}
    if(xy == 1){return mSimTrkIncidentPosY[idx];}
    return -999.;
}
Double_t StRHICfSimZDC::GetSimTrkIncidentEnergy(int idx){return mSimTrkIncidentE[idx];}
