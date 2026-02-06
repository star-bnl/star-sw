#include "StRHICfSimBTof.h"

ClassImp(StRHICfSimBTof)

StRHICfSimBTof::StRHICfSimBTof()
{
    Clear();
}

StRHICfSimBTof::~StRHICfSimBTof()
{
}

void StRHICfSimBTof::Clear(Option_t *option)
{
    mIsVPD = false;
    mTray = 0;
    mModule = 0;
    mCell = 0;
    mSimTrkId = -999;

}

void StRHICfSimBTof::SetSimBTof(bool isVPD, int tray, int module, int cell, int simTrkId)
{
    mIsVPD = isVPD;
    mTray = tray;
    mModule = module;
    mCell = cell;
    mSimTrkId = simTrkId;
}

void StRHICfSimBTof::SetIsVPD(){mIsVPD = true;}
void StRHICfSimBTof::SetTray(int idx){mTray = idx;}
void StRHICfSimBTof::SetModule(int idx){mModule = idx;}
void StRHICfSimBTof::SetCell(int idx){mCell = idx;}
void StRHICfSimBTof::SetSimTrkId(int idx){mSimTrkId = idx;}

Bool_t StRHICfSimBTof::IsVPD(){return mIsVPD;}
Int_t StRHICfSimBTof::GetTray(){return mTray;}
Int_t StRHICfSimBTof::GetModule(){return mModule;}
Int_t StRHICfSimBTof::GetCell(){return mCell;}
Int_t StRHICfSimBTof::GetGlobalCell(){return (mTray-1)*192 + (mModule-1)*6 + (mCell-1);}
Int_t StRHICfSimBTof::GetSimTrkId(){return mSimTrkId;}

