#include "StRHICfSimBBC.h"

ClassImp(StRHICfSimBBC)

StRHICfSimBBC::StRHICfSimBBC()
{
    Clear();
}

StRHICfSimBBC::~StRHICfSimBBC()
{
}

void StRHICfSimBBC::Clear(Option_t *option)
{
    fill_n(&mLargeADC[0][0], rSideNum*rBBCLargePmtNum, -999);
    fill_n(&mSmallADC[0][0], rSideNum*rBBCSmallPmtNum, -999);

    for(int pmt=0; pmt<rBBCLargePmtNum; pmt++){
        mEastLargeSimTrkId[pmt].clear();
        mWestLargeSimTrkId[pmt].clear();
    }
    for(int pmt=0; pmt<rBBCSmallPmtNum; pmt++){
        mEastSmallSimTrkId[pmt].clear();
        mWestSmallSimTrkId[pmt].clear();
    }
    
}

void StRHICfSimBBC::SetLargeADC(int dir, int pmt, short adc){mLargeADC[dir][pmt] = adc;}
void StRHICfSimBBC::SetSmallADC(int dir, int pmt, short adc){mSmallADC[dir][pmt] = adc;}

void StRHICfSimBBC::SetLargeSimTrkId(int dir, int pmt, int idx)
{
    if(dir == rEast){mEastLargeSimTrkId[pmt].push_back(idx);}
    if(dir == rWest){mWestLargeSimTrkId[pmt].push_back(idx);}
}

void StRHICfSimBBC::SetSmallSimTrkId(int dir, int pmt, int idx)
{
    if(dir == rEast){mEastSmallSimTrkId[pmt].push_back(idx);}
    if(dir == rWest){mWestSmallSimTrkId[pmt].push_back(idx);}    
}

Int_t StRHICfSimBBC::GetLargeADC(int dir, int pmt){return mLargeADC[dir][pmt];}
Int_t StRHICfSimBBC::GetSmallADC(int dir, int pmt){return mSmallADC[dir][pmt];}

Int_t StRHICfSimBBC::GetLargeSum(int dir)
{
    int adcSum = 0;
    for(int pmt=0; pmt<rBBCLargePmtNum; pmt++){
        adcSum += mLargeADC[dir][pmt];
    }
    return adcSum;
}

Int_t StRHICfSimBBC::GetSmallSum(int dir)
{
    int adcSum = 0;
    for(int pmt=0; pmt<rBBCSmallPmtNum; pmt++){
        adcSum += mSmallADC[dir][pmt];
    }
    return adcSum;
}

Int_t StRHICfSimBBC::GetEastSum(){return GetLargeSum(rEast)+GetSmallSum(rEast);}
Int_t StRHICfSimBBC::GetWestSum(){return GetLargeSum(rWest)+GetSmallSum(rWest);}
Int_t StRHICfSimBBC::GetSum(){return GetEastSum()+GetWestSum();}

Int_t StRHICfSimBBC::GetLargeSimTrkNum(int dir, int pmt)
{
    if(dir == rEast){return mEastLargeSimTrkId[pmt].size();}
    if(dir == rWest){return mWestLargeSimTrkId[pmt].size();}
    return -1;
}

Int_t StRHICfSimBBC::GetSmallSimTrkNum(int dir, int pmt)
{
    if(dir == rEast){return mEastSmallSimTrkId[pmt].size();}
    if(dir == rWest){return mWestSmallSimTrkId[pmt].size();}
    return -1;
}

Int_t StRHICfSimBBC::GetLargeSimTrkId(int dir, int pmt, int idx)
{
    if(dir == rEast){return mEastLargeSimTrkId[pmt][idx];}
    if(dir == rWest){return mWestLargeSimTrkId[pmt][idx];}
    return -1;
}

Int_t StRHICfSimBBC::GetSmallSimTrkId(int dir, int pmt, int idx)
{
    if(dir == rEast){return mEastSmallSimTrkId[pmt][idx];}
    if(dir == rWest){return mWestSmallSimTrkId[pmt][idx];}
    return -1;     
}
