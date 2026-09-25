#include "RHICfTrackInfo.hh"

RHICfTrackInfo::RHICfTrackInfo(int primaryID)
{
    fPrimaryID = primaryID;
}

RHICfTrackInfo::~RHICfTrackInfo()
{
}

void RHICfTrackInfo::SetPrimaryID(int id){fPrimaryID = id;}
int RHICfTrackInfo::GetPrimaryID(){return fPrimaryID;}