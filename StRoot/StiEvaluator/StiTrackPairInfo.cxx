//StiTrackPairInfo.cxx
//M.L. Miller (Yale Software)
//12/01

//StiEvaluator
#include "StiTrackPairInfo.h"

StiTrackPairInfo::StiTrackPairInfo()
    : mPartnerTrack(0), mPartnerMcTrack(0), mCommonTpcHits(0), mCommonSvtHits(0), mCommonFtpcHits(0)
{
}

StiTrackPairInfo::StiTrackPairInfo(const StiTrack* rcTrk, const trackPing& ping)
    : mPartnerTrack(rcTrk), mPartnerMcTrack(ping.mcTrack), mCommonTpcHits(ping.nPingsTpc),
      mCommonSvtHits(ping.nPingsSvt),mCommonFtpcHits(ping.nPingsFtpc)
{
}

StiTrackPairInfo::~StiTrackPairInfo()
{
}

