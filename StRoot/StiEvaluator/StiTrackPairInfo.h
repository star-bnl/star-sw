//StiTrackPairInfo.h
//M.L. Miller (Yale Software)
//12/01

/*! \class StiTrackPairInfo
  StiTrackPairInfo is a mirror of class StTrackPairInfo.  Whereas StTrackPairInfo (used by
  StAssociationMaker) maps an StiMcTrack to a StGlobalTrack, StiTrackPairInfo (used by
  StiEventAssociator) maps an StiMcTrack to a StiTrack.  That is the only difference.

  \author M.L. Miller (Yale Software)
*/

#ifndef StiTrackPairInfo_HH
#define StiTrackPairInfo_HH

//For trackPing
#include "StAssociationMaker/StAssociationMaker.h"

class StMcTrack;
class StiTrack;

class StiTrackPairInfo {

public:
    StiTrackPairInfo();
    StiTrackPairInfo(const StiTrack*, const trackPing&);
    virtual ~StiTrackPairInfo();

    const StMcTrack* partnerMcTrack() const;
    const StiTrack* partnerTrack() const;

    unsigned int commonTpcHits() const;
    unsigned int commonSvtHits() const;
    unsigned int commonFtpcHits() const;

    void setPartnerMcTrack(const StMcTrack*);
    void setPartnerTrack(const StiTrack*);
    
    void setCommonTpcHits(unsigned int);
    void setCommonSvtHits(unsigned int);
    void setCommonFtpcHits(unsigned int);
    
private:
    const StiTrack*  mPartnerTrack;
    const StMcTrack*      mPartnerMcTrack;
    unsigned int    mCommonTpcHits;
    unsigned int    mCommonSvtHits;
    unsigned int    mCommonFtpcHits;
};

inline void StiTrackPairInfo::setPartnerMcTrack(const StMcTrack* val)
{
    mPartnerMcTrack = val;
}

inline const StMcTrack* StiTrackPairInfo::partnerMcTrack() const
{
    return mPartnerMcTrack;
}

inline void StiTrackPairInfo::setPartnerTrack(const StiTrack* val)
{
    mPartnerTrack = val;
}

inline const StiTrack* StiTrackPairInfo::partnerTrack() const
{
    return mPartnerTrack;
}

inline void StiTrackPairInfo::setCommonTpcHits(unsigned int val)
{
    mCommonTpcHits = val;
}


inline unsigned int StiTrackPairInfo::commonTpcHits() const
{
    return mCommonTpcHits;
}

inline void StiTrackPairInfo::setCommonSvtHits(unsigned int val)
{
    mCommonSvtHits = val;
}

inline unsigned int StiTrackPairInfo::commonSvtHits() const
{
    return mCommonSvtHits;
}

inline void StiTrackPairInfo::setCommonFtpcHits(unsigned int val)
{
    mCommonFtpcHits = val;
}

inline unsigned int StiTrackPairInfo::commonFtpcHits() const
{
    return mCommonFtpcHits;
}

#endif
