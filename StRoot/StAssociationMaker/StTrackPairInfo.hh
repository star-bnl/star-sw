/*****************************************
 *
 * StTrackPairInfo.hh
 *
 *****************************************/

class StMcTrack;

class StTrackPairInfo {

public:
    StTrackPairInfo(StMcTrack*, unsigned int);
    virtual ~StTrackPairInfo();    

    StMcTrack* partnerMcTrack() const;
    unsigned int commonHits() const;
    
    void setPartnerMcTrack(StMcTrack*);
    void setCommonHits(unsigned int);

private:
    StMcTrack* mPartnerMcTrack;
    unsigned int mCommonHits;

};

inline StMcTrack* StTrackPairInfo::partnerMcTrack() const { return mPartnerMcTrack; }

inline unsigned int StTrackPairInfo::commonHits() const { return mCommonHits; }
