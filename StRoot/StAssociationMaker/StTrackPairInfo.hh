/*****************************************
 *
 * $Id: StTrackPairInfo.hh,v 1.2 1999/09/23 21:25:24 calderon Exp $
 *
 * $Log: StTrackPairInfo.hh,v $
 * Revision 1.2  1999/09/23 21:25:24  calderon
 * Added Log & Id
 * Modified includes according to Yuri
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
