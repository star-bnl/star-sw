/*****************************************
 *
 * $Id: StTrackPairInfo.hh,v 1.3 1999/12/08 00:00:25 calderon Exp $
 *
 * $Log: StTrackPairInfo.hh,v $
 * Revision 1.3  1999/12/08 00:00:25  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
 * Revision 1.2  1999/09/23 21:25:24  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *****************************************/

class StMcTrack;
class StGlobalTrack;

class StTrackPairInfo {

public:
    StTrackPairInfo(StGlobalTrack*, StMcTrack*, unsigned int, unsigned int, unsigned int);
    virtual ~StTrackPairInfo();    

    StMcTrack* partnerMcTrack() const;
    StGlobalTrack* partnerTrack() const;

    unsigned int commonTpcHits() const;
    unsigned int commonSvtHits() const;
    unsigned int commonFtpcHits() const;
    
    void setPartnerMcTrack(StMcTrack*);
    void setPartnerTrack(StGlobalTrack*);
    
    void setCommonTpcHits(unsigned int);
    void setCommonSvtHits(unsigned int);
    void setCommonFtpcHits(unsigned int);
private:
    StGlobalTrack*  mPartnerTrack;
    StMcTrack*      mPartnerMcTrack;
    unsigned int    mCommonTpcHits;
    unsigned int    mCommonSvtHits;
    unsigned int    mCommonFtpcHits;
};

inline StMcTrack* StTrackPairInfo::partnerMcTrack() const { return mPartnerMcTrack; }

inline StGlobalTrack* StTrackPairInfo::partnerTrack() const { return mPartnerTrack; }

inline unsigned int StTrackPairInfo::commonTpcHits() const { return mCommonTpcHits; }

inline unsigned int StTrackPairInfo::commonSvtHits() const { return mCommonSvtHits; }

inline unsigned int StTrackPairInfo::commonFtpcHits() const { return mCommonFtpcHits; }
