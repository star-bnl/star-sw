/*****************************************
 *
 * $Id: StTrackPairInfo.hh,v 1.10 2015/03/13 00:20:12 perev Exp $
 *
 * $Log: StTrackPairInfo.hh,v $
 * Revision 1.10  2015/03/13 00:20:12  perev
 * Upload StMcIst Amilkar
 *
 * Revision 1.9  2011/04/01 19:40:07  perev
 * const++
 *
 * Revision 1.8  2010/06/22 22:06:33  fine
 * roll back the previous version to restore the nightly builds
 *
 * Revision 1.6  2005/11/22 21:44:16  fisyak
 * Add Ssd to Associator, add IdTruth options for Svt and Ssd
 *
 * Revision 1.5  2001/03/02 22:41:55  calderon
 * Envelop header file in #ifndef's.
 *
 * Revision 1.4  1999/12/14 07:07:41  calderon
 * Added Ratio Number of Common Hits / Number of Reconstructed Hits for
 * each detector.
 * Numbering scheme from StEvent & StMcEvent as per SVT request
 * Added Kink, V0 and Xi vertex associations.
 *
 * Revision 1.3  1999/12/08 00:00:25  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   SSD Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
 * Revision 1.2  1999/09/23 21:25:24  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *****************************************/
#ifndef StTrackPairInfo_hh
#define StTrackPairInfo_hh
#include "Stiostream.h"

class StMcTrack;
class StGlobalTrack;

class StTrackPairInfo {

public:
  StTrackPairInfo(const StGlobalTrack* rcTrk,
		  const StMcTrack*     mcTrk,
		  unsigned int tpcPings,
		  unsigned int svtPings,
		  unsigned int ssdPings,
		  unsigned int istPings,   //Amilkar
		  unsigned int pxlPings,   //Amilkar
		  unsigned int ftpcPings);
  virtual ~StTrackPairInfo();    
  
  const StMcTrack* partnerMcTrack() const;
  const StGlobalTrack* partnerTrack() const;
  
  unsigned int commonTpcHits() const;
  unsigned int commonSvtHits() const;
  unsigned int commonSsdHits() const;
  unsigned int commonIstHits() const;    //Amilkar
  unsigned int commonPxlHits() const;    //Amilkar
  unsigned int commonFtpcHits() const;
  
  float percentOfPairedTpcHits() const;
  float percentOfPairedSvtHits() const;
  float percentOfPairedSsdHits() const;
  float percentOfPairedIstHits() const;   //Amilkar
  float percentOfPairedPxlHits() const;   //Amilkar
  float percentOfPairedFtpcHits() const;
  
    void setPartnerMcTrack(const StMcTrack*);
    void setPartnerTrack(const StGlobalTrack*);
    
    void setCommonTpcHits(unsigned int);
    void setCommonSvtHits(unsigned int);
    void setCommonSsdHits(unsigned int);
  void setCommonIstHits(unsigned int);    //Amilkar
  void setCommonPxlHits(unsigned int);    //Amilkar
    void setCommonFtpcHits(unsigned int);
private:
    const StGlobalTrack*  mPartnerTrack;
    const StMcTrack*      mPartnerMcTrack;
    unsigned int    mCommonTpcHits;
    unsigned int    mCommonSvtHits;
    unsigned int    mCommonSsdHits;
  unsigned int    mCommonIstHits;    //Amilkar
  unsigned int    mCommonPxlHits;    //Amilkar
    unsigned int    mCommonFtpcHits;
    float           mRatioCommonToTotalHitsTpc;
    float           mRatioCommonToTotalHitsSvt;
    float           mRatioCommonToTotalHitsSsd;
  float           mRatioCommonToTotalHitsIst;      //Amilkar
  float           mRatioCommonToTotalHitsPxl;      //Amilkar
    float           mRatioCommonToTotalHitsFtpc;
};

inline const StMcTrack* StTrackPairInfo::partnerMcTrack() const { return mPartnerMcTrack; }

inline const StGlobalTrack* StTrackPairInfo::partnerTrack() const { return mPartnerTrack; }

inline unsigned int StTrackPairInfo::commonTpcHits() const { return mCommonTpcHits; }

inline unsigned int StTrackPairInfo::commonSvtHits() const { return mCommonSvtHits; }
inline unsigned int StTrackPairInfo::commonSsdHits() const { return mCommonSsdHits; }
inline unsigned int StTrackPairInfo::commonIstHits() const { return mCommonIstHits; }     //Amilkar
inline unsigned int StTrackPairInfo::commonPxlHits() const { return mCommonPxlHits; }     //Amilkar

inline unsigned int StTrackPairInfo::commonFtpcHits() const { return mCommonFtpcHits; }

inline float StTrackPairInfo::percentOfPairedTpcHits() const { return mRatioCommonToTotalHitsTpc; }

inline float StTrackPairInfo::percentOfPairedSvtHits() const { return mRatioCommonToTotalHitsSvt; }
inline float StTrackPairInfo::percentOfPairedSsdHits() const { return mRatioCommonToTotalHitsSsd; }
inline float StTrackPairInfo::percentOfPairedIstHits() const { return mRatioCommonToTotalHitsIst; }   //Amilkar
inline float StTrackPairInfo::percentOfPairedPxlHits() const { return mRatioCommonToTotalHitsPxl; }   //Amilkar

inline float StTrackPairInfo::percentOfPairedFtpcHits() const { return mRatioCommonToTotalHitsFtpc; }
ostream&  operator<<(ostream& os, const StTrackPairInfo& v);
#endif
