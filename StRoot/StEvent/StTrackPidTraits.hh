/***************************************************************************
 *
 * $Id: StTrackPidTraits.hh,v 1.3 1999/04/08 15:01:18 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.hh,v $
 * Revision 1.3  1999/04/08 15:01:18  ullrich
 * Added dE/dx PID from TPC.
 *
 * Revision 1.2  1999/01/15 22:54:10  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StTrackPidTraits_hh
#define StTrackPidTraits_hh

class StGlobalTrack;
class StDedxPid;

class StTrackPidTraits {
public:
    StTrackPidTraits(const StGlobalTrack&);
    ~StTrackPidTraits();

    const StDedxPid* tpcDedxPid() const;
    
private:
    StDedxPid *mTpcDedxPid;
    // StDedxPid *mSvtDedxPid;  
};

inline const StDedxPid* StTrackPidTraits::tpcDedxPid() const {return mTpcDedxPid;}

#endif
