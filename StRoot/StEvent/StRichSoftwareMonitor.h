/***************************************************************************
 *
 * $Id: StRichSoftwareMonitor.h,v 2.2 2000/09/28 10:57:55 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichSoftwareMonitor.h,v $
 * Revision 2.2  2000/09/28 10:57:55  ullrich
 * Major update.
 *
 * Revision 2.1  1999/10/13 19:43:38  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichSoftwareMonitor_hh
#define StRichSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_rich_st;

class StRichSoftwareMonitor : public StObject {
public:
    StRichSoftwareMonitor();
    StRichSoftwareMonitor(const dst_mon_soft_rich_st&);
    // StRichSoftwareMonitor(const StRichSoftwareMonitor&);            use default
    // StRichSoftwareMonitor& operator=(const StRichSoftwareMonitor&); use default
    ~StRichSoftwareMonitor();

    // ClusterFinder

    void    setNumberOfPixels(Long_t);
    void    setNumberOfClusters(Long_t);
    void    setNumberOfHits(Long_t);
    void    setTotalCharge(Long_t);
    
    Long_t  numberOfPixels() const;
    Long_t  numberOfClusters() const;
    Long_t  numberOfHits() const;
    Long_t  totalCharge() const;

    // PID Maker
    void   setNumberOfTracksCrossing(Long_t);
    void   setNumberOfTracksPidCalculated(Long_t);
    void   setNumberOfTracksAbove1Gev(Long_t);
    void   setNumberOfHitsInRings(Long_t);
    void   setNumberOfRings(Long_t);
    
    Long_t  numberOfTracksCrossing() const;
    Long_t  numberOfTracksPidCalculated() const;
    Long_t  numberOfTracksAbove1Gev() const;
    Long_t  numberOfHitsInRings() const;
    
    Long_t  mult_rich_tot;

private:
    Long_t mNumberOfPixels;
    Long_t mNumberOfClusters;
    Long_t mNumberOfHits;
    Long_t mTotalCharge;

    Long_t  mNumberOfTracksCrossing;
    Long_t  mNumberOfTracksPidCalculated;
    Long_t  mNumberOfTracksAbove1Gev;
    Long_t  mNumberOfHitsInRings;
    Long_t  mNumberOfRings;
    
    ClassDef(StRichSoftwareMonitor,1)
};

inline void StRichSoftwareMonitor::setNumberOfPixels(Long_t n) { mNumberOfPixels = n;}
inline void StRichSoftwareMonitor::setNumberOfClusters(Long_t n) { mNumberOfClusters= n;}
inline void StRichSoftwareMonitor::setNumberOfHits(Long_t n){ mNumberOfHits = n;}
inline void StRichSoftwareMonitor::setTotalCharge(Long_t n){ mTotalCharge = n;}
    
inline Long_t StRichSoftwareMonitor::numberOfPixels() const {return mNumberOfPixels;}
inline Long_t StRichSoftwareMonitor::numberOfClusters() const {return mNumberOfClusters;}
inline Long_t StRichSoftwareMonitor::numberOfHits() const {return mNumberOfHits;}
inline Long_t StRichSoftwareMonitor::totalCharge() const {return mTotalCharge;}

    // PID Maker
inline void StRichSoftwareMonitor::setNumberOfTracksCrossing(Long_t n) { mNumberOfTracksCrossing = n;}
inline void StRichSoftwareMonitor::setNumberOfTracksPidCalculated(Long_t n) { mNumberOfTracksPidCalculated = n;}
inline void StRichSoftwareMonitor::setNumberOfTracksAbove1Gev(Long_t n) { mNumberOfTracksAbove1Gev= n;}
inline void StRichSoftwareMonitor::setNumberOfHitsInRings(Long_t n) { mNumberOfHitsInRings = n;}
inline void StRichSoftwareMonitor::setNumberOfRings(Long_t n) { mNumberOfRings = n;}
    
inline Long_t StRichSoftwareMonitor::numberOfTracksCrossing() const {return mNumberOfTracksCrossing;}
inline Long_t StRichSoftwareMonitor::numberOfTracksPidCalculated() const {return mNumberOfTracksPidCalculated;}
inline Long_t StRichSoftwareMonitor::numberOfTracksAbove1Gev() const {return mNumberOfTracksAbove1Gev;}
inline Long_t StRichSoftwareMonitor::numberOfHitsInRings() const {return mNumberOfHitsInRings;}
#endif
