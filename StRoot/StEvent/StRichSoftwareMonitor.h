/*!
 * \class StRichSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StRichSoftwareMonitor.h,v 2.5 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichSoftwareMonitor.h,v $
 * Revision 2.5  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.4  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:41  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
#ifndef DST_MON_SOFT_RICH_H
#define DST_MON_SOFT_RICH_H
struct dst_mon_soft_rich_st {
  int mult_rich_tot; /* Total mult. (or ADC sum) in RICH      */
};
#endif /* DST_MON_SOFT_RICH_H */


class StRichSoftwareMonitor : public StObject {
public:
    StRichSoftwareMonitor();
    StRichSoftwareMonitor(const dst_mon_soft_rich_st&);
    // StRichSoftwareMonitor(const StRichSoftwareMonitor&);            use default
    // StRichSoftwareMonitor& operator=(const StRichSoftwareMonitor&); use default
    ~StRichSoftwareMonitor();

    // ClusterFinder

    void   setNumberOfPixels(int);
    void   setNumberOfClusters(int);
    void   setNumberOfHits(int);
    void   setTotalCharge(int);
    
    int    numberOfPixels() const;
    int    numberOfClusters() const;
    int    numberOfHits() const;
    int    totalCharge() const;

    // PID Maker
    void   setNumberOfTracksCrossing(int);
    void   setNumberOfTracksPidCalculated(int);
    void   setNumberOfTracksAbove1Gev(int);
    void   setNumberOfHitsInRings(int);
    void   setNumberOfRings(int);
    
    int    numberOfTracksCrossing() const;
    int    numberOfTracksPidCalculated() const;
    int    numberOfTracksAbove1Gev() const;
    int    numberOfHitsInRings() const;
    
    int    mult_rich_tot;

private:
    Int_t  mNumberOfPixels;
    Int_t  mNumberOfClusters;
    Int_t  mNumberOfHits;
    Int_t  mTotalCharge;

    Int_t  mNumberOfTracksCrossing;
    Int_t  mNumberOfTracksPidCalculated;
    Int_t  mNumberOfTracksAbove1Gev;
    Int_t  mNumberOfHitsInRings;
    Int_t  mNumberOfRings;
    
    ClassDef(StRichSoftwareMonitor,1)
};

inline void StRichSoftwareMonitor::setNumberOfPixels(int n) { mNumberOfPixels = n;}
inline void StRichSoftwareMonitor::setNumberOfClusters(int n) { mNumberOfClusters= n;}
inline void StRichSoftwareMonitor::setNumberOfHits(int n){ mNumberOfHits = n;}
inline void StRichSoftwareMonitor::setTotalCharge(int n){ mTotalCharge = n;}
    
inline int StRichSoftwareMonitor::numberOfPixels() const {return mNumberOfPixels;}
inline int StRichSoftwareMonitor::numberOfClusters() const {return mNumberOfClusters;}
inline int StRichSoftwareMonitor::numberOfHits() const {return mNumberOfHits;}
inline int StRichSoftwareMonitor::totalCharge() const {return mTotalCharge;}

    // PID Maker
inline void StRichSoftwareMonitor::setNumberOfTracksCrossing(int n) { mNumberOfTracksCrossing = n;}
inline void StRichSoftwareMonitor::setNumberOfTracksPidCalculated(int n) { mNumberOfTracksPidCalculated = n;}
inline void StRichSoftwareMonitor::setNumberOfTracksAbove1Gev(int n) { mNumberOfTracksAbove1Gev= n;}
inline void StRichSoftwareMonitor::setNumberOfHitsInRings(int n) { mNumberOfHitsInRings = n;}
inline void StRichSoftwareMonitor::setNumberOfRings(int n) { mNumberOfRings = n;}
    
inline int StRichSoftwareMonitor::numberOfTracksCrossing() const {return mNumberOfTracksCrossing;}
inline int StRichSoftwareMonitor::numberOfTracksPidCalculated() const {return mNumberOfTracksPidCalculated;}
inline int StRichSoftwareMonitor::numberOfTracksAbove1Gev() const {return mNumberOfTracksAbove1Gev;}
inline int StRichSoftwareMonitor::numberOfHitsInRings() const {return mNumberOfHitsInRings;}
#endif
