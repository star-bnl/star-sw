/*!
 * \class StTrackTopologyMap
 * \author Thomas Ullrich, AUg 1999
 */
/***************************************************************************
 *
 * $Id: StTrackTopologyMap.h,v 2.13 2016/02/24 22:01:12 ullrich Exp $
 *
 * Author: Thomas Ullrich, AUg 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackTopologyMap.h,v $
 * Revision 2.13  2016/02/24 22:01:12  ullrich
 * Added method hasHitInSstLayer().
 *
 * Revision 2.12  2014/03/16 16:06:24  fisyak
 * Xin\'s fix for HFT
 *
 * Revision 2.11  2007/11/07 00:54:54  ullrich
 * Added PXL and IST.
 *
 * Revision 2.10  2005/06/23 19:04:24  ullrich
 * Added overloaded version of hasHitInDetector() taking up to 6 args.
 *
 * Revision 2.9  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.8  2002/02/22 22:56:53  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.7  2001/04/24 21:32:07  genevb
 * Additional helper functions
 *
 * Revision 2.6  2001/04/05 04:00:46  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2000/05/17 17:21:38  ullrich
 * New method largestGap() and new output operator.
 *
 * Revision 2.4  2000/04/12 19:44:03  genevb
 * Reimplement mMap data members as individual unsigned ints
 *
 * Revision 2.3  2000/04/10 19:59:33  genevb
 * StRoot/StEvent/doc/tex/
 *
 * Revision 2.2  1999/12/13 20:16:39  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.1  1999/10/13 19:44:17  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTrackTopologyMap_hh
#define StTrackTopologyMap_hh
#include <Stiostream.h>
#include "StObject.h"
#include "StEnumerations.h"
#define __kfpAtFirstHit__
class StTrackTopologyMap : public StObject {
 public:
  StTrackTopologyMap(UInt_t m1 = 0, UInt_t m2 = 0, UInt_t m3 = 0) : mMap0(m1), mMap1(m2), mMap2(m3) {}
  StTrackTopologyMap(const UInt_t*  m) : mMap0(m[0]), mMap1(m[1]), mMap2(0) {}
  StTrackTopologyMap(const ULong_t* m) : mMap0(m[0]), mMap1(m[1]), mMap2(0) {}
    // StTrackTopologyMap(const StTrackTopologyMap&);            use default
    // StTrackTopologyMap& operator=(const StTrackTopologyMap&); use default
  ~StTrackTopologyMap() {}

    bool           primaryVertexUsed() const;
    UInt_t   numberOfHits(StDetectorId) const;
    bool           hasHitInDetector(StDetectorId) const;
    bool           hasHitInDetector(StDetectorId, StDetectorId,
                              StDetectorId = kUnknownId, StDetectorId = kUnknownId,
                              StDetectorId = kUnknownId, StDetectorId = kUnknownId) const;
    bool           hasHitInRow(StDetectorId, UInt_t) const; // first row = 1
    bool           hasHitInSvtLayer(UInt_t) const;          // first layer = 1
    bool           hasHitInPxlLayer(UInt_t) const;          // first layer = 1
    bool           hasHitInIstLayer(UInt_t) const;          // first layer = 1
    bool           hasHitInSsdLayer(UInt_t) const;          // first layer = 1
    bool           hasHitInSstLayer(UInt_t) const;
    
    bool           trackTpcOnly() const;
    bool           trackSvtOnly() const;
    bool           trackTpcSvt() const;
    bool           trackFtpcEast() const;
    bool           trackFtpcWest() const;
    bool           trackFtpc() const;

    bool           turnAroundFlag() const;
    UInt_t   data(UInt_t) const;

    int            largestGap(StDetectorId) const;
    
protected:
    bool bit(int) const;     // range 0-63
    bool iTpcBit(int) const; // range 0-63
    bool ftpcFormat() const;
    bool hftFormat() const;  // TPC tracks with HFT (Run13++) hit format
    
private:
    UInt_t  mMap0;
    UInt_t  mMap1;
#ifndef __ROOT_CINT__
    union {
      UInt_t   mMap2;
      Long64_t mMap_iTpc;
    };
#else
      UInt_t   mMap2;
#endif
    ClassDef(StTrackTopologyMap,3)
};

ostream& operator<< (ostream&, const StTrackTopologyMap&);

inline bool StTrackTopologyMap::hasHitInSstLayer(UInt_t val) const
{
    return hasHitInSsdLayer(val);
}

#endif
