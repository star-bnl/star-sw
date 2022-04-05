/*!
 * \class StTrackTopologyMap
 * \author Thomas Ullrich, AUg 1999
 */
/***************************************************************************
 *
 * $Id: StTrackTopologyMap.h,v 2.14 2018/03/27 02:41:00 genevb Exp $
 *
 * Author: Thomas Ullrich, AUg 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackTopologyMap.h,v $
 * Revision 2.14  2018/03/27 02:41:00  genevb
 * iTPC modifications, plus proper use of booleans
 *
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

class StTrackTopologyMap : public StObject {
public:
    StTrackTopologyMap();  
    StTrackTopologyMap(unsigned int, unsigned int, unsigned long long = 0);  
    StTrackTopologyMap(const unsigned long*, unsigned long long = 0);  
    StTrackTopologyMap(const unsigned int*, unsigned long long = 0);  
    // StTrackTopologyMap(const StTrackTopologyMap&);            use default
    // StTrackTopologyMap& operator=(const StTrackTopologyMap&); use default
    ~StTrackTopologyMap();  
    
    bool           primaryVertexUsed() const;     
    unsigned int   numberOfHits(StDetectorId) const;   
    bool           hasHitInDetector(StDetectorId) const;  
    bool           hasHitInDetector(StDetectorId, StDetectorId,
                                    StDetectorId = kUnknownId, StDetectorId = kUnknownId,
                                    StDetectorId = kUnknownId, StDetectorId = kUnknownId) const;  
    bool           hasHitInRow(StDetectorId, unsigned int) const; // first row = 1     
    bool           hasHitInSvtLayer(unsigned int) const;          // first layer = 1   
    bool           hasHitInPxlLayer(unsigned int) const;          // first layer = 1   
    bool           hasHitInIstLayer(unsigned int) const;          // first layer = 1   
    bool           hasHitInSsdLayer(unsigned int) const;          // first layer = 1   
    bool           hasHitInSstLayer(unsigned int) const;   
    
    bool           trackTpcOnly() const; 
    bool           trackSvtOnly() const;  
    bool           trackTpcSvt() const;  
    bool           trackFtpcEast() const; 
    bool           trackFtpcWest() const; 
    bool           trackFtpc() const;     
    
    bool           turnAroundFlag() const;    
    unsigned long long  data(unsigned int) const;  
    
    int            largestGap(StDetectorId) const;
    
protected:
    bool bit(int) const;        // range 0-63
    bool iTpcBit(int) const;    // range 0-63
    bool ftpcFormat() const;
    bool hftFormat() const;  // TPC tracks with HFT (Run13++) hit format
    
private:
    UInt_t     mMap0;
    UInt_t     mMap1;
    ULong64_t  mMap_iTpc;
    
    ClassDef(StTrackTopologyMap,2)
};

ostream& operator<< (ostream&, const StTrackTopologyMap&);

inline bool StTrackTopologyMap::hasHitInSstLayer(unsigned int val) const  
{
    return hasHitInSsdLayer(val);
}

#endif
