/*!
 * \class StTrackABC 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrackABC.h,v 1.1.1.1 2013/07/23 14:13:30 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 * Note the following: with the arrival of ITTF it is now possible to
 * store the numberOfPossiblePoints for every detector individually. Before
 * that and because of the way the tables were defined TPC and FTPC were
 * one and the same. This caused confusion. However, since we have to
 * stay backwards compatible the "old way" is still working.
 *    mNumberOfPossiblePoints ==>  seedQuality 
 *    mNumberOfPossiblePointsTpc
 *    mNumberOfPossiblePointsFtpcWest 
 *    mNumberOfPossiblePointsFtpcEast 
 *    mNumberOfPossiblePointsSvt 
 *    mNumberOfPossiblePointsSsd 
 *    are the ones that count
  * --------------------------------------------------------------------------
 *  The track flag (mFlag accessed via flag() method) definitions with ITTF 
 *  (flag definition in EGR era can be found at
 *   http://www.star.bnl.gov/STAR/html/all_l/html/dst_track_flags.html)
 *
 *  mFlag= zxyy, where  z = 1 for pile up track in TPC (otherwise 0) 
 *                      x indicates the detectors included in the fit and 
 *                     yy indicates the status of the fit. 
 *  Positive mFlag values are good fits, negative values are bad fits. 
 *
 *  The first digit indicates which detectors were used in the refit: 
 *
 *      x=1 -> TPC only 
 *      x=3 -> TPC       + primary vertex 
 *      x=5 -> SVT + TPC 
 *      x=6 -> SVT + TPC + primary vertex 
 *      x=7 -> FTPC only 
 *      x=8 -> FTPC      + primary 
 *      x=9 -> TPC beam background tracks            
 *
 *  The last two digits indicate the status of the refit: 
 *       = +x01 -> good track 
 *
 *      = -x01 -> Bad fit, outlier removal eliminated too many points 
 *      = -x02 -> Bad fit, not enough points to fit 
 *      = -x03 -> Bad fit, too many fit iterations 
 *      = -x04 -> Bad Fit, too many outlier removal iterations 
 *      = -x06 -> Bad fit, outlier could not be identified 
 *      = -x10 -> Bad fit, not enough points to start 
 *
 *      = +x11 -> Short track pointing to EEMC
 *
 ***************************************************************************
 *
 * $Log: StTrackABC.h,v $
 * Revision 1.1.1.1  2013/07/23 14:13:30  fisyak
 *
 *
 * Revision 2.33  2013/07/16 14:29:04  fisyak
 * Restore mass fit tracks
 *
 * Revision 2.31  2013/04/05 15:11:33  ullrich
 * Changes due to the addition of StTrackMassFit (Yuri)
 *
 * Revision 2.30  2013/01/15 23:21:06  fisyak
 * improve printouts
 *
 * Revision 2.29  2012/09/16 21:36:09  fisyak
 * Handlers for Tpc Only West and Only East bits
 *
 * Revision 2.28  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.27  2011/10/13 21:25:27  perev
 * setting IdTruth from the hits is added
 *
 * Revision 2.26  2011/04/26 21:41:29  fisyak
 * Make mKey Int_t instead of UShort_t (no. of tracks might be more that 64k)
 *
 * Revision 2.25  2011/03/31 19:29:01  fisyak
 * Add IdTruth information for tracks and vertices
 *
 * Revision 2.24  2010/08/31 20:00:09  fisyak
 * Clean up, add mSeedQuality
 *
 * Revision 2.23  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.22  2008/08/26 12:47:38  fisyak
 * add track pule up flag description
 *
 * Revision 2.21  2007/10/11 21:51:40  ullrich
 * Added member to handle number of possible points fpr PXL and IST.
 *
 * Revision 2.20  2006/08/28 17:03:54  fisyak
 * Add track flag definitions from EGR (we suppose that this will be primary information from now)
 *
 * Revision 2.19  2004/08/05 22:24:51  ullrich
 * Changes to the handling of numberOfPoints() to allow ITTF more flexibility.
 *
 * Revision 2.18  2004/07/15 16:36:26  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.17  2003/10/31 16:00:04  ullrich
 * Added setKey() method.
 *
 * Revision 2.16  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.15  2002/03/14 17:42:15  ullrich
 * Added method to set mNumberOfPossiblePoints.
 *
 * Revision 2.14  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.13  2001/09/28 22:20:50  ullrich
 * Added helix geometry at last point.
 *
 * Revision 2.12  2001/05/30 17:45:55  perev
 * StEvent branching
 *
 * Revision 2.11  2001/04/05 04:00:45  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.10  2001/03/24 03:34:59  perev
 * clone() -> clone() const
 *
 * Revision 2.9  2001/03/16 20:56:46  ullrich
 * Added non-const version of fitTraits().
 *
 * Revision 2.8  1999/12/01 15:58:10  ullrich
 * New decoding for dst_track::method. New enum added.
 *
 * Revision 2.7  1999/11/30 23:20:32  didenko
 * temporary solution to get library compiled
 *
 * Revision 2.6  1999/11/29 17:32:45  ullrich
 * Added non-const method pidTraits().
 *
 * Revision 2.5  1999/11/15 18:48:22  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.4  1999/11/05 15:27:07  ullrich
 * Added non-const versions of several methods
 *
 * Revision 2.3  1999/11/04 13:32:03  ullrich
 * Added non-const versions of some methods
 *
 * Revision 2.2  1999/11/01 12:45:06  ullrich
 * Modified unpacking of point counter
 *
 * Revision 2.1  1999/10/28 22:27:24  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:56  ullrich
 * Completely Revised for New Version
 *
 * 
 **************************************************************************/
#ifndef StTrackABC_hh
#define StTrackABC_hh
#include "StObject.h"
#include "StArray.h"
#include "StEnumerations.h"
#include "StFunctional.h"
#include "Stiostream.h"
class StTrackABC;
std::ostream&  operator<<(std::ostream& os,  const StTrackABC& t);

class StVertex;
class StTrackNode;

class StTrackABC : public StObject {
public:
    StTrackABC();
    StTrackABC(const StTrackABC&);
    StTrackABC & operator=(const StTrackABC&);
    virtual ~StTrackABC() {}

    virtual StTrackType            type() const = 0;
    virtual Int_t                  key() const { return mKey; }
    StTrackNode*                   node();
    const StTrackNode*             node() const;
    const StVertex*                vertex() const {return mVertex;}
    virtual const StVertex*        endVertex() const {return mEndVertex;}
    void                           setKey(Int_t val) { mKey = val; }
    void                           setNode(StTrackNode* node);
    void                           setEndVertex(StVertex *v = 0) {mEndVertex = v;}
    void                           setVertex(StVertex* /* val */);
    virtual void                   Print(Option_t *option="") const {std::cout << option << (*this) << std::endl; }
    
protected:
    Char_t                         mBeg[1]; //!
    Int_t                          mKey;
    StVertex                      *mEndVertex;
//  StTrackNode                   *mNode;                 //$LINK
#ifdef __CINT__
    StObjLink                      mNode;                 
#else
    StLink<StTrackNode>            mNode;                 
#endif //__CINT__
    StVertex                      *mVertex;               
    Char_t                         mEnd[1]; //!
    ClassDef(StTrackABC,1)
};
#endif
