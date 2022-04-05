/*!
 * \class StTrackPidTraits 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StTrackPidTraits.h,v 2.9 2012/05/07 14:42:58 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.h,v $
 * Revision 2.9  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.8  2009/11/19 14:04:33  fisyak
 * move definition of dst_dedx_st IN
 *
 * Revision 2.7  2004/07/15 16:36:26  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.6  2002/02/22 22:56:53  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.5  2001/04/05 04:00:46  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2001/03/24 03:35:00  perev
 * clone() -> clone() const
 *
 * Revision 2.3  1999/11/29 17:07:32  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
 *
 * Revision 2.2  1999/11/15 18:48:28  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.1  1999/10/28 22:27:52  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:07  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#ifndef StTrackPidTraits_hh
#define StTrackPidTraits_hh
#include "StObject.h"
#include "StEnumerations.h"

#ifndef DST_DEDX_H
#define DST_DEDX_H
struct dst_dedx_st {
  Float_t dedx[3]; /* dE/dx summary info; mean,sigma of mean, log2(<dX>) */
  Int_t id_track;  /* Foreign key to dst_track                           */
  Int_t det_id;    /* Det ID-SVT,TPC,FTPC, use StDetectorDefinitions.h   */
  Int_t method;    /* dE/dx calculation method (see  comments above)     */
  Int_t ndedx;     /* number of points used in dE/dx calcu. + 100*((int)TrackLength) for fit */
};
#endif
class StTrackPidTraits : public StObject {
public:
    StTrackPidTraits();
    StTrackPidTraits(StDetectorId);
    StTrackPidTraits(const dst_dedx_st&);
    virtual ~StTrackPidTraits();
    
    Short_t detector() const;

protected:
    Short_t mDetectorId;

    ClassDef(StTrackPidTraits,2)
};
#endif
