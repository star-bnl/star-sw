/*!
 * \class StPrimaryVertex 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StPrimaryVertex.h,v 2.8 2002/04/18 23:38:21 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryVertex.h,v $
 * Revision 2.8  2002/04/18 23:38:21  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.7  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.6  2001/04/05 04:00:39  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.5  2001/03/24 03:34:54  perev
 * clone() -> clone() const
 *
 * Revision 2.4  1999/11/09 15:44:22  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.3  1999/11/04 20:36:17  ullrich
 * New method to obtain daughter container directly
 *
 * Revision 2.2  1999/10/28 22:26:19  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:43:33  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StPrimaryVertex_hh
#define StPrimaryVertex_hh
#include "StVertex.h"

class StPrimaryVertex : public StVertex {
public:
    StPrimaryVertex();
    StPrimaryVertex(const dst_vertex_st&);
    // StPrimaryVertex(const StPrimaryVertex&);            use default
    // StPrimaryVertex& operator=(const StPrimaryVertex&); use default
    ~StPrimaryVertex();
    
    StVertexId                   type() const;
    unsigned int                 numberOfDaughters() const;
    unsigned int                 numberOfDaughters(StTrackType) const;      // remove when EST becomes standard
    StTrack*                     daughter(unsigned int);
    const StTrack*               daughter(unsigned int) const;
    StTrack*                     daughter(unsigned int, StTrackType);       // remove when EST becomes standard
    const StTrack*               daughter(unsigned int, StTrackType) const; // remove when EST becomes standard
    StPtrVecTrack                daughters(StTrackFilter&);
    StPtrVecTrack                daughters(StTrackFilter&, StTrackType);    // remove when EST becomes standard
    StSPtrVecPrimaryTrack&       daughters(StTrackType = primary);          // change when EST becomes standard
    const StSPtrVecPrimaryTrack& daughters(StTrackType = primary) const;    // change when EST becomes standard
    void                         addDaughter(StTrack*);
    void                         removeDaughter(StTrack*);

    void setParent(StTrack*);     // overwrite inherited
    
protected:
    StSPtrVecPrimaryTrack    mDaughters;
    StSPtrVecPrimaryTrack    mEstDaughters;  // remove when EST becomes standard

    StObject* clone() const;
    ClassDef(StPrimaryVertex,2)
};
#endif
