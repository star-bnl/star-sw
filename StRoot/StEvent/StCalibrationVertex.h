/***************************************************************************
 *
 * $Id: StCalibrationVertex.h,v 2.1 2001/11/10 23:52:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2001
 ***************************************************************************
 *
 * Description: Concrete implementatin of StVertex to represent
 *              various types of vertices useful for calibration
 *              and diagnostics. No daughters, no parent.
 *
 ***************************************************************************
 *
 * $Log: StCalibrationVertex.h,v $
 * Revision 2.1  2001/11/10 23:52:14  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StCalibrationVertex_hh
#define StCalibrationVertex_hh
#include "StVertex.h"

class StCalibrationVertex : public StVertex {
public:
    StCalibrationVertex();
    StCalibrationVertex(const dst_vertex_st&);
    // StCalibrationVertex(const StCalibrationVertex&);            use default
    // StCalibrationVertex& operator=(const StCalibrationVertex&); use default
    virtual ~StCalibrationVertex();
    
    StVertexId     type() const;
    
    unsigned int   numberOfDaughters() const;
    StTrack*       daughter(unsigned int);
    const StTrack* daughter(unsigned int) const;
    StPtrVecTrack  daughters(StTrackFilter&);
    
    void addDaughter(StTrack*);
    void removeDaughter(StTrack*);

protected:    
    StObject* clone() const;
    ClassDef(StCalibrationVertex,1)
};
#endif
