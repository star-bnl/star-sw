/***************************************************************************
 *
 * $Id: StCalibrationVertex.cxx,v 2.1 2001/11/10 23:52:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, Nov 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCalibrationVertex.cxx,v $
 * Revision 2.1  2001/11/10 23:52:13  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StCalibrationVertex.h"
#include "tables/St_dst_vertex_Table.h"
#include "StTrack.h"
#if !defined(ST_NO_NAMESPACES)
using std::copy;
#endif

ClassImp(StCalibrationVertex)

static const char rcsid[] = "$Id: StCalibrationVertex.cxx,v 2.1 2001/11/10 23:52:13 ullrich Exp $";

StCalibrationVertex::StCalibrationVertex()
{
    mType = kOtherVtxId;
}

StCalibrationVertex::StCalibrationVertex(const dst_vertex_st& v)
    : StVertex(v) { mType = kOtherVtxId; }

StCalibrationVertex::~StCalibrationVertex() {/* noop */};

StVertexId
StCalibrationVertex::type() const { return kOtherVtxId; }

unsigned int
StCalibrationVertex::numberOfDaughters() const { return 0; }

StTrack*
StCalibrationVertex::daughter(unsigned int) { return 0; }

const StTrack*
StCalibrationVertex::daughter(unsigned int) const { return 0; }

StPtrVecTrack
StCalibrationVertex::daughters(StTrackFilter&)
{
    StPtrVecTrack vec;
    return vec;
}

void
StCalibrationVertex::addDaughter(StTrack*) { /* noop */ }

void
StCalibrationVertex::removeDaughter(StTrack*) { /* noop */ }

StObject*
StCalibrationVertex::clone() const { return new StCalibrationVertex(*this); }

