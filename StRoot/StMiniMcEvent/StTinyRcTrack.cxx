/**
 * $Id $
 * \file  StTinyRcTrack.cxx
 * \brief Implementation of StTinyMcTrack (only a basic constructor and destructor are needed, plus ClassImp for CINT).
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *  $Log $
 */
#include "StTinyRcTrack.h"

ClassImp(StTinyRcTrack)

StTinyRcTrack::StTinyRcTrack() {}

StTinyRcTrack::~StTinyRcTrack() {}
//
// $Log: StTinyRcTrack.cxx,v $
// Revision 1.2  2002/06/06 18:58:29  calderon
// Added $Log$
// Added mDedxPts data member, get and set methods, and updated ClassDef(StTinyRcTrack,1)
// to ClassDef(StTinyRcTrack,2) because of this change for schema evolution
//
// Revision 1.1  2002/05/30 01:20:58  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//
