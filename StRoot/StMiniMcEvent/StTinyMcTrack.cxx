/**
 * $Id $
 * \file  StTinyMcTrack.cxx
 * \brief Implementation of StTinyRcTrack (only a basic constructor and destructor are needed, plus ClassImp for CINT).
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 *  
 */
#include "StTinyMcTrack.h"

ClassImp(StTinyMcTrack)

StTinyMcTrack::StTinyMcTrack() {}

StTinyMcTrack::~StTinyMcTrack() {}
//
// $Log: StTinyMcTrack.cxx,v $
// Revision 1.1  2002/05/30 01:20:58  calderon
// Classes for use in a general framework for extracting efficiencies
// from both embedding and full simulations
// (after GSTAR+TRS+StEvent+StMcEvent+StAssociationMaker)
// so that the information of the track matches gets stored persistently.
//
//
