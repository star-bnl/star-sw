/***********************************************************************
 *
 * $Id: StKinkMc.cc,v 3.0 2000/07/14 12:56:48 genevb Exp $
 * $Log: StKinkMc.cc,v $
 * Revision 3.0  2000/07/14 12:56:48  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:39  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo Kink micro dst class
 *
 ***********************************************************************/
#include "StKinkMc.hh"

ClassImp(StKinkMc)

StKinkMc::StKinkMc() : StODMc()
{}
  
StKinkMc::StKinkMc(StMcVertex* mcVertex, StMcTrack* mcDaughterTrack) :
         StODMc(mcVertex, mcDaughterTrack)
{}

StKinkMc::~StKinkMc()
{}
  
  
