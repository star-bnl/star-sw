/***********************************************************************
 *
 * $Id: StKinkMc.cc,v 2.0 2000/06/05 05:19:39 genevb Exp $
 * $Log: StKinkMc.cc,v $
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
  
  
