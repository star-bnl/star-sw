/***********************************************************************
 *
 * $Id: StXiMc.cc,v 2.0 2000/06/05 05:19:46 genevb Exp $
 * $Log: StXiMc.cc,v $
 * Revision 2.0  2000/06/05 05:19:46  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Monte Carlo Xi micro dst class
 *
 ***********************************************************************/
#include "StXiMc.hh"

ClassImp(StXiMc)

StXiMc::StXiMc() : StODMc()
{}
  
StXiMc::StXiMc(StMcVertex* mcVertex, StMcTrack* mcDaughterTrack) :
         StODMc(mcVertex, mcDaughterTrack)
{}

StXiMc::~StXiMc()
{}
  
  
