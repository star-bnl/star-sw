/***********************************************************************
 *
 * $Id: StXiMc.cc,v 3.1 2000/07/14 21:28:34 genevb Exp $
 * $Log: StXiMc.cc,v $
 * Revision 3.1  2000/07/14 21:28:34  genevb
 * Added V0Mc index for XiMc, fixed bug with entries for XiMc, cleaned up controllers
 *
 * Revision 3.0  2000/07/14 12:56:50  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
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
{ v0 = -1; }
  
StXiMc::StXiMc(StMcVertex* mcVertex, StMcTrack* mcDaughterTrack) :
         StODMc(mcVertex, mcDaughterTrack)
{ v0 = -1; }

StXiMc::~StXiMc()
{}
  
  
