/***************************************************************************
 *
 * $Id: StMcFtpcHit.cc,v 2.0 1999/11/17 02:12:16 calderon Exp $
 * $Log: StMcFtpcHit.cc,v $
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:50  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcFtpcHit.hh"
#include "StMcTrack.hh"

static const char rcsid[] = "$Id: StMcFtpcHit.cc,v 2.0 1999/11/17 02:12:16 calderon Exp $";

StMcFtpcHit::StMcFtpcHit(const StThreeVectorF& p,
			 const float de, const float ds,
			 StMcTrack* parent)  : StMcHit(p, de, ds, parent)
{ /* noop */ }

StMcFtpcHit::StMcFtpcHit(g2t_ftp_hit_st* pt)
{
  mdE = pt->de;
  mdS = pt->ds;
  // Decode position.
  mPosition.setX(pt->x[0]); 
  mPosition.setY(pt->x[1]);
  mPosition.setZ(pt->x[2]);
  // For parent track, the g2t_hits table only gives the id of 
  // the parent track :  pt->track_p  .  We need to decode this and assign
  // mParentTrack to the pointer to the parent track.
}



