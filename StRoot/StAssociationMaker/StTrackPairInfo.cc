/***************************************************************************
 *
 * $Id: StTrackPairInfo.cc,v 1.4 1999/12/08 00:00:25 calderon Exp $
 * $Log: StTrackPairInfo.cc,v $
 * Revision 1.4  1999/12/08 00:00:25  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
 * Revision 1.3  1999/09/23 21:25:24  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#include "StTrackPairInfo.hh"
#include "StMcTrack.hh"
#include "StGlobalTrack.h"

static const char rcsid[] = "$Id: StTrackPairInfo.cc,v 1.4 1999/12/08 00:00:25 calderon Exp $";

StTrackPairInfo::StTrackPairInfo(StGlobalTrack* rcTrk,
				 StMcTrack*     mcTrk,
				 unsigned int tpcPings,
				 unsigned int svtPings,
				 unsigned int ftpcPings)
    :
    mPartnerTrack(rcTrk),
    mPartnerMcTrack(mcTrk),
    mCommonTpcHits(tpcPings),
    mCommonSvtHits(svtPings),
    mCommonFtpcHits(ftpcPings)
{ /* noop */ }

StTrackPairInfo::~StTrackPairInfo() { /* noop */ }

void StTrackPairInfo::setPartnerMcTrack(StMcTrack* val) { mPartnerMcTrack = val; }

void StTrackPairInfo::setPartnerTrack(StGlobalTrack* val) { mPartnerTrack = val; }

void StTrackPairInfo::setCommonTpcHits(unsigned int val) { mCommonTpcHits = val; }

void StTrackPairInfo::setCommonSvtHits(unsigned int val) { mCommonSvtHits = val; }

void StTrackPairInfo::setCommonFtpcHits(unsigned int val) { mCommonFtpcHits = val; }

