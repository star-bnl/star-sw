/***************************************************************************
 *
 * $Id: StMcEventTypes.hh,v 2.2 2000/03/06 18:05:21 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMcEventTypes.hh,v $
 * Revision 2.2  2000/03/06 18:05:21  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.1  1999/12/03 21:31:19  calderon
 * Initial Revision
 *
 *
 **************************************************************************/
#ifndef StMcEventTypes_hh
#define StMcEventTypes_hh

#include "StMcContainers.hh"
#include "StMcEvent.hh"
#include "StMcFtpcHit.hh"
#include "StMcFtpcHitCollection.hh"
#include "StMcFtpcPlaneHitCollection.hh"
#include "StMcHit.hh"
#include "StMcHitComparisons.hh"
#include "StMcRichHit.hh"
#include "StMcRichHitCollection.hh"
#include "StMcSvtHit.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcSvtLadderHitCollection.hh"
#include "StMcSvtLayerHitCollection.hh"
#include "StMcSvtWaferHitCollection.hh"
#include "StMcTpcHit.hh"
#include "StMcTpcHitCollection.hh"
#include "StMcTpcPadrowHitCollection.hh"
#include "StMcTpcSectorHitCollection.hh"
#include "StMcTrack.hh"
#include "StMcVertex.hh"

#endif
