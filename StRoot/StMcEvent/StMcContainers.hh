/***************************************************************************
 *
 * $Id: StMcContainers.hh,v 2.0 1999/11/17 02:00:58 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez, Oct 1999
 ***************************************************************************
 *
 * Description: Containers for StMcEvent objects
 *
 ***************************************************************************
 *
 * $Log: StMcContainers.hh,v $
 * Revision 2.0  1999/11/17 02:00:58  calderon
 * Completely revised for new StEvent
 *
 **************************************************************************/
#ifndef StMcContainers_hh
#define StMcContainers_hh

class StMcHit;
class StMcVertex;
class StMcSvtHit;
class StMcTpcHit;
class StMcFtpcHit;
class StMcTrack;

#ifdef PERSISTENT
#include "StArray.h"

StCollectionDef(McHit)
StCollectionDef(McVertex)
StCollectionDef(McSvtHit)
StCollectionDef(McTpcHit)
StCollectionDef(McFtpcHit)
StCollectionDef(McTrack)

#else
#include <vector>
     
#ifndef ST_NO_TEMPLATE_DEF_ARGS
//Owners
typedef vector<StMcHit*>     StSPtrVecMcHit; 
typedef vector<StMcVertex*>  StSPtrVecMcVertex;
typedef vector<StMcSvtHit*>  StSPtrVecMcSvtHit;
typedef vector<StMcTpcHit*>  StSPtrVecMcTpcHit;
typedef vector<StMcFtpcHit*> StSPtrVecMcFtpcHit;
typedef vector<StMcTrack*>   StSPtrVecMcTrack;
// Not owners
typedef vector<StMcVertex*>  StPtrVecMcVertex;
typedef vector<StMcSvtHit*>  StPtrVecMcSvtHit;
typedef vector<StMcTpcHit*>  StPtrVecMcTpcHit;
typedef vector<StMcFtpcHit*> StPtrVecMcFtpcHit;
typedef vector<StMcTrack*>   StPtrVecMcTrack;

#else
//Owners
typedef vector<StMcHit*, allocator<StMcHit*> >          StSPtrVecMcHit;
typedef vector<StMcVertex*, allocator<StMcVertex*> >    StSPtrVecMcVertex;
typedef vector<StMcSvtHit*, allocator<StMcSvtHit*> >    StSPtrVecMcSvtHit;
typedef vector<StMcTpcHit*, allocator<StMcTpcHit*> >    StSPtrVecMcTpcHit;
typedef vector<StMcFtpcHit*, allocator<StMcFtpcHit*> >  StSPtrVecMcFtpcHit;
typedef vector<StMcTrack*, allocator<StMcTrack*> >      StSPtrVecMcTrack;
// Not owners
typedef vector<StMcVertex*, allocator<StMcVertex*> >    StPtrVecMcVertex;
typedef vector<StMcSvtHit*, allocator<StMcSvtHit*> >    StPtrVecMcSvtHit;
typedef vector<StMcTpcHit*, allocator<StMcTpcHit*> >    StPtrVecMcTpcHit;
typedef vector<StMcFtpcHit*, allocator<StMcFtpcHit*> >  StPtrVecMcFtpcHit;
typedef vector<StMcTrack*, allocator<StMcTrack*> >      StPtrVecMcTrack;

#endif // no template default arguments
//Iterators
typedef StPtrVecMcVertex::iterator  StPtrVecMcVertexIterator;
typedef StPtrVecMcSvtHit::iterator  StPtrVecMcSvtHitIterator;
typedef StPtrVecMcTpcHit::iterator  StPtrVecMcTpcHitIterator;
typedef StPtrVecMcFtpcHit::iterator StPtrVecMcFtpcHitIterator;
typedef StPtrVecMcTrack::iterator   StPtrVecMcTrackIterator;
//Const Iterators
typedef StPtrVecMcVertex::const_iterator  StPtrVecMcVertexConstIterator;
typedef StPtrVecMcSvtHit::const_iterator  StPtrVecMcSvtHitConstIterator;
typedef StPtrVecMcTpcHit::const_iterator  StPtrVecMcTpcHitConstIterator;
typedef StPtrVecMcFtpcHit::const_iterator StPtrVecMcFtpcHitConstIterator;
typedef StPtrVecMcTrack::const_iterator   StPtrVecMcTrackConstIterator;

#endif //persistent

#endif //StMcContainers
