/***************************************************************************
 *
 * $Id: StMcSvtHitCollection.hh,v 2.0 1999/11/17 02:12:16 calderon Exp $
 * $Log: StMcSvtHitCollection.hh,v $
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.2  1999/09/23 21:25:52  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/
#ifndef StMcSvtHitCollection_hh
#define StMcSvtHitCollection_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StMcSvtHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StMcSvtHit*, allocator<StMcSvtHit*> >            StMcSvtHitCollection;
typedef vector<StMcSvtHit*, allocator<StMcSvtHit*> >::iterator  StMcSvtHitIterator;
#else
typedef vector<StMcSvtHit*>            StMcSvtHitCollection;
typedef vector<StMcSvtHit*>::iterator  StMcSvtHitIterator;
#endif

#endif
