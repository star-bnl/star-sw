/***************************************************************************
 *
 * StMcSvtHitCollection.hh
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
