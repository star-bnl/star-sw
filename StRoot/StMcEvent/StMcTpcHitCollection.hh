/***************************************************************************
 *
 * StMcTpcHitCollection.hh
 *
 **************************************************************************/
#ifndef StMcTpcHitCollection_hh
#define StMcTpcHitCollection_hh


#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StMcTpcHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StMcTpcHit*, allocator<StMcTpcHit*> >            StMcTpcHitCollection;
typedef vector<StMcTpcHit*, allocator<StMcTpcHit*> >::iterator  StMcTpcHitIterator;
#else
typedef vector<StMcTpcHit*>            StMcTpcHitCollection;
typedef vector<StMcTpcHit*>::iterator  StMcTpcHitIterator;
#endif

#endif
