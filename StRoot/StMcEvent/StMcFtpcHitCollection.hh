/***************************************************************************
 *
 * StMcFtpcHitCollection.hh
 *
 **************************************************************************/
#ifndef StMcFtpcHitCollection_hh
#define StMcFtpcHitCollection_hh


#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StMcFtpcHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StMcFtpcHit*, allocator<StMcFtpcHit*> >            StMcFtpcHitCollection;
typedef vector<StMcFtpcHit*, allocator<StMcFtpcHit*> >::iterator  StMcFtpcHitIterator;
#else
typedef vector<StMcFtpcHit*>            StMcFtpcHitCollection;
typedef vector<StMcFtpcHit*>::iterator  StMcFtpcHitIterator;
#endif

#endif
