/***************************************************************************
 *
 * $Id: StMcFtpcHitCollection.hh,v 2.0 1999/11/17 02:12:16 calderon Exp $
 * $Log: StMcFtpcHitCollection.hh,v $
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.2  1999/09/23 21:25:51  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
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
