/***************************************************************************
 *
 * $Id: StHbtThreeParticleCorrFctnCollection.hh,v 1.3 2000/04/12 01:54:45 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *  
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The ThreeParticleCorrFctnCollection contains pointers to all 
 *   ThreeParticleCorrelation Functions that are associated with a 
 *   particular ThreeParticleAnalysis object.
 *
 ***************************************************************************
 *
 * $Log: StHbtThreeParticleCorrFctnCollection.hh,v $
 * Revision 1.3  2000/04/12 01:54:45  willson
 * Initial Installation - Comments Added
 *
 *
 ***************************************************************************/

#ifndef StHbtThreeParticleCorrFctnCollection_hh
#define StHbtThreeParticleCorrFctnCollection_hh


#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif
class StHbtThreeParticleCorrFctn;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtThreeParticleCorrFctn*, allocator<StHbtThreeParticleCorrFctn*> >            StHbtThreeParticleCorrFctnCollection;
typedef list<StHbtThreeParticleCorrFctn*, allocator<StHbtThreeParticleCorrFctn*> >::iterator  StHbtThreeParticleCorrFctnIterator;
#else
typedef list<StHbtThreeParticleCorrFctn*>            StHbtThreeParticleCorrFctnCollection;
typedef list<StHbtThreeParticleCorrFctn*>::iterator  StHbtThreeParticleCorrFctnIterator;
#endif

#endif
