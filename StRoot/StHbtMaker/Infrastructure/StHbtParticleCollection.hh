/***************************************************************************
 *
 * $Id: StHbtParticleCollection.hh,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   The ParticleCollection is the main component of the picoEvent
 *   It points to the particle objects in the picoEvent.           
 *
 ***************************************************************************
 *
 * $Log: StHbtParticleCollection.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/
#ifndef StHbtParticleCollection_hh
#define StHbtParticleCollection_hh
#include "StHbtMaker/Infrastructure/StHbtParticle.hh"
#include <list>

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtParticle*, allocator<StHbtParticle*> >            StHbtParticleCollection;
typedef list<StHbtParticle*, allocator<StHbtParticle*> >::iterator  StHbtParticleIterator;
#else
typedef list<StHbtParticle*>            StHbtParticleCollection;
typedef list<StHbtParticle*>::iterator  StHbtParticleIterator;
#endif

#endif
