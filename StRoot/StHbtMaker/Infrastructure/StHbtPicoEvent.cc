/***************************************************************************
 *
 * $Id: StHbtPicoEvent.cc,v 1.4 2000/07/16 21:38:23 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *  PicoEvents are last-step ultra-compressed "events" just containing
 *  bare information about the particles of interest.  They have already
 *  gone through Event and Track cuts, so only Pair cuts are left.
 *  PicoEvents are *internal* to the code, and are stored in the
 *  Event-mixing buffers.
 *           
 *
 ***************************************************************************
 *
 * $Log: StHbtPicoEvent.cc,v $
 * Revision 1.4  2000/07/16 21:38:23  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.3  2000/06/01 20:40:13  laue
 * StHbtIO.cc: updated for new V0s
 * StHbtPicoEvent.cc: collections especially cleared
 * franks1DHistoD.hh, include changed from  <stdio> to <cstdio>
 * franks1DHistoD.cc, cout statement deleted
 *
 * Revision 1.2  2000/03/17 17:23:05  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtPicoEvent.hh"

//________________
StHbtPicoEvent::StHbtPicoEvent(){
  mFirstParticleCollection = new StHbtParticleCollection;
  mSecondParticleCollection = new StHbtParticleCollection;
  mThirdParticleCollection = new StHbtParticleCollection;
}
//_________________
StHbtPicoEvent::~StHbtPicoEvent(){
  StHbtParticleIterator iter;


  if (mFirstParticleCollection){
      for (iter=mFirstParticleCollection->begin();iter!=mFirstParticleCollection->end();iter++){
	delete *iter;
      }
      mFirstParticleCollection->clear();
      delete mFirstParticleCollection;
      mFirstParticleCollection = 0;
  }

  if (mSecondParticleCollection){
    for (iter=mSecondParticleCollection->begin();iter!=mSecondParticleCollection->end();iter++){
      delete *iter;
    }
    mSecondParticleCollection->clear();
    delete mSecondParticleCollection;
    mSecondParticleCollection = 0;
  }

  if (mThirdParticleCollection){
    if (mThirdParticleCollection->size() != 0 ) {
      for (iter=mThirdParticleCollection->begin();iter!=mThirdParticleCollection->end();iter++){
	delete *iter;
      }
    }
    mThirdParticleCollection->clear();
    delete mThirdParticleCollection;
    mThirdParticleCollection = 0;
  }
}
//_________________
