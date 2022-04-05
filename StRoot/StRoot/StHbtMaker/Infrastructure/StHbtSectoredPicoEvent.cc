/***************************************************************************
 *
 * $Id: StHbtSectoredPicoEvent.cc,v 1.2 2000/05/25 20:57:18 laue Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Specialized PicoEvents used in StHbtSectoredAnalysis.  A single
 *   PicoEvent contains a number of particle collections for each sector
 *   in momentum space.  These are grouped in a single dimensional array.
 *
 ***************************************************************************
 *
 * $Log: StHbtSectoredPicoEvent.cc,v $
 * Revision 1.2  2000/05/25 20:57:18  laue
 * bug fix in creating the arrays of collections. Now compiles on solaris
 *
 * Revision 1.1  2000/04/12 01:47:00  willson
 * Initial Installation
 *
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtSectoredPicoEvent.hh"

//________________
StHbtSectoredPicoEvent::StHbtSectoredPicoEvent(int numbins){

  int i;

  mNumBins = numbins;

  StHbtParticleCollection **sectoredCollection1 = new StHbtParticleCollection*[mNumBins+1];
  mFirstSectoredCollection = sectoredCollection1;

  for (i=0; i<=mNumBins; i++) 
	mFirstSectoredCollection[i] = new StHbtParticleCollection;

  StHbtParticleCollection **sectoredCollection2 = new StHbtParticleCollection*[mNumBins+1];
  mSecondSectoredCollection = sectoredCollection2;

  for (i=0; i<=mNumBins; i++) 
	mSecondSectoredCollection[i] = new StHbtParticleCollection;

  StHbtParticleCollection **sectoredCollection3 = new StHbtParticleCollection*[mNumBins+1];
  mThirdSectoredCollection = sectoredCollection3;

  for (i=0; i<=mNumBins; i++) 
	mThirdSectoredCollection[i] = new StHbtParticleCollection;
}

//________________
StHbtSectoredPicoEvent::StHbtSectoredPicoEvent(int numbinsx, int numbinsy, int numbinsz){

  int i;

  mNumBins = numbinsx*numbinsy*numbinsz;

  StHbtParticleCollection **sectoredCollection1 = new StHbtParticleCollection*[mNumBins+1];
  mFirstSectoredCollection = sectoredCollection1;

  for (i=0; i<=mNumBins; i++) 
	mFirstSectoredCollection[i] = new StHbtParticleCollection;

  StHbtParticleCollection **sectoredCollection2 = new StHbtParticleCollection*[mNumBins+1];
  mSecondSectoredCollection = sectoredCollection2;

  for (i=0; i<=mNumBins; i++) 
	mSecondSectoredCollection[i] = new StHbtParticleCollection;

  StHbtParticleCollection **sectoredCollection3 = new StHbtParticleCollection*[mNumBins+1];
  mThirdSectoredCollection = sectoredCollection3;

  for (i=0; i<=mNumBins; i++) 
	mThirdSectoredCollection[i] = new StHbtParticleCollection;
}

//_________________
StHbtSectoredPicoEvent::~StHbtSectoredPicoEvent(){
  
  int i;
  
  StHbtParticleIterator iter;
  
  for (i=0; i<=mNumBins; i++) {
    for (iter=mFirstSectoredCollection[i]->begin();iter!=mFirstSectoredCollection[i]->end();iter++){
      delete *iter;
    }	
    delete mFirstSectoredCollection[i];
  }
  delete [] mFirstSectoredCollection;

  for (i=0; i<=mNumBins; i++) {
    for (iter=mSecondSectoredCollection[i]->begin();iter!=mSecondSectoredCollection[i]->end();iter++){
      delete *iter;
    }	
    delete mSecondSectoredCollection[i];
  }
  delete [] mSecondSectoredCollection;

  for (i=0; i<=mNumBins; i++) {
    for (iter=mThirdSectoredCollection[i]->begin();iter!=mThirdSectoredCollection[i]->end();iter++){
      delete *iter;
    }	
    delete mThirdSectoredCollection[i];
  }
  delete [] mThirdSectoredCollection;

}
//_________________
