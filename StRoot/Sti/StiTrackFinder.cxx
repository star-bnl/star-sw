//StiTrackFinder.cxx
//C.Prunea (Wayne State U)
//05/01

#include <iostream.h>
#include <stdlib.h>
#include "StiSeedFinder.h"
#include "StiTrackFilter.h"
#include "StiDetectorContainer.h"
#include "StiHitContainer.h"
#include "StiTrackContainer.h"

#include "StiTrackFinder.h"


StiTrackFinder::StiTrackFinder()
{
}

StiTrackFinder::~StiTrackFinder()
{
}

//Was defined and not implemented. BROKE THE PROJECT... Blame and shame... ;)
void StiTrackFinder::findTracks()
{
    return;
}

void StiTrackFinder::setTrackSeedFinder(StiSeedFinder * finder)
{
  //----------------------------------------------------------------- 
  // Set the seedFinder to be used by this finder to the given
  // value. The current finder, if any, is destroyed.
  // Nothing is done if the given finder is same as currently used
  // by this track finder.
  //----------------------------------------------------------------- 
  
  // check whether current seed finder equals given finder, if so exit
  if (trackSeedFinder==finder)
    return;

  // check whether a seed finder currently exist, if so destroy it.
  if (trackSeedFinder!=0)
    delete trackSeedFinder;
 
  // set seed finder used by this track finder to given value
  trackSeedFinder = finder;  
}

void StiTrackFinder::setTrackFilter(StiTrackFilter * filter)
{
  //----------------------------------------------------------------- 
  // Set the seedFilter to be used by this track finder to the given
  // value. The current filter, if any, is destroyed.
  // Nothing is done if the given filter is same as that currently used
  // by this track finder.
  //----------------------------------------------------------------- 
  
  // check whether current filter equals given filter, if so exit
  if (trackFilter==filter)
    return;

  // check whether a filter currently exist, if so destroy it.
  if (trackFilter!=0)
    delete trackFilter;
 
  // set filter used by this track finder to given value
  trackFilter = filter;  
}

void StiTrackFinder::setGeometryContainer(StiDetectorContainer* newGeometry)
{
  //----------------------------------------------------------------- 
  // Set the detector geometry to be used by this track finder to the given
  // geometry container.
  // Note: the current container is destroyed.
  // Note: the current container is replaced only if the new container
  //       is different.
  //----------------------------------------------------------------- 
  
  // check whether current geometry equals given geometry, if so exit
  if (newGeometry==geometryContainer)
    return;

  // check whether a geometry currently exist, if so destroy it.
  if (geometryContainer!=0)
    delete geometryContainer;
 
  // set geometry used by this track finder to given value
  geometryContainer = newGeometry;
}

void StiTrackFinder::setHitContainer(StiHitContainer * newHitContainer)
{
  //----------------------------------------------------------------- 
  // Set the hit container to be used by this track finder to the given
  // hit container.
  // Note: the current container is destroyed.
  // Note: the current container is replaced only if the new container
  //       is different.
  //----------------------------------------------------------------- 
  
  // check whether current geometry equals given geometry, if so exit
  if (newHitContainer==hitContainer)
    return;

  // check whether a hit container currently exist, if so destroy it.
  if (hitContainer!=0)
    delete hitContainer;
 
  // set hit container used by this track finder to given value
  hitContainer = newHitContainer;
}

void StiTrackFinder::setTrackContainer(StiTrackContainer * newTrackContainer)
{
  //----------------------------------------------------------------- 
  // Set the track container to be used by this track finder to the given
  // track container.
  // Note: the current container is destroyed.
  // Note: the current container is replaced only if the new container
  //       is different.
  //----------------------------------------------------------------- 
  
  // check whether current container equals given container, if so exit
  if (newTrackContainer==trackContainer)
    return;

  // check whether a track container currently exist, if so destroy it.
  if (trackContainer!=0)
    delete trackContainer;
 
  // set track container used by this track finder to given value
  trackContainer = newTrackContainer;
}

void StiTrackFinder::setTrackNodeFactory(StiTrackNodeFactory * factory)
{
  //----------------------------------------------------------------- 
  // Set the track node factory to be used by this track finder to the given
  // factory
  // Note: the current factory is destroyed.
  // Note: the current factory is replaced only if the new factory
  //       is different.
  // 
  //----------------------------------------------------------------- 
  if (trackNodeFactory!=0 && trackNodeFactory!=factory)
    delete trackNodeFactory;
  trackNodeFactory = factory;
}

