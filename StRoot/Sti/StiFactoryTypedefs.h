//StiFactoryTypedefs.h
//M.L. Miller (Yale Software)
//05/01

#ifndef StiFactoryTypedefs_HH
#define StiFactoryTypedefs_HH

#include "StiObjectFactory.h"

#include "StiHit.h"
typedef StiObjectFactory<StiHit> StiHitFactory;

//#include "StiTrackNode.h"
//typedef StiObjectFactory<StiTrackNode> StiTrackNodeFactory;

#include "StiKalmanTrack.h"
typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackNodeFactory;

#endif
