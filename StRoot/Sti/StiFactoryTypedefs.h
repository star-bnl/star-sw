//StiFactoryTypedefs.h
//M.L. Miller (Yale Software)
//05/01

#ifndef StiFactoryTypedefs_HH
#define StiFactoryTypedefs_HH

#include "StiObjectFactory.h"

#include "StiHit.h"
typedef StiObjectFactory<StiHit> StiHitFactory;

#include "StiKalmanTrack.h"
typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackNodeFactory;

#include "StiEvaluableTrack.h"
typedef StiObjectFactory<StiEvaluableTrack> StiEvaluableTrackFactory;

#endif
