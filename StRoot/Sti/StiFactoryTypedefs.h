//StiFactoryTypedefs.h
//M.L. Miller (Yale Software)
//05/01

#ifndef StiFactoryTypedefs_HH
#define StiFactoryTypedefs_HH

#include "StiObjectFactory.h"

#include "StiHit.h"
#include "StiTrackNode.h"
#include "StiKalmanTrack.h"

//#include "StiDefaultMutableTreeNode.h" //Obsolete
//typedef StiObjectFactory<StiDefaultMutableTreeNode> StiTreeNodeFactory;  //Obsolete

typedef StiObjectFactory<StiHit> StiHitFactory;
typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackNodeFactory;
typedef StiObjectFactory<StiTrackNode> StiTrackNodeFactory;

#endif
