//StiFactoryTypedefs.h
//M.L. Miller (Yale Software)
//05/01

#ifndef StiFactoryTypedefs_HH
#define StiFactoryTypedefs_HH

#include "StiFactory.h" //OK
#include "StiObjectFactory.h" //OK

#include "StiHit.h" //OK
typedef StiObjectFactory<StiHit> StiHitFactory; //OK

#include "StiKalmanTrack.h" //OK
typedef StiObjectFactory<StiKalmanTrack> StiKalmanTrackNodeFactory; //OK

#include "StiEvaluableTrack.h" //OK
#include "StiGui/StiRootDrawableStiEvaluableTrack.h"
//typedef StiObjectFactory<StiEvaluableTrack> StiEvaluableTrackFactory; //OK
typedef StiObjectFactory<StiRootDrawableStiEvaluableTrack> StiEvaluableTrackFactory; //OK

#include "StiDetector.h" //OK
#include "StiGui/StiRootDrawableDetector.h"

//typedef StiObjectFactory<StiDetector> detector_factory; //OK
typedef StiObjectFactory<StiRootDrawableDetector> detector_factory; //OK

#include "StiCompositeTreeNode.h" //OK
typedef StiDetector data_t; //OK

#ifndef __CINT__
typedef StiCompositeTreeNode<data_t> data_node;
#else
class data_node;
#endif

typedef vector<data_node*> data_node_vec; 
typedef StiObjectFactory<data_node> data_node_factory;

#endif
