/**
 *@file  StiToolkit.h
 *@brief Abstract interface for a STI toolkit
 *@author Claude A Pruneau, Wayne State University, 
 *@date   March 2002
 *@copyright 2002, STAR  Experiment at BNL, All rights reserved.  
 *<p> 
 *Permission to use, copy, modify and distribute this software and its
 *documentation strictly for non-commercial purposes is hereby granted 
 *without fee, provided that the above copyright notice appears in all
 *copies and that both the copyright notice and this permission notice
 *appear in the supporting documentation. The authors make no claims 
 *about the suitability of this software for any purpose. It is     
 *provided "as is" without express or implied warranty.             
 */
#ifndef StiToolkit_H
#define StiToolkit_H 1

class   StEvent;
class   StiDetector;
class   StiTrack;
class   StiKalmanTrack;
class   StiKalmanTrackNode;
class   StiNodeExt;
class   StiNodeInf;
class   StiHit;
class   StiDetectorBuilder;
class   StiMasterDetectorBuilder;
template<class Event> class StiDetectorGroup;
class  StiDetectorGroups;
template<class Factorized> class Factory;
template<class Filtered>   class EditableFilter;
template<class Filtered>   class Filter;
template<class T>          class StiCompositeTreeNode;
template<class X,class y>  class StiHitLoader;
template<class X,class y>  class StiMasterHitLoader;


// common object containers

class 	StiDetectorContainer;
class 	StiHitContainer;
class 	StiTrackContainer;
// service and convenience class objects.
class 	StiTrackFinder;
class 	StiTrackFinder;
class 	StiTrackFitter;
class 	StiTrackMerger;
class   StiVertexFinder;
//??class   StAssociationMaker;
class   EditableParameter;

/**
 *@class StiToolkit
 *@brief Definition of toolkit
 */
class StiToolkit 
{
public:
  StiToolkit(); 
  virtual Factory<StiHit> 		*getHitFactory()	{return _hitFactory;}
  virtual Factory<StiKalmanTrack> 	*getTrackFactory()	{return _trackFactory;}
  virtual Factory<StiKalmanTrackNode> 	*getTrackNodeFactory()	{return _trackNodeFactory;}
  virtual Factory<StiNodeExt>         	*getTrackNodeExtFactory()	{return _trackNodeExtFactory;}
  virtual Factory<StiNodeInf>         	*getTrackNodeInfFactory()	{return _trackNodeInfFactory;}
  virtual Factory<StiDetector>  	*getDetectorFactory()	{return _detectorFactory;}
  virtual Factory<StiCompositeTreeNode<StiDetector> >  *getDetectorNodeFactory(){return _detectorNodeFactory;}
  
  // common object containers 
  virtual StiMasterDetectorBuilder 	*getDetectorBuilder()	{return _detectorBuilder;}
  virtual StiDetectorContainer  	*getDetectorContainer()	{return _detectorContainer;}
  virtual StiDetectorGroups     	*getDetectorGroups()	{return _detectorGroups;}
  virtual StiHitContainer       	*getHitContainer()	{return _hitContainer;}
  virtual StiTrackContainer     	*getTrackContainer()	{return _trackContainer;}
  
  // service and convenience class objects.
  virtual StiTrackFinder        	*getTrackSeedFinder()	{return _trackSeedFinder;}
  virtual StiTrackFinder        	*getTrackFinder()	{return _trackFinder;}
  virtual StiTrackFitter        	*getTrackFitter()	{return _trackFitter;}
  virtual StiVertexFinder       	*getVertexFinder()	{return _vertexFinder;}
  virtual StiHitLoader<StEvent,StiDetectorBuilder> *getHitLoader()	{return _hitLoader;}

  virtual void add(StiDetectorGroup<StEvent>*detectorGroup)=0;

  virtual void setHitFactory(Factory<StiHit>			*set)	{_hitFactory   = set;}
  virtual void setTrackFactory(Factory<StiKalmanTrack>		*set)	{_trackFactory = set;}
  virtual void setTrackNodeFactory(Factory<StiKalmanTrackNode>	*set)	{_trackNodeFactory    = set;}
  virtual void setTrackNodeExtFactory(Factory<StiNodeExt>	*set)	{_trackNodeExtFactory = set;}
  virtual void setTrackNodeInfFactory(Factory<StiNodeInf>	*set)	{_trackNodeInfFactory = set;}
  virtual void setDetectorFactory(Factory<StiDetector>		*set)	{_detectorFactory     = set;}
  virtual void setDetectorNodeFactory(Factory<StiCompositeTreeNode<StiDetector> > *set){_detectorNodeFactory = set;}
  
  // common object containers 
  virtual void setDetectorBuilder(StiMasterDetectorBuilder	*set)	{_detectorBuilder   = set;}
  virtual void setDetectorContainer(StiDetectorContainer	*set)	{_detectorContainer = set;}
  virtual void setDetectorGroups(StiDetectorGroups		*set)	{_detectorGroups    = set;}
  virtual void setHitContainer(StiHitContainer			*set)	{_hitContainer      = set;}
  virtual void setTrackContainer(StiTrackContainer		*set)	{_trackContainer    = set;}
  
  // service and convenience class objects.
  virtual void setTrackSeedFinder(StiTrackFinder		*set)	{_trackSeedFinder = set;}
  virtual void addTrackSeedFinder(StiTrackFinder		*set)=0;
  virtual void setTrackFinder(StiTrackFinder			*set)	{_trackFinder     = set;}
  virtual void setTrackFitter(StiTrackFitter			*set)	{_trackFitter     = set;}
  virtual void setVertexFinder(StiVertexFinder			*set)	{_vertexFinder    = set;}
  virtual void setHitLoader(StiHitLoader<StEvent,StiDetectorBuilder>	*set) {_hitLoader = set;}

  static StiToolkit *instance();
  static void kill();
  
 protected:
  char mBeg[1];
  // small object factories
  Factory<StiHit>              *_hitFactory;
  Factory<StiKalmanTrack>      *_trackFactory;
  Factory<StiDetector>         *_detectorFactory;
  Factory< StiCompositeTreeNode<StiDetector> > *_detectorNodeFactory;
  Factory<StiKalmanTrackNode>  *_trackNodeFactory;
  Factory<StiNodeExt>          *_trackNodeExtFactory;
  Factory<StiNodeInf>          *_trackNodeInfFactory;
  
  // common object containers 
  StiMasterDetectorBuilder     *_detectorBuilder;
  StiDetectorContainer         *_detectorContainer;
  StiDetectorGroups            *_detectorGroups;  
  StiHitContainer              *_hitContainer;
  StiTrackContainer            *_trackContainer;
  
  // service and convenience class objects.
  StiTrackFinder          *_trackSeedFinder;
  StiTrackFinder          *_trackFinder;
  StiTrackFitter          *_trackFitter;
  StiVertexFinder         *_vertexFinder;
  StiHitLoader<StEvent,StiDetectorBuilder> *_hitLoader;
  char mEnd[1];


  static StiToolkit *_instance;
};

#endif

