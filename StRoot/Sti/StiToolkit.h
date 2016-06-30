/** 
 * @file  StiToolkit.h
 * @brief Abstract interface for a STI toolkit
 * @author Claude A Pruneau, Wayne State University, 
 * @date   March 2002
 * @copyright 2002, STAR  Experiment at BNL, All rights reserved.  
 * <p> 
 * Permission to use, copy, modify and distribute this software and its
 * documentation strictly for non-commercial purposes is hereby granted 
 * without fee, provided that the above copyright notice appears in all
 * copies and that both the copyright notice and this permission notice
 * appear in the supporting documentation. The authors make no claims 
 * about the suitability of this software for any purpose. It is     
 * provided "as is" without express or implied warranty.             
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
//template<class EVENT> class StiDetectorGroups;
class StiDetectorGroups;
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
class 	StiTrackFitter;
class   StiVertexFinder;
class   EditableParameter;

/** 
 * @class StiToolkit
 * @brief Definition of toolkit
 */
class StiToolkit 
{
public:
  StiToolkit(); 
  virtual ~StiToolkit(){ /* nada */ }
  virtual Factory<StiHit> * getHitFactory()=0;
  virtual Factory<StiKalmanTrack> * getTrackFactory()=0;
  virtual Factory<StiKalmanTrackNode> * getTrackNodeFactory()=0;
  virtual Factory<StiNodeExt>         * getTrackNodeExtFactory()=0;
  virtual Factory<StiNodeInf>         * getTrackNodeInfFactory()=0;
  virtual Factory<StiDetector>  * getDetectorFactory()=0;
  virtual Factory<StiCompositeTreeNode<StiDetector> >  * getDetectorNodeFactory()=0;
  virtual Factory<EditableParameter>  * getParameterFactory()=0;
  virtual Factory< Filter<StiTrack>   >  * getTrackFilterFactory()=0;
  
  // common object containers 
  virtual StiMasterDetectorBuilder * getDetectorBuilder()=0;
  virtual StiDetectorContainer  * getDetectorContainer()=0;
  virtual StiDetectorGroups     * getDetectorGroups()=0;
  virtual StiHitContainer       * getHitContainer()=0;
  virtual StiTrackContainer     * getTrackContainer()=0;
  
  // service and convenience class objects.
  virtual StiTrackFinder        * getTrackSeedFinder()=0;
  virtual StiTrackFinder        * getTrackSeedFinderCA()=0;
  virtual StiTrackFinder        * getTrackSeedFinderKNN()=0;
  virtual StiTrackFinder        * getTrackFinder()=0;
  virtual StiTrackFitter        * getTrackFitter()=0;
  virtual StiVertexFinder       * getVertexFinder()=0;
  virtual StiHitLoader<StEvent,StiDetectorBuilder> * getHitLoader()=0;

  virtual void add(StiDetectorGroup<StEvent>* detectorGroup)=0;
  
  virtual void setEvaluatorEnabled(bool)=0;
  virtual bool isEvaluatorEnabled() const=0;

  virtual EditableFilter<StiHit>   * getLoaderHitFilter()=0;
  virtual EditableFilter<StiTrack> * getLoaderTrackFilter()=0;
  virtual EditableFilter<StiTrack> * getFinderTrackFilter()=0;
  virtual void setLoaderHitFilter(EditableFilter<StiHit>   *)=0;
  virtual void setLoaderTrackFilter(EditableFilter<StiTrack> *)=0;
  virtual void setFinderTrackFilter(EditableFilter<StiTrack> *)=0;

  virtual int getTruth(const StiHit *hit)=0;

  static void setToolkit(StiToolkit*toolkit);
  static StiToolkit *instance();
  static StiToolkit *Inst(){return instance();}
  static void kill();
  
 protected:

  static StiToolkit * _instance;
};

#endif

