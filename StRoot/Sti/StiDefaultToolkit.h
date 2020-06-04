/** 
 * @file  StiDefaultToolkit.h
 * @brief Default Implementation of the StiToolkit Abstract interface
 * @author Claude A Pruneau, Wayne State University, 
 * @date   March 2001
 * @copyright 2001, STAR  Experiment at BNL, All rights reserved.  
 *  
 * Permission to use, copy, modify and distribute this software and its
 * documentation strictly for non-commercial purposes is hereby granted 
 * without fee, provided that the above copyright notice appears in all
 * copies and that both the copyright notice and this permission notice
 * appear in the supporting documentation. The authors make no claims 
 * about the suitability of this software for any purpose. It is     
 * provided "as is" without express or implied warranty.             
 */
#ifndef StiDefaultToolkit_H
#define StiDefaultToolkit_H 1
#include "Sti/StiToolkit.h"

/** 
 * @class StiDefaultToolkit
 * @brief Definition of toolkit
 */
class StiDefaultToolkit : public StiToolkit
{
public:
  
  StiDefaultToolkit() ;
  virtual Factory<StiHit>          *getHitFactory();
  virtual Factory<StiKalmanTrack>  *getTrackFactory();
  virtual Factory<StiDetector>     *getDetectorFactory();
  virtual Factory<StiCompositeTreeNode<StiDetector> >  *getDetectorNodeFactory();
  virtual Factory<StiKalmanTrackNode>   *getTrackNodeFactory();
  virtual Factory<StiNodeExt>      *getTrackNodeExtFactory();
  virtual Factory<StiNodeInf>      *getTrackNodeInfFactory();
  virtual Factory<EditableParameter>    *getParameterFactory();
  virtual Factory<Filter<StiTrack>  >   *getTrackFilterFactory();

  // common object containers
  virtual StiMasterDetectorBuilder *getDetectorBuilder();
  virtual StiDetectorContainer     *getDetectorContainer();
  virtual StiDetectorGroups        *getDetectorGroups();
  virtual StiHitContainer          *getHitContainer();
  virtual StiTrackContainer        *getTrackContainer();
  
  // service and convenience class objects.
  virtual StiTrackFinder         *getTrackSeedFinder();
  virtual StiTrackFinder         *getTrackSeedFinderCA();
  virtual StiTrackFinder         *getTrackSeedFinderKNN();
  virtual StiTrackFinder         *getTrackFinder();
  virtual StiTrackFitter         *getTrackFitter();
  virtual StiVertexFinder        *getVertexFinder();
  virtual StiHitLoader<StEvent,StiDetectorBuilder> *getHitLoader();

  virtual void add(StiDetectorGroup<StEvent>* detectorGroup);

  void setEvaluatorEnabled(bool);
  bool isEvaluatorEnabled() const;
   
  virtual EditableFilter<StiHit>   *getLoaderHitFilter();
  virtual EditableFilter<StiTrack> *getLoaderTrackFilter();
  virtual EditableFilter<StiTrack> *getFinderTrackFilter();
  virtual void setLoaderHitFilter(EditableFilter<StiHit>   *);
  virtual void setLoaderTrackFilter(EditableFilter<StiTrack> *);
  virtual void setFinderTrackFilter(EditableFilter<StiTrack> *);

  virtual int getTruth(const StiHit *hit);
  
 protected:
  
  bool _evaluatorEnabled;
  
  virtual ~StiDefaultToolkit();
  
  // small object factories
  Factory< Filter<StiTrack>  > *_trackFilterFactory;
  Factory<EditableParameter>   *_parameterFactory;
  Factory<StiHit>              *_hitFactory;
  Factory<StiKalmanTrack>      *_trackFactory;
  Factory<StiDetector>         *_detectorFactory;
  Factory< StiCompositeTreeNode<StiDetector> > *_detectorNodeFactory;
  Factory<StiKalmanTrackNode>                  *_trackNodeFactory;
  Factory<StiNodeExt>                          *_trackNodeExtFactory;
  Factory<StiNodeInf>                          *_trackNodeInfFactory;
  
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

  EditableFilter<StiHit>   *_loaderHitFilter;
  EditableFilter<StiTrack> *_loaderTrackFilter;
  EditableFilter<StiTrack> *_finderTrackFilter;

};

#endif


