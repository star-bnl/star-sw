/** 
 * @file  StiDefaultToolkit.cxx
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
#include "StiDefaultToolkit.h"
#include "Sti/Filter.h"
#include "Sti/Factory.h"
#include "Sti/VectorizedFactory.h"
#include "Sti/StiMcTrack.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiHitFiller.h"
#include "Sti/StiMasterHitLoader.h"
#include "Sti/StiTpcHitLoader.h"
#include "Sti/StiSvtHitLoader.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiGeometryTransform.h"
#include "Sti/StiCoordinateTransform.h"
#include "Sti/StiTrackSeedFinder.h"
#include "Sti/StiTrackFinder.h"
#include "Sti/StiTrackFitter.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiTrack.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiKalmanTrackFitter.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiTrackMerger.h"
#include "Sti/StiCompositeSeedFinder.h"
#include "Sti/StiLocalTrackMerger.h"
#include "Sti/StiDisplayManager.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiIOBroker.h"
#include "Sti/Parameter.h"
#include "StiMaker/RootEditableParameter.h"
#include "StiMaker/StiRootIOBroker.h"
#include "StiGui/StiRootDisplayManager.h"
#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiRootDrawableMcTrack.h"
#include "StiGui/StiRootDrawableHitContainer.h"
#include "StiGui/StiRootDrawableKalmanTrack.h"
#include "StiGui/StiRootDrawableStiEvaluableTrack.h"

#include "StiEvaluator/StiEvaluator.h"
#include "Sti/StiEvaluableTrackSeedFinder.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "Sti/StiHitErrorCalculator.h"

StiDefaultToolkit::StiDefaultToolkit()
  :
  trackFilterFactory(0),
  parameterFactory(0),
  hitFactory(0),
  trackFactory(0),
  mcTrackFactory(0),
  detectorFactory(0),
  trackNodeFactory(0),
  detectorContainer(0),
  hitContainer(0),
  trackContainer(0),
  mcTrackContainer(0),
  geometryTransform(0),
  coordinateTransform(0),
  detectorFinder(0),
  trackSeedFinder(0),
  trackFinder(0),
  trackFitter(0),
  trackMerger(0),
  displayManager(0),
  ioBroker(0),
  hitFiller(0),
  associationMaker(0)
{};

StiDefaultToolkit::~StiDefaultToolkit()
{
  delete trackFilterFactory;
  delete hitFactory;
  delete hitContainer;
  delete detectorFactory;
  delete detectorContainer;
  StiDetectorFinder::kill();
  delete trackNodeFactory;
  delete trackContainer;
  delete mcTrackContainer;
  StiGeometryTransform::kill();
  StiCoordinateTransform::kill();
  delete trackFactory;
  delete mcTrackFactory;
  delete parameterFactory;
  delete trackSeedFinder;
  delete trackFinder;
  delete trackFitter;
  delete trackMerger;
  //delete displayManager;
  //delete evaluator;
  //delete eventAssociator;
  delete ioBroker;
};

Factory< Filter<StiTrack>   >  * StiDefaultToolkit::getTrackFilterFactory()
{
  if (trackFilterFactory)
    return trackFilterFactory;
  cout << "StiDefaultToolkit::getTrackFilterFactory() - INFO - Instantiating StiTrackFilterFactory" << endl;
  trackFilterFactory = new VectorizedFactory<StiDefaultTrackFilter, Filter<StiTrack>  >("StiDefaultTrackFilterFactory",3,5,2);
  return trackFilterFactory;
}

Factory<Parameter>  * StiDefaultToolkit::getParameterFactory()
{
  if (parameterFactory)
    return parameterFactory;
  StiIOBroker * ioBroker = getIOBroker();
  if (ioBroker->useGui())
    parameterFactory = new VectorizedFactory<RootEditableParameter,Parameter>("ParameterFactory",100,20,10);
  else
    parameterFactory = new VectorizedFactory<Parameter,Parameter>("ParameterFactory",100,20,10);
  return parameterFactory;
}

StiGeometryTransform * StiDefaultToolkit::getGeometryTransform()
{
  if(!geometryTransform){
    geometryTransform = StiGeometryTransform::instance();
  }
  return geometryTransform;
}

StiCoordinateTransform *StiDefaultToolkit::getCoordinateTransform(){
  if(!coordinateTransform){
    coordinateTransform = StiCoordinateTransform::instance();
  }
  return coordinateTransform;
}

Factory<StiHit>* StiDefaultToolkit::getHitFactory()
{
  if (hitFactory)
    return hitFactory;
  hitFactory = new VectorizedFactory<StiHit,StiHit>("StiHitFactory",50000,20000,5);
  return hitFactory;
}

Factory<StiKalmanTrack>* StiDefaultToolkit::getTrackFactory()
{
  if (trackFactory)
    return trackFactory;
  StiIOBroker * ioBroker = getIOBroker();
  cout << "StiDefaultToolkit::getTrackFactory() - INFO - "; 
  if (ioBroker->useGui())
    {
      if (ioBroker->seedFinderType()==StiIOBroker::kEvaluable)
	  trackFactory = new VectorizedFactory<StiRootDrawableStiEvaluableTrack,StiKalmanTrack>("StiRDEvaluableTrackFactory",10000,5000,10);
      else //if (ioBroker->seedFinderType()==StiIOBroker::kComposite)
	  trackFactory = new VectorizedFactory<StiRootDrawableKalmanTrack,StiKalmanTrack>("StiRDKalmanTrackFactory",10000,5000,10);
    }
  else // no gui needed
    {	
      if (ioBroker->seedFinderType()==StiIOBroker::kEvaluable)
	  trackFactory = new VectorizedFactory<StiEvaluableTrack,StiKalmanTrack>("StiEvaluableTrackFactory",10000,5000,10);
      else //if (ioBroker->seedFinderType()==StiIOBroker::kComposite)
	  trackFactory = new VectorizedFactory<StiKalmanTrack,StiKalmanTrack>("StiKalmanTrackFactory",10000,5000,10);
    }
  return trackFactory;
}

Factory<StiMcTrack>* StiDefaultToolkit::getMcTrackFactory()
{
  if (mcTrackFactory)
    return mcTrackFactory;
  StiIOBroker * ioBroker = getIOBroker();
  cout << "StiDefaultToolkit::getMcTrackFactory() - INFO - "; 
  if (ioBroker->useGui())
      mcTrackFactory = new VectorizedFactory<StiRootDrawableMcTrack,StiMcTrack>("StiRootDrawableMcTrackFactory",10000,5000,10);
  else // no gui needed
      mcTrackFactory = new VectorizedFactory<StiMcTrack,StiMcTrack>("StiMcTrackFactory",10000,5000,10);
  return mcTrackFactory;
}

Factory<StiDetector>* StiDefaultToolkit::getDetectorFactory()
{
  if (detectorFactory)
    return detectorFactory;
  cout << "StiDefaultToolkit::getDetectorFactory() - INFO - "; 
  if (getIOBroker()->useGui())
    detectorFactory = new VectorizedFactory<StiRootDrawableDetector,StiDetector>("StiRDDetectorFactory",1000,200,10);
  else
    detectorFactory = new VectorizedFactory<StiDetector,StiDetector>("StiDetectorFactory",1000,200,10);
  return detectorFactory;
}

Factory< StiCompositeTreeNode<StiDetector>  >* StiDefaultToolkit::getDetectorNodeFactory()
{
  if (detectorNodeFactory)
    return detectorNodeFactory;
  detectorNodeFactory = new VectorizedFactory< StiCompositeTreeNode<StiDetector>  , 
    StiCompositeTreeNode<StiDetector>  >(" StiCompositeTreeNode<StiDetector>Factory",1000,500,10);
  return detectorNodeFactory;
}


Factory<StiKalmanTrackNode>* StiDefaultToolkit::getTrackNodeFactory()
{
  if (trackNodeFactory)
    return trackNodeFactory;
  trackNodeFactory = new VectorizedFactory<StiKalmanTrackNode,StiKalmanTrackNode>("StiKalmanTrackNodeFactory",20000,20000,50);
  StiKalmanTrack::setKalmanTrackNodeFactory(trackNodeFactory);
  return trackNodeFactory;	
}


StiDetectorContainer  * StiDefaultToolkit::getDetectorContainer()
{
  if (detectorContainer)
    return detectorContainer;
  detectorContainer = StiDetectorContainer::instance();
  detectorContainer->buildDetectors(getDetectorNodeFactory(),getDetectorFactory());
  detectorContainer->reset();
  return detectorContainer;
}

StiHitContainer       * StiDefaultToolkit::getHitContainer()
{
  if (hitContainer)
    return hitContainer;
  cout << "StiDefaultToolkit::getHitContainer() - INFO - "; 
  if (getIOBroker()->useGui())
    {
      hitContainer = new StiRootDrawableHitContainer();
      cout << "instantiating StiRootDrawableHitContainer" << endl;
    }
  else 
    {
      hitContainer = new StiHitContainer();			
      cout << "instantiating StiRootDrawableHitContainer" << endl;
    }
  return hitContainer;
}

StiTrackContainer     * StiDefaultToolkit::getTrackContainer()
{	
  cout << "StiDefaultToolkit::getTrackContainer() - INFO - Starting" << endl;
  if (trackContainer)
    return trackContainer;
  cout << "StiDefaultToolkit::getTrackContainer() - INFO - Instantiating Container" << endl;
  trackContainer = new StiTrackContainer();
  return trackContainer;
}

StiTrackContainer     * StiDefaultToolkit::getMcTrackContainer()
{	
  cout << "StiDefaultToolkit::getMcTrackContainer() - INFO - Starting" << endl;
  if (mcTrackContainer)
    return mcTrackContainer;
  cout << "StiDefaultToolkit::getMcTrackContainer() - INFO - Instantiating Container" << endl;
  mcTrackContainer = new StiTrackContainer();
  return mcTrackContainer;
}


StiDetectorFinder    * StiDefaultToolkit::getDetectorFinder()
{
  if (detectorFinder)
    return detectorFinder;
  detectorFinder = StiDetectorFinder::instance();
  return detectorFinder;
}

StiSeedFinder   * StiDefaultToolkit::getTrackSeedFinder()
{
  if (trackSeedFinder)
    return trackSeedFinder;
  cout << "StiDefaultToolkit::getTrackSeedFinder() - INFO - "; 
  StiIOBroker * ioBroker = getIOBroker();
  if (ioBroker->seedFinderType()==StiIOBroker::kEvaluable) 
    {
      trackSeedFinder = new StiEvaluableTrackSeedFinder(getAssociationMaker(), getHitContainer());
      cout << "instantiating StiEvaluableTrackSeedFinder" << endl;
    }
  else //if (ioBroker->seedFinderType()==StiIOBroker::kComposite)
    {
      trackSeedFinder = new StiCompositeSeedFinder(getTrackFactory(), getHitContainer());
      cout << "instantiating StiCompositeTrackSeedFinder" << endl;
      //trackSeedFinder->setFactory(mtrackfactory);
    }
  return trackSeedFinder;
}

StiTrackFinder       * StiDefaultToolkit::getTrackFinder()
{
  if (trackFinder)
    return trackFinder;
  // only one track finder at this point, no option
  trackFinder = new StiKalmanTrackFinder(this);
  StiTrack::setTrackFinder(trackFinder);
  getTrackFitter();
  return trackFinder;
}

StiTrackFitter       * StiDefaultToolkit::getTrackFitter()
{
  if (trackFitter)
    return trackFitter;
  trackFitter = new StiKalmanTrackFitter();
  StiTrack::setTrackFitter(trackFitter);
  return trackFitter;
}

StiTrackMerger       * StiDefaultToolkit::getTrackMerger()
{
  if (trackMerger)
    return trackMerger;
  trackMerger = new StiLocalTrackMerger(getTrackContainer());
  return trackMerger;
}

StiDisplayManager    * StiDefaultToolkit::getDisplayManager()
{
  return StiRootDisplayManager::instance();
}

StiHitLoader<StEvent,StiGeometryTransform>    * StiDefaultToolkit::getHitLoader()
{
  if (hitLoader)
    return hitLoader;
  StiMasterHitLoader<StEvent,StiGeometryTransform> * loader = new StiMasterHitLoader<StEvent,StiGeometryTransform>(getHitContainer(),getHitFactory(),getGeometryTransform());
  hitLoader = loader;
  loader->addLoader(new StiTpcHitLoader(getHitContainer(),getHitFactory(),getGeometryTransform()));
  loader->addLoader(new StiSvtHitLoader(getHitContainer(),getHitFactory(),getGeometryTransform()));
  return hitLoader;
}

StiHitFiller    * StiDefaultToolkit::getHitFiller()
{
  if (hitFiller)
    return hitFiller;
  hitFiller = new StiHitFiller();//getHitContainer(), getHitFactory());
  hitFiller->addDetector(kTpcId);
  hitFiller->addDetector(kSvtId);
  return hitFiller;
}

StiIOBroker * StiDefaultToolkit::getIOBroker()
{
  if (!ioBroker)
    ioBroker = new StiRootIOBroker();
  return ioBroker;
}

StAssociationMaker * StiDefaultToolkit::getAssociationMaker()
{
  if (!associationMaker)
    cout << "StiDefaultToolkit::getAssociationMaker() - FATAL - associationMaker is NULL, stupid !!!!!!" << endl;
  return associationMaker;
}


void StiDefaultToolkit::setAssociationMaker(StAssociationMaker * a)
{
  associationMaker = a;
}


StiHitErrorCalculator * StiDefaultToolkit::getHitErrorCalculator()
{
  if (!hitErrorCalculator)
    hitErrorCalculator = new StiHitErrorDefault();
  return hitErrorCalculator;
}

