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
#include "../Sti/StiFactoryTypes.h"
#include "../StiGui/StiGuiFactoryTypes.h"
#include "../StiGui/StiRootDrawableHitContainer.h"
#include "../Sti/StiHitContainer.h"
#include "../Sti/StiDetectorContainer.h"
#include "../Sti/StiDetectorFinder.h"
#include "../Sti/StiTrackContainer.h"
#include "../Sti/StiGeometryTransform.h"
#include "../Sti/StiCoordinateTransform.h"
#include "../Sti/StiDetectorFinder.h"
#include "../Sti/StiTrackSeedFinder.h"
#include "../Sti/StiTrackFinder.h"
#include "../Sti/StiTrackFitter.h"
#include "../Sti/StiTrackFilter.h"
#include "../Sti/StiSimpleTrackFilter.h"
#include "../Sti/StiKalmanTrackFitter.h"
#include "../Sti/StiKalmanTrackFinder.h"
#include "../Sti/StiTrackMerger.h"
#include "../Sti/StiCompositeSeedFinder.h"
#include "../Sti/StiLocalTrackMerger.h"
#include "../Sti/StiDisplayManager.h"
#include "../Sti/StiIOBroker.h"
#include "../Sti/StiDynamicTrackFilter.h"
#include "../Sti/StiHitFiller.h"
#include "../StiMaker/StiRootIOBroker.h"
#include "../StiGui/StiRootDisplayManager.h"

#include "../StiEvaluator/StiEvaluator.h"
#include "../StiEvaluator/StiEventAssociator.h"
#include "../Sti/StiEvaluableTrackSeedFinder.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "../Sti/StiHitErrorCalculator.h"

StiDefaultToolkit::StiDefaultToolkit() :
	hitFactory(0),
	trackFactory(0),
	detectorFactory(0),
	trackNodeFactory(0),
	detectorContainer(0),
	hitContainer(0),
	trackContainer(0),
	geometryTransform(0),
	coordinateTransform(0),
	detectorFinder(0),
	trackSeedFinder(0),
	trackFilter(0),
	trackFinder(0),
	trackFitter(0),
	trackMerger(0),
	displayManager(0),
	ioBroker(0),
	hitFiller(0),
	associationMaker(0)
    {
			sInstance = this;
    };

StiDefaultToolkit::~StiDefaultToolkit()
{
	delete hitFactory;
	delete hitContainer;
	delete detectorFactory;
	delete detectorContainer;
	StiDetectorFinder::kill();
	delete trackNodeFactory;
	delete trackContainer;
	StiGeometryTransform::kill();
	StiCoordinateTransform::kill();
	delete trackFactory;
	delete trackSeedFinder;
	delete trackFinder;
	delete trackFilter;
	delete trackFitter;
	delete trackMerger;
	//delete displayManager;
	//delete evaluator;
	//delete eventAssociator;
	delete ioBroker;
};

StiGeometryTransform *StiDefaultToolkit::getGeometryTransform(){
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

StiObjectFactoryInterface<StiHit>* StiDefaultToolkit::getHitFactory()
{
	if (hitFactory)
		return hitFactory;
	hitFactory = new StiHitFactory("HitFactory");
	hitFactory->setIncrementalSize(50000);
	hitFactory->setMaxIncrementCount(10);
	return hitFactory;
}

StiObjectFactoryInterface<StiKalmanTrack>* StiDefaultToolkit::getTrackFactory()
{
	if (trackFactory)
		return trackFactory;
	StiIOBroker * ioBroker = getIOBroker();
	cout << "StiDefaultToolkit::getTrackFactory() - INFO - "; 
	if (ioBroker->useGui())
		{
			if (ioBroker->seedFinderType()==StiIOBroker::kEvaluable)
				{
					trackFactory = new StiRDEvaluableTrackFactory("StiRDEvaluableTrackFactory",50);
					cout << "instantiating StiRDEvaluableTrackFactory" << endl;
				}
			else //if (ioBroker->seedFinderType()==StiIOBroker::kComposite)
				{
					trackFactory = new StiRDKalmanTrackFactory("StiRDKalmanTrackFactory",50);
					cout << "instantiating StiRDKalmanTrackFactory" << endl;
				}
		}
	else // no gui needed
		{	
			if (ioBroker->seedFinderType()==StiIOBroker::kEvaluable)
				{
					trackFactory = new StiEvaluableTrackFactory("StiEvaluableTrackFactory",50);
					cout << "instantiating StiEvaluableTrackFactory" << endl;
				}
			else //if (ioBroker->seedFinderType()==StiIOBroker::kComposite)
				{
					trackFactory = new StiKalmanTrackFactory("StiKalmanTrackFactory",50);
					cout << "instantiating StiKalmanTrackFactory" << endl;
				}
		}
	trackFactory->setIncrementalSize(1000);
	trackFactory->setMaxIncrementCount(200);
	return trackFactory;
}

StiObjectFactoryInterface<StiDetector>* StiDefaultToolkit::getDetectorFactory()
{
	if (detectorFactory)
		return detectorFactory;
	cout << "StiDefaultToolkit::getDetectorFactory() - INFO - "; 
	if (getIOBroker()->useGui())
		{
			detectorFactory = new StiRDDetectorFactory("RDDetectorFactory");
			cout << "instantiating StiRDDetectorFactory" << endl;
		}
	else
		{
			detectorFactory = new StiDetectorFactory("DetectorFactory");
			cout << "instantiating StiDetectorFactory" << endl;
		}
	detectorFactory->setIncrementalSize(1000);
	detectorFactory->setMaxIncrementCount(10);
	detectorFactory->reset();
	return detectorFactory;
}

StiObjectFactoryInterface<StiDetectorNode>* StiDefaultToolkit::getDetectorNodeFactory()
{
	if (detectorNodeFactory)
		return detectorNodeFactory;
	detectorNodeFactory = new StiDetectorNodeFactory("DetectorNodeFactory");
	detectorNodeFactory->setIncrementalSize(1000);
	detectorNodeFactory->setMaxIncrementCount(10);
	detectorNodeFactory->reset();
	return detectorNodeFactory;
}


StiObjectFactoryInterface<StiKalmanTrackNode>* StiDefaultToolkit::getTrackNodeFactory()
{
	if (trackNodeFactory)
		return trackNodeFactory;
	trackNodeFactory = new StiKalmanTrackNodeFactory("KalmanTrackNodeFactory");
	trackNodeFactory->setIncrementalSize(10000);
	trackNodeFactory->setMaxIncrementCount(200);
	trackNodeFactory->reset();
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
	if (trackContainer)
		return trackContainer;
	trackContainer = new StiTrackContainer();
	return trackContainer;
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

StiTrackFilter       * StiDefaultToolkit::getTrackFilter()
{
	if (trackFilter)
	  return trackFilter;
	trackFilter = new StiSimpleTrackFilter();
	//trackFilter = new StiDynamicTrackFilter(getIOBroker());
	return trackFilter;
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

StiHitFiller    * StiDefaultToolkit::getHitFiller()
{
  if (hitFiller)
    return hitFiller;
  hitFiller = new StiHitFiller();//getHitContainer(), getHitFactory());
  hitFiller->addDetector(kTpcId);
  hitFiller->addDetector(kSvtId);
  return hitFiller;
}

/*
  StiEvaluator         * StiDefaultToolkit::getEvaluator(const string& fname)
  {
  if (evaluator)
  return evaluator;
  StiEventAssociator::instance(getAssociationMaker());
  evaluator = StiDefaultEvaluator::instance(fname);
  return evaluator;
  }
  
  StiEvaluator         * StiDefaultToolkit::getEvaluator()
  {
  if (evaluator)
  return evaluator;
  evaluator = StiDefaultEvaluator::instance("Evaluator.root");
  return evaluator;
  }
*/

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

