/***************************************************************************
 *
 * $Id: StiDefaultToolkit.cxx,v 2.9 2003/03/31 17:19:26 pruneau Exp $
 *
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
 *
 ***************************************************************************
 *
 * $Log: StiDefaultToolkit.cxx,v $
 * Revision 2.9  2003/03/31 17:19:26  pruneau
 * various
 *
 * Revision 2.8  2003/02/11 10:38:18  andrewar
 * Changed limits for StiHitFactory:: maxIncrements (from 5 to 10).
 *
 */

#include "StiDefaultToolkit.h"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Filter.h"
#include "Sti/Base/Factory.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/Base/VectorizedFactory.h"
#include "Sti/StiMcTrack.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiMasterDetectorBuilder.h"
#include "Sti/StiMasterHitLoader.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiDetectorFinder.h"
#include "Sti/StiTrackContainer.h"
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
#include "Sti/StiDummyVertexFinder.h"
#include "Sti/StiCompositeSeedFinder.h"
#include "StiGui/StiRDLocalTrackSeedFinder.h"
#include "Sti/StiEvaluableTrackSeedFinder.h"
#include "Sti/StiLocalTrackMerger.h"
#include "Sti/StiDisplayManager.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiDetectorGroup.h"
#include "Sti/StiEvaluableTrackSeedFinder.h"
#include "Sti/StiHitErrorCalculator.h"

#include "StiTpc/StiTpcHitLoader.h"
#include "StiSvt/StiSvtHitLoader.h"

#include "StiMaker/RootEditableParameter.h"
#include "StiGui/StiRootDisplayManager.h"
#include "StiGui/StiRootDrawableDetector.h"
#include "StiGui/StiRootDrawableMcTrack.h"
#include "StiGui/StiRootDrawableHitContainer.h"
#include "StiGui/StiRootDrawableKalmanTrack.h"
#include "StiGui/StiRootDrawableStiEvaluableTrack.h"
#include "StiEvaluator/StiEvaluator.h"
//#include "StiEvaluator/StiEventAssociator.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "Sti/StiHitErrorCalculator.h"


StiDefaultToolkit::StiDefaultToolkit()
  :
	_guiEnabled(false),
	_evaluatorEnabled(false),
	_mcEnabled(false),
  _trackFilterFactory(0),
  _parameterFactory(0),
  _hitFactory(0),
  _trackFactory(0),
  _mcTrackFactory(0),
  _detectorFactory(0),
  _trackNodeFactory(0),
  _detectorContainer(0),
  _hitContainer(0),
  _mcHitContainer(0),
  _trackContainer(0),
  _mcTrackContainer(0),
  _detectorFinder(0),
  _trackSeedFinder(0),
  _trackFinder(0),
  _trackFitter(0),
  _trackMerger(0),
	_vertexFinder(0),
  _displayManager(0),
  _hitLoader(0),
  _associationMaker(0)
{
  cout<<"StiDefaultToolkit::StiDefaultToolkit() -I- Started"<<endl;
  Messenger::init(0);
  //Messenger::setRoutingMask(0);
  cout<<"StiDefaultToolkit::StiDefaultToolkit() -I- Done"<<endl;
};

StiDefaultToolkit::~StiDefaultToolkit()
{
  delete _trackFilterFactory;
  delete _hitFactory;
  delete _hitContainer;
  delete _mcHitContainer;
  delete _detectorFactory;
  delete _detectorContainer;
  StiDetectorFinder::kill(); 
  Messenger::kill();
  delete _trackNodeFactory;
  delete _trackContainer;
  delete _mcTrackContainer;
  delete _trackFactory;
  delete _mcTrackFactory;
  delete _parameterFactory;
  delete _trackSeedFinder;
  delete _trackFinder;
  delete _trackFitter;
  delete _trackMerger;
};

Factory< Filter<StiTrack>   >  * StiDefaultToolkit::getTrackFilterFactory()
{
  if (_trackFilterFactory)
    return _trackFilterFactory;
  cout << "StiDefaultToolkit::getTrackFilterFactory() -I- Instantiating StiTrackFilterFactory" << endl;
  _trackFilterFactory = new VectorizedFactory<StiDefaultTrackFilter, 
    Filter<StiTrack>  >("StiDefaultTrackFilterFactory",3,5,2);
  return _trackFilterFactory;
}

Factory<EditableParameter>  * StiDefaultToolkit::getParameterFactory()
{
  if (_parameterFactory)
    return _parameterFactory;
  if (_guiEnabled)
    _parameterFactory = new VectorizedFactory<RootEditableParameter,EditableParameter>("EditableParameterFactory",100,20,10);
  else
    _parameterFactory = new VectorizedFactory<EditableParameter,EditableParameter>("EditableParameterFactory",100,20,10);
  return _parameterFactory;
}

Factory<StiHit>* StiDefaultToolkit::getHitFactory()
{
  if (_hitFactory)
    return _hitFactory;
  _hitFactory = new VectorizedFactory<StiHit,StiHit>("StiHitFactory",50000,20000,10);
  return _hitFactory;
}

Factory<StiKalmanTrack>* StiDefaultToolkit::getTrackFactory()
{
  if (_trackFactory)
    return _trackFactory;
  cout << "StiDefaultToolkit::getTrackFactory() -I- "; 
  if (_guiEnabled)
    {
      if (_evaluatorEnabled)
	  _trackFactory = new VectorizedFactory<StiRootDrawableStiEvaluableTrack,StiKalmanTrack>("StiRDEvaluableTrackFactory",10000,5000,10);
      else 
	  _trackFactory = new VectorizedFactory<StiRootDrawableKalmanTrack,StiKalmanTrack>("StiRDKalmanTrackFactory",10000,5000,10);
    }
  else // no gui needed
    {	
      if (_evaluatorEnabled)
	  _trackFactory = new VectorizedFactory<StiEvaluableTrack,StiKalmanTrack>("StiEvaluableTrackFactory",10000,5000,10);
      else 
	  _trackFactory = new VectorizedFactory<StiKalmanTrack,StiKalmanTrack>("StiKalmanTrackFactory",10000,5000,10);
    }
  return _trackFactory;
}

Factory<StiMcTrack>* StiDefaultToolkit::getMcTrackFactory()
{
  if (_mcTrackFactory)
    return _mcTrackFactory;
  cout << "StiDefaultToolkit::getMcTrackFactory() -I- "; 
  if (_guiEnabled)
      _mcTrackFactory = new VectorizedFactory<StiRootDrawableMcTrack,StiMcTrack>("StiRootDrawableMcTrackFactory",10000,5000,10);
  else // no gui needed
      _mcTrackFactory = new VectorizedFactory<StiMcTrack,StiMcTrack>("StiMcTrackFactory",10000,5000,10);
  return _mcTrackFactory;
}

Factory<StiDetector>* StiDefaultToolkit::getDetectorFactory()
{
  if (_detectorFactory)
    return _detectorFactory;
  cout << "StiDefaultToolkit::getDetectorFactory() -I- Instantiating Detector Factory"; 
  if (_guiEnabled)
    _detectorFactory = new VectorizedFactory<StiRootDrawableDetector,StiDetector>("StiRDDetectorFactory",2000,500,10);
  else
    _detectorFactory = new VectorizedFactory<StiDetector,StiDetector>("StiDetectorFactory",2000,500,10);
  return _detectorFactory;
}

Factory< StiCompositeTreeNode<StiDetector>  >* StiDefaultToolkit::getDetectorNodeFactory()
{
  if (_detectorNodeFactory)
    return _detectorNodeFactory;
  _detectorNodeFactory = new VectorizedFactory< StiCompositeTreeNode<StiDetector>  , 
    StiCompositeTreeNode<StiDetector>  >(" StiCompositeTreeNode<StiDetector>Factory",1000,500,10);
  return _detectorNodeFactory;
}


Factory<StiKalmanTrackNode>* StiDefaultToolkit::getTrackNodeFactory()
{
  if (_trackNodeFactory)
    return _trackNodeFactory;
  _trackNodeFactory = new VectorizedFactory<StiKalmanTrackNode,StiKalmanTrackNode>("StiKalmanTrackNodeFactory",20000,20000,50);
  StiKalmanTrack::setKalmanTrackNodeFactory(_trackNodeFactory);
  return _trackNodeFactory;	
}


void StiDefaultToolkit::add(StiDetectorGroup<StEvent,StMcEvent>* detectorGroup)
{
  //_detectorGroups.push_back(detectorGroup);
  StiMasterHitLoader<StEvent,StMcEvent,StiDetectorBuilder> * masterLoader;
  masterLoader = static_cast<StiMasterHitLoader<StEvent,StMcEvent,StiDetectorBuilder> *>(getHitLoader());
  StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder> * loader = detectorGroup->getHitLoader();
  if (loader)
    {
      cout << "StiDefaultToolkit::add() -I- Adding hit loader for detector group:"
	   << detectorGroup->getName()<<endl;
      masterLoader->addLoader(loader);
    }
  else
    cout << "StiDefaultToolkit::add() -I- Not adding hit loader for detector group:"<< detectorGroup->getName()<<endl;

  StiMasterDetectorBuilder * masterBuilder = getDetectorBuilder();
  StiDetectorBuilder * builder = detectorGroup->getDetectorBuilder();
  if (builder)
    {
      cout << "StiDefaultToolkit::add() -I- Adding builder for detector group:"<< detectorGroup->getName()<<endl;
      masterBuilder->addBuilder(builder);
    }
  else
    cout << "StiDefaultToolkit::add() -I- Not adding builder for detector group:"<< detectorGroup->getName()<<endl;
}

StiMasterDetectorBuilder * StiDefaultToolkit::getDetectorBuilder()
{  
  if (_detectorBuilder)
    return _detectorBuilder;
  _detectorBuilder = new StiMasterDetectorBuilder();
  return _detectorBuilder;
}

StiDetectorContainer  * StiDefaultToolkit::getDetectorContainer()
{
  if (_detectorContainer)
    return _detectorContainer;
  _detectorContainer = StiDetectorContainer::instance();
  _detectorContainer->build(getDetectorBuilder());
  _detectorContainer->reset();
  return _detectorContainer;
}

StiHitContainer       * StiDefaultToolkit::getHitContainer()
{
  if (_hitContainer)
    return _hitContainer;
  cout << "StiDefaultToolkit::getHitContainer() -I- "; 
  if (_guiEnabled)
    {
      _hitContainer = new StiRootDrawableHitContainer();
      cout << "instantiating StiRootDrawableHitContainer" << endl;
    }
  else 
    {
      _hitContainer = new StiHitContainer();			
      cout << "instantiating StiHitContainer" << endl;
    }
  return _hitContainer;
}


StiHitContainer       * StiDefaultToolkit::getMcHitContainer()
{
  if (_mcHitContainer)
    return _mcHitContainer;
  cout << "StiDefaultToolkit::getMcHitContainer() -I- "; 
  if (_guiEnabled)
    {
      _mcHitContainer = new StiRootDrawableHitContainer();
      cout << "instantiating StiRootDrawableHitContainer" << endl;
    }
  else 
    {
      _mcHitContainer = new StiHitContainer();			
      cout << "instantiating StiHitContainer" << endl;
    }
  return _mcHitContainer;
}


StiTrackContainer     * StiDefaultToolkit::getTrackContainer()
{	
  cout << "StiDefaultToolkit::getTrackContainer() -I- Starting" << endl;
  if (_trackContainer)
    return _trackContainer;
  cout << "StiDefaultToolkit::getTrackContainer() -I- Instantiating Container" << endl;
  _trackContainer = new StiTrackContainer();
  return _trackContainer;
}

StiTrackContainer     * StiDefaultToolkit::getMcTrackContainer()
{	
  cout << "StiDefaultToolkit::getMcTrackContainer() -I- Starting" << endl;
  if (_mcTrackContainer)
    return _mcTrackContainer;
  cout << "StiDefaultToolkit::getMcTrackContainer() -I- Instantiating Container" << endl;
  _mcTrackContainer = new StiTrackContainer();
  return _mcTrackContainer;
}


StiDetectorFinder    * StiDefaultToolkit::getDetectorFinder()
{
  if (_detectorFinder)
    return _detectorFinder;
  _detectorFinder = StiDetectorFinder::instance();
  return _detectorFinder;
}

StiTrackSeedFinder   * StiDefaultToolkit::getTrackSeedFinder()
{
  if (_trackSeedFinder)
    return _trackSeedFinder;
  cout << "StiDefaultToolkit::getTrackSeedFinder() -I- "; 
  if (_evaluatorEnabled)
    {
      cout << "instantiating StiEvaluableTrackSeedFinder" << endl;
      _trackSeedFinder = new StiEvaluableTrackSeedFinder("EvaluableTrackSeedFinder",
																												 getTrackFactory(),
																												 getHitContainer(),
																												 getDetectorContainer(),
																												 getAssociationMaker());
    }
  else 
    {
			//StiCompositeSeedFinder * compositeTrackSeedFinder;
			//compositeTrackSeedFinder = new StiCompositeSeedFinder("CompositeTrackSeedFinder",
			//																											getTrackFactory(),
			//																											getHitContainer(), 
			//																											getDetectorContainer());  
			//StiTrackSeedFinder * trackSeedFinder;																																					
			if (_guiEnabled)
				{
					cout << "instantiating StiRDLocalTrackSeedFinder" << endl;
					_trackSeedFinder = new StiRDLocalTrackSeedFinder("RDLocalTrackSeedFinder",
																													 getTrackFactory(),
																													 getHitContainer(), 
																													 getDetectorContainer());  
				}
			else
				{
					cout << "instantiating StiLocalTrackSeedFinder" << endl;
					_trackSeedFinder = new StiLocalTrackSeedFinder("LocalTrackSeedFinder",
																												 getTrackFactory(),
																												 getHitContainer(), 
																												 getDetectorContainer());  
					//compositeTrackSeedFinder->Vectorized<StiTrackSeedFinder>::add(trackSeedFinder);
					//compositeTrackSeedFinder->reset();
					//_trackSeedFinder = compositeTrackSeedFinder;
				}
    }
  return _trackSeedFinder;
}

StiTrackFinder       * StiDefaultToolkit::getTrackFinder()
{
  if (_trackFinder)
    return _trackFinder;
  // only one track finder at this point, no option
  _trackFinder = new StiKalmanTrackFinder(this);
  StiTrack::setTrackFinder(_trackFinder);
  getTrackFitter();
  return _trackFinder;
}

StiTrackFitter       * StiDefaultToolkit::getTrackFitter()
{
  if (_trackFitter)
    return _trackFitter;
  _trackFitter = new StiKalmanTrackFitter();
  StiTrack::setTrackFitter(_trackFitter);
  return _trackFitter;
}

StiTrackMerger       * StiDefaultToolkit::getTrackMerger()
{
  if (_trackMerger)
    return _trackMerger;
  _trackMerger = new StiLocalTrackMerger(getTrackContainer());
  return _trackMerger;
}

StiVertexFinder * StiDefaultToolkit::getVertexFinder()
{
	cout << "StiDefaultToolkit::getVertexFinder() -I- Started"<<endl;
  if (_vertexFinder)
    return _vertexFinder;
  _vertexFinder = new StiDummyVertexFinder("StEventVertex");
  return _vertexFinder;
}

StiDisplayManager    * StiDefaultToolkit::getDisplayManager()
{
  return StiRootDisplayManager::instance();
}

StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder>    * StiDefaultToolkit::getHitLoader()
{
  if (_hitLoader)
    return _hitLoader;
  _hitLoader = new StiMasterHitLoader<StEvent,StMcEvent,StiDetectorBuilder>("StarHitLoader",
									    getHitContainer(),
									    getMcHitContainer(),
									    getHitFactory(),
									    0);
  return _hitLoader;
}

StAssociationMaker * StiDefaultToolkit::getAssociationMaker()
{
  if (!_associationMaker)
    throw runtime_error(" StiDefaultToolkit::getAssociationMaker() - FATAL - _associationMaker==0");
  return _associationMaker;
}

void StiDefaultToolkit::setAssociationMaker(StAssociationMaker * a)
{
  _associationMaker = a;
  if (!_associationMaker)
    throw runtime_error(" StiDefaultToolkit::getAssociationMaker() - FATAL - _associationMaker==0");
}


void StiDefaultToolkit::setGuiEnabled(bool guiEnabled)
{
	_guiEnabled = guiEnabled;
}

bool StiDefaultToolkit::isGuiEnabled() const
{
	return _guiEnabled;
}

void StiDefaultToolkit::setMcEnabled(bool mcEnabled)
{
	_mcEnabled = mcEnabled;
}

bool StiDefaultToolkit::isMcEnabled() const
{
	return _mcEnabled;
}

void StiDefaultToolkit::setEvaluatorEnabled(bool evaluatorEnabled)
{
	_evaluatorEnabled = evaluatorEnabled;
}

bool StiDefaultToolkit::isEvaluatorEnabled() const
{
	return _evaluatorEnabled;
}

