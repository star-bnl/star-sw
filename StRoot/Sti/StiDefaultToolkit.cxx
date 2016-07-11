/***************************************************************************
 *
 * $Id: StiDefaultToolkit.cxx,v 2.47 2015/12/20 01:06:39 fisyak Exp $
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
 * Revision 2.47  2015/12/20 01:06:39  fisyak
 * Merge
 *
 * Revision 2.46  2015/08/28 23:57:53  perev
 * Cleanup
 *
 * Revision 2.45  2015/07/28 00:43:57  perev
 * Added StiKNNSeedFinder. There is no direct dependency, new made by CINT
 *
 * Revision 2.44  2015/07/09 17:22:54  jeromel
 * Back-step from StiKNNSeedFinder / will branch
 *
 * Revision 2.43  2015/07/07 14:52:45  perev
 * Added selection of KNN Seed finder
 *
 * Revision 2.42  2009/03/18 19:55:39  fisyak
 * remove StiDetectorFinder class
 *
 * Revision 2.41  2009/03/16 13:50:14  fisyak
 * Move out all Sti Chairs into StDetectorDb
 *
 * Revision 2.40  2008/02/29 16:28:00  fisyak
 * Fix unassigned variable (_trackNodeInfFactory), synchronize name with BigFullChain.h
 *
 * Revision 2.39  2007/04/16 22:45:34  perev
 * endl added
 *
 * Revision 2.38  2006/12/18 01:26:32  perev
 * StiNodeInf factory added
 *
 * Revision 2.37  2006/12/06 00:47:40  fine
 * rename StiGeomMake to StiDetectorVolume
 *
 * Revision 2.36  2006/10/16 20:30:42  fisyak
 * Clean dependencies from Sti useless classes
 *
 * Revision 2.35  2006/07/19 18:39:22  perev
 * Max NHits=2000000
 *
 * Revision 2.34  2006/05/31 03:59:04  fisyak
 * Add Victor's dca track parameters, clean up
 *
 * Revision 2.33  2006/04/07 17:39:49  perev
 * Cleanup: redundant comments removed
 *
 * Revision 2.31  2005/12/07 23:13:31  perev
 * last traces of StiMaker and associated maker removed
 *
 * Revision 2.30  2005/10/26 21:54:10  fisyak
 * Remove dead classes, gid rid off dependencies from StMcEvent and StiGui
 *
 * Revision 2.29  2005/10/06 19:36:38  fisyak
 * Clean up
 *
 * Revision 2.28  2005/08/09 15:22:41  perev
 * Add new factory for Node extention
 *
 * Revision 2.27  2005/07/20 17:32:39  perev
 * IdTruth
 *
 * Revision 2.26  2005/05/31 17:12:02  perev
 * Fast delete used
 *
 * Revision 2.25  2005/05/12 18:31:02  perev
 * New factory IMP
 *
 * Revision 2.24  2004/10/08 14:50:11  pruneau
 * changed hit factory allocation
 *
 * Revision 2.23  2004/09/15 21:34:40  pruneau
 * Increased maximum allocation for track factory.
 *
 * Revision 2.22  2004/08/12 14:06:19  pruneau
 * doubled StiKalmanTrackNode factory allocation
 *
 * Revision 2.21  2004/03/18 17:09:11  calderon
 * Change the StiXXXVertexFinder from StiDummy to StiStar for Y2004 chain,
 * as per Lee's example.
 *
 * Revision 2.20  2004/02/24 01:59:46  jeromel
 * Commented out include of disappeared .h
 *
 * Revision 2.19  2004/02/21 19:17:38  pruneau
 * removing reference to evaluable tracks and finder
 *
 * Revision 2.18  2004/02/21 18:28:29  pruneau
 * Updates to comply to changes in interfaces
 *
 * Revision 2.17  2003/09/21 02:19:28  perev
 * several initializations to 0 added
 *
 * Revision 2.16  2003/05/07 03:06:32  pruneau
 * *** empty log message ***
 *
 * Revision 2.15  2003/05/06 15:36:36  mmiller
 * Committing changes to turn on multiple regions (StiPlacement::StiRegion -> kMidRapidity, kForwardRapidity, etc).
 * Also added a point to StiToolkit for StiMaker.  This allows for the req. GetDataSet calls in the FTPC code.
 * Not so elegant...
 *
 * Revision 2.14  2003/04/30 15:39:32  pruneau
 * Integrating StiResidual in main stream Sti
 *
 * Revision 2.13  2003/04/11 16:51:53  pruneau
 * various fixes
 *
 * Revision 2.12  2003/04/10 14:53:03  pruneau
 * removing obsolete files and classes
 *
 * Revision 2.11  2003/04/10 12:10:08  pruneau
 * Changed StiMaker and Default Toolkit to accomodate the new Event Display
 *
 * Revision 2.10  2003/04/09 21:16:19  andrewar
 * Changed limits for StiHitFactory: maxIncrements (from 10 to 20)
 *
 * Revision 2.9  2003/03/31 17:19:26  pruneau
 * various
 *
 * Revision 2.8  2003/02/11 10:38:18  andrewar
 * Changed limits for StiHitFactory:: maxIncrements (from 5 to 10).
 *
 */

#include "TSystem.h"
#include "TROOT.h"
#include "StiDefaultToolkit.h"
#include "Sti/Base/Filter.h"
#include "Sti/Base/Factory.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/Base/StiFactory.h"
#include "Sti/StiHit.h"
#include "Sti/StiHitContainer.h"
#include "Sti/StiMasterDetectorBuilder.h"
#include "Sti/StiMasterHitLoader.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorContainer.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiLocalTrackSeedFinder.h"
#include "Sti/StiTrackFinder.h"
#include "Sti/StiTrackFitter.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiTrack.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiKalmanTrackFitter.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiTrackMerger.h"
#include "Sti/StiStarVertexFinder.h"
//#include "Sti/StiCompositeSeedFinder.h"
#include "Sti/StiLocalTrackMerger.h"
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiDetectorGroup.h"
#include "Sti/StiDetectorGroups.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StiTpc/StiTpcHitLoader.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"

#include "StEvent/StHit.h"
#include "StarClassLibrary/StMCTruth.h"

using namespace std;
//______________________________________________________________________________
StiDefaultToolkit::StiDefaultToolkit()
  :
  _evaluatorEnabled(false),
  _trackFilterFactory(0),
  _parameterFactory(0),
  _hitFactory(0),
  _trackFactory(0),
  _detectorFactory(0),
  _detectorNodeFactory(0),
  _trackNodeFactory(0),
  _trackNodeExtFactory(0),
  _trackNodeInfFactory(0),
  _detectorBuilder(),
  _detectorContainer(0),
  _detectorGroups(0),
  _hitContainer(0),
  _trackContainer(0),
  _trackSeedFinder(0),
  _trackFinder(0),
  _trackFitter(0),
  _trackMerger(0),
  _vertexFinder(0),
  _hitLoader(0),
  _loaderHitFilter(0),
  _loaderTrackFilter(0),
  _finderTrackFilter(0)
{
  cout<<"StiDefaultToolkit::StiDefaultToolkit() -I- Started"<<endl;
  _detectorGroups = new StiDetectorGroups("StarDetectorGroups","StarDetectorGroups");
  cout<<"StiDefaultToolkit::StiDefaultToolkit() -I- Done"<<endl;
};

//______________________________________________________________________________
StiDefaultToolkit::~StiDefaultToolkit()
{
  delete _trackFilterFactory;
  delete _hitFactory;
  delete _hitContainer;
  delete _detectorFactory;
  delete _detectorContainer;
  delete _trackNodeFactory;
  delete _trackNodeExtFactory;
  delete _trackNodeInfFactory;
  delete _trackContainer;
  delete _trackFactory;
  delete _parameterFactory;
  delete _trackSeedFinder;
  delete _trackFinder;
  delete _trackFitter;
  delete _trackMerger;
}

//______________________________________________________________________________
Factory< Filter<StiTrack>   >  * StiDefaultToolkit::getTrackFilterFactory()
{
  if (_trackFilterFactory)
    return _trackFilterFactory;
  cout << "StiDefaultToolkit::getTrackFilterFactory() -I- Instantiating StiTrackFilterFactory" << endl;
  _trackFilterFactory = StiFactory<StiDefaultTrackFilter, 
    Filter<StiTrack>  >::myInstance();
  return _trackFilterFactory;
}

//______________________________________________________________________________
Factory<EditableParameter>  * StiDefaultToolkit::getParameterFactory()
{
  if (_parameterFactory)
    return _parameterFactory;
    _parameterFactory = StiFactory<EditableParameter,EditableParameter>::myInstance();
  return _parameterFactory;
}

//______________________________________________________________________________
Factory<StiHit>* StiDefaultToolkit::getHitFactory()
{
  if (_hitFactory) return _hitFactory;
  _hitFactory = StiFactory<StiHit,StiHit>::myInstance();
  _hitFactory->setFastDelete();
  _hitFactory->setMaxIncrementCount(2000000);
  return _hitFactory;
}

//______________________________________________________________________________
Factory<StiKalmanTrack>* StiDefaultToolkit::getTrackFactory()
{
  if (_trackFactory) return _trackFactory;
  cout << "StiDefaultToolkit::getTrackFactory() -I- "; 
      _trackFactory = StiFactory<StiKalmanTrack,StiKalmanTrack>::myInstance();
      _trackFactory->setFastDelete();
  return _trackFactory;
}


//______________________________________________________________________________
Factory<StiDetector>* StiDefaultToolkit::getDetectorFactory()
{
  if (_detectorFactory)
    return _detectorFactory;
  cout << "StiDefaultToolkit::getDetectorFactory() -I- Instantiating Detector Factory"<<endl; 
    _detectorFactory = StiFactory<StiDetector,StiDetector>::myInstance();
  return _detectorFactory;
}

//______________________________________________________________________________
Factory< StiCompositeTreeNode<StiDetector>  >* StiDefaultToolkit::getDetectorNodeFactory()
{
  if (_detectorNodeFactory)
    return _detectorNodeFactory;
  _detectorNodeFactory = StiFactory< StiCompositeTreeNode<StiDetector>  , 
    StiCompositeTreeNode<StiDetector>  >::myInstance();
  return _detectorNodeFactory;
}


//______________________________________________________________________________
Factory<StiKalmanTrackNode>* StiDefaultToolkit::getTrackNodeFactory()
{
  if (_trackNodeFactory)
    return _trackNodeFactory;
  _trackNodeFactory = StiFactory<StiKalmanTrackNode,StiKalmanTrackNode>::myInstance();
  _trackNodeFactory->setMaxIncrementCount(4000000);
  _trackNodeFactory->setFastDelete();
  StiKalmanTrack::setKalmanTrackNodeFactory(_trackNodeFactory);
  return _trackNodeFactory;	
}

//______________________________________________________________________________
Factory<StiNodeExt>* StiDefaultToolkit::getTrackNodeExtFactory()
{
  if (_trackNodeExtFactory)
    return _trackNodeExtFactory;
  _trackNodeExtFactory= StiFactory<StiNodeExt,StiNodeExt>::myInstance();
  _trackNodeExtFactory->setMaxIncrementCount(4000000);
  _trackNodeExtFactory->setFastDelete();
  return _trackNodeExtFactory;	
}
//______________________________________________________________________________
Factory<StiNodeInf>* StiDefaultToolkit::getTrackNodeInfFactory()
{
  if (_trackNodeInfFactory)
    return _trackNodeInfFactory;
  _trackNodeInfFactory= StiFactory<StiNodeInf,StiNodeInf>::myInstance();
  _trackNodeInfFactory->setMaxIncrementCount(4000000);
  _trackNodeInfFactory->setFastDelete();
  return _trackNodeInfFactory;	
}

//______________________________________________________________________________
StiDetectorGroups  *StiDefaultToolkit::getDetectorGroups()
{
  return _detectorGroups;
}


//______________________________________________________________________________
void StiDefaultToolkit::add(StiDetectorGroup<StEvent>* detectorGroup)
{
  _detectorGroups->push_back((StiGenericDetectorGroup *)detectorGroup);
  StiMasterHitLoader<StEvent,StiDetectorBuilder> * masterLoader;
  masterLoader = static_cast<StiMasterHitLoader<StEvent,StiDetectorBuilder> *>(getHitLoader());
  StiHitLoader<StEvent,StiDetectorBuilder> * loader = detectorGroup->getHitLoader();
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
      masterBuilder->add(builder);
    }
  else
    cout << "StiDefaultToolkit::add() -I- Not adding builder for detector group:"<< detectorGroup->getName()<<endl;
}

//______________________________________________________________________________
StiMasterDetectorBuilder * StiDefaultToolkit::getDetectorBuilder()
{  
  if (_detectorBuilder)
    return _detectorBuilder;
  _detectorBuilder = new StiMasterDetectorBuilder(true);
  return _detectorBuilder;
}

//______________________________________________________________________________
StiDetectorContainer  * StiDefaultToolkit::getDetectorContainer()
{
  if (_detectorContainer)
    return _detectorContainer;
  _detectorContainer = new StiDetectorContainer("DetectorContainer","Detector Container", getDetectorBuilder());
  //  _detectorContainer->build(getDetectorBuilder());
  //_detectorContainer->reset();
  return _detectorContainer;
}

//______________________________________________________________________________
StiHitContainer       * StiDefaultToolkit::getHitContainer()
{
  if (_hitContainer)
    return _hitContainer;
  _hitContainer = new StiHitContainer("HitContainer","Reconstructed Hits", getHitFactory() );
  return _hitContainer;
}



//______________________________________________________________________________
StiTrackContainer     * StiDefaultToolkit::getTrackContainer()
{	
  if (_trackContainer)
    return _trackContainer;
  _trackContainer = new StiTrackContainer("TrackContainer","Reconstructed Tracks");
  return _trackContainer;
}
//______________________________________________________________________________
StiTrackFinder   * StiDefaultToolkit::getTrackSeedFinder()
{
  if (_trackSeedFinder)
    return _trackSeedFinder;
  if (!gSystem->Getenv("StiKNN")) {

  _trackSeedFinder = new StiLocalTrackSeedFinder("LocalTrackSeedFinder",
						 "Local Track Seed Finder",
						 getTrackFactory(),
						 getHitContainer(), 
						 getDetectorContainer());  
  } else {
  _trackSeedFinder  =   (StiTrackFinder*)gROOT->ProcessLineFast("new StiKNNSeedFinder");
  if ( !_trackSeedFinder) 
     LOG_WARN << "StiKNNSeedFinder not yet integrated" << endm;
  }
  assert(_trackSeedFinder);
  return _trackSeedFinder;
}

//______________________________________________________________________________
StiTrackFinder       * StiDefaultToolkit::getTrackFinder()
{
  if (_trackFinder)
    return _trackFinder;
  _trackFinder = new StiKalmanTrackFinder(this);
  StiTrack::setTrackFinder(_trackFinder);
  getTrackFitter();
  return _trackFinder;
}

//______________________________________________________________________________
StiTrackFitter       * StiDefaultToolkit::getTrackFitter()
{
  if (_trackFitter)
    return _trackFitter;
  _trackFitter = new StiKalmanTrackFitter();
  StiTrack::setTrackFitter(_trackFitter);
  return _trackFitter;
}

//______________________________________________________________________________
StiTrackMerger       * StiDefaultToolkit::getTrackMerger()
{
  if (_trackMerger)
    return _trackMerger;
  _trackMerger = new StiLocalTrackMerger(getTrackContainer());
  return _trackMerger;
}

//______________________________________________________________________________
StiVertexFinder * StiDefaultToolkit::getVertexFinder()
{
  cout << "StiDefaultToolkit::getVertexFinder() -I- Started"<<endl;
  if (_vertexFinder)
    return _vertexFinder;
  _vertexFinder = new StiStarVertexFinder("GenericVertex"); // MCBS, for Y2004 chain
  return _vertexFinder;
}


//______________________________________________________________________________
StiHitLoader<StEvent,StiDetectorBuilder>    * StiDefaultToolkit::getHitLoader()
{
  if (_hitLoader)
    return _hitLoader;
  _hitLoader = new StiMasterHitLoader<StEvent,StiDetectorBuilder>("StarHitLoader",
								  getHitContainer(),
								  getHitFactory(),
								  0);
  return _hitLoader;
}

//______________________________________________________________________________
void StiDefaultToolkit::setEvaluatorEnabled(bool evaluatorEnabled)
{
	_evaluatorEnabled = evaluatorEnabled;
}

//______________________________________________________________________________
bool StiDefaultToolkit::isEvaluatorEnabled() const
{
	return _evaluatorEnabled;
}


//______________________________________________________________________________
EditableFilter<StiHit>   * StiDefaultToolkit::getLoaderHitFilter()
{
  return _loaderHitFilter;
}

//______________________________________________________________________________
EditableFilter<StiTrack> * StiDefaultToolkit::getLoaderTrackFilter()
{
  return _loaderTrackFilter;
}

//______________________________________________________________________________
EditableFilter<StiTrack> * StiDefaultToolkit::getFinderTrackFilter()
{
  return _finderTrackFilter;
}

//______________________________________________________________________________
void StiDefaultToolkit::setLoaderHitFilter(EditableFilter<StiHit>   * loaderHitFilter)
{
  _loaderHitFilter = loaderHitFilter;
}

//______________________________________________________________________________
void StiDefaultToolkit::setLoaderTrackFilter(EditableFilter<StiTrack> * loaderTrackFilter)
{
  _loaderTrackFilter = loaderTrackFilter;
}

//______________________________________________________________________________
void StiDefaultToolkit::setFinderTrackFilter(EditableFilter<StiTrack> * finderTrackFilter)
{
  _finderTrackFilter = finderTrackFilter;
}

//______________________________________________________________________________
int StiDefaultToolkit::getTruth(const StiHit *stiHit)
{
  if (!stiHit->detector()) return 0;
  StHit *stHit = (StHit*)stiHit->stHit();	
  if (!stHit) return 0;
  return StMCTruth(stHit->idTruth(),stHit->qaTruth());
}

  
