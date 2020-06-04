/***************************************************************************
 *
 * $Id: StiDefaultToolkit.cxx,v 2.3 2016/06/30 19:50:26 perev Exp $
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
 * Revision 2.3  2016/06/30 19:50:26  perev
 * StiTrackMerger removed
 *
 * Revision 2.2  2016/06/29 18:03:13  perev
 * Added two seed finders CA & KNN
 *
 * Revision 1.1.2.1  2016/05/24 22:49:17  smirnovd
 * Squashed commit of the following:
 *
 * commit 169aac1e9a4e5291586418783d9b969a1d047035
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 18:07:56 2016 -0400
 *
 *     StiMaker: Included missing header for templated class
 *
 * commit ebf9abbd2d31db0bfb053a5c1e2715c969496b8f
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 17:30:47 2016 -0400
 *
 *     Moved StiDefaultToolkit from StiMaker/ to Sti/ to break circular dependancy
 *
 *     Relevant include paths and LinkDef's have been updated.
 *
 * commit 3dfd849439f6350577937ac042944d9d1b0b0978
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 17:19:33 2016 -0400
 *
 *     StiMaker: Added method to update internal toolkit to StiCA
 *
 * commit 9e34887223d52f7ab3a784bb81879f303ac5d2fc
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 17:11:10 2016 -0400
 *
 *     StiKalmanTrack: Make initialize() virtual
 *
 *     We call it for StiCAKalmanTrack via a base pointer. For example, see
 *
 *     bool StiLocalTrackSeedFinder::fit(StiKalmanTrack* track)
 *
 * commit 04044129cc01bf326261b5934e8feb48fcac9798
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 17:09:36 2016 -0400
 *
 *     StiCAKalmanTrack: [Style] Mark refit() virtual as that's what it is
 *
 * commit fac70613e865d1d3f9312dcac6a3c4bc5270abc6
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 16:25:56 2016 -0400
 *
 *     StiFactory: Include proper dependency
 *
 * commit 45404e1dcdcff8311184663c40cd0f8fe4ef0625
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 19:05:32 2016 -0400
 *
 *     StiCATpcSeedFinder: [Cosmetic] Removed irrelevant code/comments
 *
 * commit 4bb50aac320c58c2fa2c2da47c8875800d92fac0
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 16 21:36:08 2016 -0400
 *
 *     Make use of StiKalmanTrack's static members
 *
 * commit 90c647615138540a1f59810d9259da70f6f162f4
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 16 21:34:17 2016 -0400
 *
 *     StiKalmanTrack: Moved static global entities from file scope to class
 *
 * commit 1105164c6f4c569ee5e2a3ff64fa8132c5f95914
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 16 15:41:50 2016 -0400
 *
 *     StiCAKalmanTrackFinder: Accept pointers to base class StiKalmanTrack
 *
 *     The method does not do anything specific to derived class StiCAKalmanTrack hence
 *     it is safe to accept a pointer to the base class
 *
 * commit 1e7e9f26806bad39cec6e02d83a12e8def577671
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 16 12:38:01 2016 -0400
 *
 *     StiCAKalmanTrack: Added include declaring kTpcId
 *
 * commit ed22761b7dd81a2a3bc9efa55d037541bbaad2bd
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Fri May 13 00:07:18 2016 -0400
 *
 *     StiKalmanTrackFinder: Changed access to protected for private data member
 *
 *     This member is used in derived StiCAKalmanTrackFinder class so allow direct use
 *
 * commit fe547209f89d8d9981aade883fd86fae1ee8b304
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 23:47:14 2016 -0400
 *
 *     StiCATpcSeedFinder: Cast StiKalmanTrack to StiCAKalmanTrack
 *
 *     In StiCA we would like to work with StiCAKalmanTrack's
 *     It is safe to cast to derived class as derived class does not introduce any new
 *     data members
 *
 * commit 2ee090b303f76c2f431173c81b3a54cdbb492682
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Tue May 24 16:50:47 2016 -0400
 *
 *     StiCADefaultToolkit: Modifed config for StiCA to instantiate StiCA specific objects
 *
 * commit 62e6095815e15dad7ddb66f20a8151d0c9b4c7fd
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 16:20:25 2016 -0400
 *
 *     StiCADefaultToolkit: Remove duplicate code belonging to base class
 *
 * commit 05cbe67227dc31778148cd8a92707d940bf77e72
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 15:50:41 2016 -0400
 *
 *     StiCADefaultToolkit: Inherit from StiDefaultToolkit
 *
 * commit c56f4d0a216f905f734d6af2764c9f9c6df41337
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Tue May 24 16:14:35 2016 -0400
 *
 *     StiCAKalmanTrackFinder: Removed duplicate code that exists in parent class StiKalmanTrackFinder
 *
 * commit 6becbb84ed39a587d3c4d01e71f9210c7d683a65
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 23:43:37 2016 -0400
 *
 *     StiCAKalmanTrackFinder inherits from StiKalmanTrackFinder
 *
 * commit a992c34fc7d1642617952053327378cf39f3af0d
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 16 12:35:51 2016 -0400
 *
 *     StiCAKalmanTrack: Removed duplicate code that exists in parent class StiKalmanTrack
 *
 * commit 805f37719570820a95069144dfb9d28356a95e1c
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 16 12:37:10 2016 -0400
 *
 *     StiCAKalmanTrack: Inherit from Sti/ to avoid duplicate code
 *
 *     Conflicts:
 *     	StiCA/StiCAKalmanTrack.h
 *
 * commit e967b188bad7f7cdc0f48ededa58365a1f145697
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 15:42:30 2016 -0400
 *
 *     StiCA: Removed DO_TPCCATRACKER protection
 *
 *     For StiCA we assume DO_TPCCATRACKER is defined
 *
 * commit e80e0659619f2d0369549bfb717639a4a5b02eb7
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 14:49:04 2016 -0400
 *
 *     Sti: Removed DO_TPCCATRACKER protection
 *
 *     For Sti we assume DO_TPCCATRACKER is not defined
 *
 * commit e9df39949dc387f2eab3cb1812ebf99832fafaaf
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 18:13:26 2016 -0400
 *
 *     StiCA: Path of includes corrected
 *
 * commit dbc98816057b09b843e86e6b36ed1c81a7932ea0
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 18:08:46 2016 -0400
 *
 *     StiCA: Classes renamed for consistency
 *
 *     The following substitutions were made:
 *
 *     s/StiKalmanTrackFinder/StiCAKalmanTrackFinder/
 *     s/StiTpcSeedFinder/StiCATpcSeedFinder/
 *     s/StiKalmanTrack/StiCAKalmanTrack/
 *     s/StiTPCCATrackerInterface/StiCATpcTrackerInterface/
 *     s/StiDefaultToolkit/StiCADefaultToolkit/
 *
 * commit b0c26b55a4b491ce0dd92b6da1c4c74a70440c52
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Mon May 23 15:11:48 2016 -0400
 *
 *     StiCA: Renamed StiDefaultToolkit.cxx -> StiCADefaultToolkit
 *
 * commit 4a738dbe2290203cbdc69dc213bd97661693661a
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 15:31:11 2016 -0400
 *
 *     Added in StiCA/ direct copies of corresponding files from Sti/
 *
 * commit 25cf766787a65f6d6b1b2789637941c0dd016b55
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 14:33:45 2016 -0400
 *
 *     Moved StiCA related files from Sti/ to StiCA/
 *
 *     Some files have been renamed. We'll use StiCA prefix for files in StiCA/
 *
 * commit a51e816f2d21645cc4768098a6867b78724e5215
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Thu May 12 19:47:58 2016 -0400
 *
 *     Fixes to catch up with latest changes in Sti
 *
 * commit 27264048a6fd93ff9b03613df04eec9452aa332f
 * Author: Dmitri Smirnov <d.s@plexoos.com>
 * Date:   Tue May 24 12:11:25 2016 -0400
 *
 *     StiKalmanTrack: Change from original code found in "eval"
 *
 *     This change was reverted by Irakli but in fact needed to avoid triggereing of assert
 *     See commit 4d993f28
 *
 * commit 8391c593621c48e775efa453c9b694c46229aba8
 * Author: Irakli Chakaberia <iraklic@rcas6007.rcf.bnl.gov>
 * Date:   Tue Apr 26 11:39:42 2016 -0400
 *
 *     assert removed to allow for multiple use of hit
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
#include "Sti/StiDefaultTrackFilter.h"
#include "Sti/StiDetectorGroup.h"
#include "Sti/StiDetectorGroups.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StiTpc/StiTpcHitLoader.h"
#include "StiSvt/StiSvtHitLoader.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"

#include "StEvent/StHit.h"
#include "StarClassLibrary/StMCTruth.h"


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
  auto *mySeed = new StiLocalTrackSeedFinder("DefaultSeedFinder",
						 "Local Track Seed Finder",
						 getTrackFactory(),
						 getHitContainer(), 
						 getDetectorContainer());  
  assert(mySeed);
  return mySeed;
}
//______________________________________________________________________________
StiTrackFinder   * StiDefaultToolkit::getTrackSeedFinderKNN()
{
  auto *mySeed = (StiTrackFinder*)gROOT->ProcessLineFast("new StiKNNSeedFinder");
  assert(mySeed);
  return mySeed;
}
//______________________________________________________________________________
StiTrackFinder   * StiDefaultToolkit::getTrackSeedFinderCA()
{
  gSystem->Load("TPCCATracker");
  gSystem->Load("StiCA");
  auto *mySeed = (StiTrackFinder*)gROOT->ProcessLineFast("StiCALoader::New()");
  assert(mySeed);
  return mySeed;
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

  
