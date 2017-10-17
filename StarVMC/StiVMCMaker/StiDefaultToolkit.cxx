/***************************************************************************
 *
 * $Id: StiDefaultToolkit.cxx,v 2.52 2009/08/19 18:08:01 fisyak Exp $
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
 * Revision 2.52  2009/08/19 18:08:01  fisyak
 * Eliminate StiStarVertexFinder
 *
 * Revision 2.51  2009/08/04 18:55:14  fisyak
 * Capitilize method names
 *
 * Revision 2.50  2009/08/02 19:05:36  fisyak
 * Add reference track
 *
 * Revision 2.49  2009/07/28 20:53:22  fisyak
 * Eliminate StiVMC/Base
 *
 * Revision 2.48  2009/07/23 19:40:02  fisyak
 * Remove StiKalmanTrackFinder
 *
 * Revision 2.47  2009/07/20 14:10:25  fisyak
 * Remove *Parameter* and exceptions
 *
 * Revision 2.46  2009/07/20 13:15:46  fisyak
 * Remove Filters
 *
 * Revision 2.45  2009/07/19 20:56:37  fisyak
 * Eliminate StiKalmanTrackFitter
 *
 * Revision 2.44  2009/07/19 20:14:35  fisyak
 * remove abstract classes
 *
 * Revision 2.43  2009/04/15 20:26:53  fisyak
 * Clean ups, use VMC TGeo for detector description, load hits in the central place
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
 * last traces of StiVMCMaker and associated maker removed
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
 * Also added a point to StiToolkit for StiVMCMaker.  This allows for the req. GetDataSet calls in the FTPC code.
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
 * Changed StiVMCMaker and Default Toolkit to accomodate the new Event Display
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

#include "StiDefaultToolkit.h"
#include "StiVMC/StiFactory.h"
#include "StiVMC/StiHit.h"
#include "StHit.h"
#include "StiVMC/StiHitContainer.h"
#include "StiVMC/StiDetector.h"
#include "StiVMC/StiDetectorContainer.h"
#include "StiVMC/StiKalmanTrackContainer.h"
#include "StiVMC/StiLocalTrackSeedFinder.h"
#include "StiVMC/StiKalmanTrack.h"
#include "StiVMC/StiKalmanTrackNode.h"
#include "StiVMC/StiVertexFinder.h"
#include "StDetectorDbMaker/StiHitErrorCalculator.h"
#include "StarClassLibrary/StMCTruth.h"

//______________________________________________________________________________
StiDefaultToolkit::StiDefaultToolkit()  :
  _hitFactory(0),
  _trackFactory(0),
  _detectorFactory(0),
  _detectorNodeFactory(0),
  _trackNodeFactory(0),
  _detectorContainer(0),
  _hitContainer(0),
  _trackContainer(0),
  _trackSeedFinder(0),
  _vertexFinder(0) {};
//______________________________________________________________________________
StiDefaultToolkit::~StiDefaultToolkit()
{
  SafeDelete(_hitFactory);
  SafeDelete(_hitContainer);
  SafeDelete(_detectorFactory);
  SafeDelete(_detectorContainer);
  SafeDelete(_trackNodeFactory);
  SafeDelete(_trackContainer);
  SafeDelete(_trackFactory);
  SafeDelete(_trackSeedFinder);
}
//______________________________________________________________________________
Factory<StiHit>* StiDefaultToolkit::GetHitFactory() {
  StiFactory<StiHit,StiHit> *hitFactory = StiFactory<StiHit,StiHit>::myInstance();
  //  hitFactory->SetFastDelete();
  hitFactory->SetMaxIncrementCount(2000000);
  return hitFactory;
}
//______________________________________________________________________________
Factory<StiKalmanTrack>* StiDefaultToolkit::GetTrackFactory() {
  StiFactory<StiKalmanTrack,StiKalmanTrack>  *trackFactory = StiFactory<StiKalmanTrack,StiKalmanTrack>::myInstance();
  trackFactory->SetFastDelete();
  return trackFactory;
}
//______________________________________________________________________________
Factory<StiDetector>* StiDefaultToolkit::GetDetectorFactory() {
  return StiFactory<StiDetector,StiDetector>::myInstance();
}
//______________________________________________________________________________
Factory< StiDetectorNode  >* StiDefaultToolkit::GetDetectorNodeFactory() {
  return StiFactory< StiDetectorNode, StiDetectorNode >::myInstance();
}


//______________________________________________________________________________
Factory<StiKalmanTrackNode>* StiDefaultToolkit::GetTrackNodeFactory() {
  StiFactory<StiKalmanTrackNode,StiKalmanTrackNode> *trackNodeFactory = StiFactory<StiKalmanTrackNode,StiKalmanTrackNode>::myInstance();
  trackNodeFactory->SetMaxIncrementCount(4000000);
  //  trackNodeFactory->SetFastDelete();
  return trackNodeFactory;	
}

//______________________________________________________________________________
StiDetectorContainer  * StiDefaultToolkit::GetDetectorContainer() {
  return new StiDetectorContainer("DetectorContainer","Detector Container");
}

//______________________________________________________________________________
StiHitContainer       * StiDefaultToolkit::GetHitContainer() {
  return new StiHitContainer("HitContainer","Reconstructed Hits", HitFactory() );
}
//______________________________________________________________________________
StiKalmanTrackContainer     * StiDefaultToolkit::GetTrackContainer() {	
  return new StiKalmanTrackContainer("TrackContainer","Reconstructed Tracks");
}
//______________________________________________________________________________
StiLocalTrackSeedFinder   * StiDefaultToolkit::GetTrackSeedFinder() {
  return new StiLocalTrackSeedFinder("LocalTrackSeedFinder",
				     "Local Track Seed Finder");
}
//______________________________________________________________________________
StiVertexFinder * StiDefaultToolkit::GetVertexFinder() {
  return  new StiVertexFinder("GenericVertex"); // MCBS, for Y2004 chain
}
//______________________________________________________________________________
int StiDefaultToolkit::Truth(const StiHit *stiHit)
{
  if (!stiHit->Detector()) return 0;
  StHit *stHit = (StHit*)stiHit->stHit();	
  if (!stHit) return 0;
  return StMCTruth(stHit->idTruth(),stHit->qaTruth());
}

  
