//StiResidualMaker.h
/***************************************************************************
 *
 * $Id: StiResidualCalculator.h,v 2.6 2004/02/21 18:27:42 pruneau Exp $
 *
 * Author: Andrew Rose, Wayne State University, October 2002
 ***************************************************************************
 * $Log: StiResidualCalculator.h,v $
 * Revision 2.6  2004/02/21 18:27:42  pruneau
 * Updates to comply with changes made in abstract interfaces.
 *
 * Revision 2.5  2003/09/07 03:49:08  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 2.4  2003/06/10 16:23:30  andrewar
 * Added functions to residual calculator. Added parallel hist set for
 * different detector layers.
 *
 * Revision 2.3  2003/04/30 15:38:57  pruneau
 * Integrating StiResidualCalculator into the main stream.
 *
 * Revision 2.2  2003/04/29 18:48:33  pruneau
 * *** empty log message ***
 *
 * Revision 2.1  2003/04/29 14:59:02  andrewar
 * Modified to conform to naming convention. Added
 * initDetectors(StiDetectorBuilder) to switch desired detectors 'off' during
 * tracking, so residual will be unbiased.
 *
 * Revision 2.2  2003/03/16 21:57:45  andrewar
 * Fixed filling bug, removed couts and improved functionality
 *
 * Revision 2.1  2003/03/16 16:52:12  andrewar
 * Add histograms, removed redundant checks on hits
 *
 * Revision 2.0  2002/12/10 21:59:58  pruneau
 * Introducing version 2.0
 *
 * Revision 1.2  2002/11/15 17:07:20  andrewar
 * Fixed bug with virtual base class destructor.
 *
 * Revision 1.1  2002/10/16 18:41:37  andrewar
 * Initial commit. Derived class declaration for residual maker.
 *
 */


#ifndef StiResidualMaker_HH
#define StiResidualMaker_HH

//forward declarations
class TH3D;
class TH2D;
class StiHit;
class StiTrack;
class StiKalmanTrack;
class StiNeverActiveFunctor;
class HistogramGroup;

#include "StDetectorId.h"
#include "Sti/StiResiduals.h"
#include "Sti/Base/HistogramGroup.h"
#include "Sti/StiHitContainer.h"

class StiResidualCalculator: public StiResiduals, public HistogramGroup
{
   public:
     StiResidualCalculator(StiHitContainer*);
     ~StiResidualCalculator(){/*noop*/};
     void initialize(StiDetectorBuilder*);
     void calcResiduals(StiTrackContainer*);

   private:
     int  Init();
     void initDetector(StiDetectorBuilder*);
     int  trackResidue(const StiTrack *track);
     int  trackResidue(const StiKalmanTrack *track);
     void NodeResidue(StiKalmanTrackNode iNode, vector<StiHit*> hits, 
		       int histVecOffset);
     void ResidualBackground(StiKalmanTrackNode iNode, vector<StiHit*> hitVec);
     void FillHist(int offset, double z, double y,
		   double cross, double dip,
		   double dz, double dy,
		   double dze, double dye);
     void fillTrackHist(double cross, double dip, double pt, double drift);


     StDetectorId           mDetector;
     vector<StiDetector*>   candidates;
     StiHitContainer *      candidateHits;
     StiNeverActiveFunctor *isNotActiveFunc;

     //Residual Hists  
     vector<TH3D*> mYResidualCrossZ;
     vector<TH3D*> mYResidualDipZ;
     vector<TH3D*> mYDResidualCrossZ;
     vector<TH3D*> mYDResidualDipZ;
     vector<TH3D*> mZResidualCrossZ;
     vector<TH3D*> mZResidualDipZ;
     vector<TH3D*> mZDResidualCrossZ;
     vector<TH3D*> mZDResidualDipZ;
     vector<TH3D*> mResidualCrossDip;
     vector<TH3D*> mResidualZY;

     //Track Hists
     TH1D *mCross;
     TH1D *mDip;
     TH1D *mPt;
     TH1D *mDrift;

     TH1D* _BackgroundZ;
     TH1D* _BackgroundY;
     TH1D* _BackgroundClosestZ;
     TH1D* _BackgroundClosestY;



     TH2D *mDetectorHist;


};
  

#endif
