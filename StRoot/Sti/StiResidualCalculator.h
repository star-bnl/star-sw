//StiResidualMaker.h
/***************************************************************************
 *
 * $Id: StiResidualCalculator.h,v 2.2 2003/04/29 18:48:33 pruneau Exp $
 *
 * Author: Andrew Rose, Wayne State University, October 2002
 ***************************************************************************
 * $Log: StiResidualCalculator.h,v $
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


#define TRACKMESSENGER *(Messenger::instance(MessageType::kResidualMessage))

#ifndef StiResidualMaker_HH
#define StiResidualMaker_HH

//forward declarations
class TH3D;
class TH2D;
class StiHit;
class StiTrack;
class StiKalmanTrack;
class StiNeverActiveFunctor;

#include "StDetectorId.h"
#include "Sti/StiResiduals.h"

class StiResidualCalculator: public StiResiduals
{
   public:
     StiResidualCalculator(StiHitContainer*, StiDetectorBuilder*);
     ~StiResidualCalculator(){/*noop*/};

     void initDetector(StiDetectorBuilder*);

     void calcResiduals(StiTrackContainer*);

     void Write(char* outfile);

   private:
     int  Init();
     int  trackResidue(const StiTrack *track);
     int  trackResidue(const StiKalmanTrack *track);
     void FillHist(double z, double y,
		   double cross, double dip,
		   double dz, double dy);
     void fillTrackHist(double cross, double dip, double pt, double drift);


     StDetectorId mDetector;
     vector<StiDetector*> candidates;
     StiHitContainer * candidateHits;
     StiNeverActiveFunctor * isNotActiveFunc;

     //Residual Hists  
     TH3D *mYResidualCrossDip;
     TH3D *mYResidualZY;
     TH3D *mZResidualCrossDip;
     TH3D *mZResidualZY;
     TH3D *mResidualCrossDip;
     TH3D *mResidualZY;

     TH1D *mCross;
     TH1D *mDip;
     TH1D *mPt;
     TH1D *mDrift;

     TH2D *mDetectorHist;

};
  

#endif
