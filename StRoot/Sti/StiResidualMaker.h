//StiResidualMaker.h
/***************************************************************************
 *
 * $Id: StiResidualMaker.h,v 2.2 2003/03/16 21:57:45 andrewar Exp $
 *
 * Author: Andrew Rose, Wayne State University, October 2002
 ***************************************************************************
 * $Log: StiResidualMaker.h,v $
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
class StiHit;
class StiTrack;
class StiKalmanTrack;

#include "StDetectorId.h"

class StiResidualMaker: StiResiduals
{
   public:
     StiResidualMaker(StDetectorId det);
     virtual ~StiResidualMaker(){};
 
     int calcResiduals(StiTrackContainer*);

     void Write(char* outfile);

   private:
     int  Init();
     int  trackResidue(const StiTrack *track);
     int  trackResidue(const StiKalmanTrack *track);
     void FillHist(StiKalmanTrackNode *node);

     StDetectorId mDetector;


     //Residual Hists  
     TH3D *mYResidualCrossDip;
     TH3D *mYResidualZY;
     TH3D *mZResidualCrossDip;
     TH3D *mZResidualZY;
     TH3D *mResidualCrossDip;
     TH3D *mResidualZY;
};
  

#endif
