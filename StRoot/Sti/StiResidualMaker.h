//StiResidualMaker.h
/***************************************************************************
 *
 * $Id: StiResidualMaker.h,v 1.1 2002/10/16 18:41:37 andrewar Exp $
 *
 * Author: Andrew Rose, Wayne State University, October 2002
 ***************************************************************************
 * $Log: StiResidualMaker.h,v $
 * Revision 1.1  2002/10/16 18:41:37  andrewar
 * Initial commit. Derived class declaration for residual maker.
 *
 */

//forward declarations
class TH3D;
class StiHit;
class StiTrack;
class StiKalmanTrack;

class StiResidualMaker: StiResiduals
{
   public:
     StiResidualMaker(StDetectorId det);
     ~StiResidualMaker(){/*noop, but should delete hists*/}
 

     int calcResiduals(StiTrackContainer*);

     void Write(char* outfile);

   private:
     int  Init();
     int  trackResidue(const StiKalmanTrack *track);
     void FillHist(StiKalmanTrackNode *node);

     StDetectorId mDetector;


     //Residual Hists  
     TH3D *mYResidualCrossDipZ;
     TH3D *mYResidualZYRow;
     TH3D *mZResidualCrossDipZ;
     TH3D *mZResidualZYRow;
};
  

