/***************************************************************************
 *
 * $Id: StPidAmpBGNet.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpBGNet is a net type with mass=1, charge=1
 *             it is for dedx~beta*gamma fitting        
 ***************************************************************************
 *
 * $Log: StPidAmpBGNet.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpBGNet_hh
#define StPidAmpBGNet_hh
#include "TF1.h"

#include "StPidAmpMaker/Infrastructure/StPidAmpNet.hh"

class StPidAmpBGNet : public StPidAmpNet {

public:

  StPidAmpBGNet();
  StPidAmpBGNet(StPidAmpParticle def, StPidAmpChannelInfo channelInfo);

  double dedxAtBandCenter(double rig);
  void   setUp(); //set up slices and paths. shoulb be public.
  void   processNet(StPidAmpTrkVector* trks=0,TH3D* histo=0);
  void   fillBand(); //fill band graph.
  void   fitBand(TH3D* histo);//fit band. put result in mBandParams.
  void   drawNetFittings();
  void   fillNetOut();//fill mNetOut. 
  int    getSliceIndex(double x); 
 
private:


  void bookSlicesInASegment(double theSliceWidth, double theSegBegin, double theSegEnd, double& continuationPoint, int& continuationIdx);

  void    redefineWindow();
  void    fillSliceWidth();





  double  sliceWidth4Window[NWindows4BG];
  int     NSlice4Window[NWindows4BG]; //N of slices in an window
  int     NSliceAccum[NWindows4BG]; //N of total silces in window lower than


  TGraph* mBandGraphLowBetaGamma;
  TGraph* mBandGraphMiddleBetaGamma;
  TF1*    mBandFitFcn;


};



#endif
