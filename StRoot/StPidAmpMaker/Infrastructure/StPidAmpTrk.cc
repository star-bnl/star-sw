/***************************************************************************
 *
 * $Id: StPidAmpTrk.cc,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             class of track for Probability(amplitude) PID
 ***************************************************************************
 *
 * $Log: StPidAmpTrk.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpMaker/Infrastructure/StPidAmpTrk.hh"


//---------------------------
StPidAmpTrk::StPidAmpTrk(){

  /* no op */
}
//---------------------------
StPidAmpTrk::~StPidAmpTrk(){

  /* no op */
}



//---------------------------
StPidAmpTrk::StPidAmpTrk(double& rig, double& dedx, int& charge,double& pt, int& nhits, double& dca){

     mRig=rig;
     mDedx=dedx;
     mPt=pt;
     mNhits=nhits;
     mCharge=charge;
     mDca=dca;
}


