/***************************************************************************
 *
 * $Id: StPidAmpCut.cc,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             Be used by StPidAmpChannelInfo and StPidAmpWindow
 *             In mName, 
 *             "N" means Nhits cut.
 *             "P" means Pt cut
 *             "R" means rigidity cut.         
 ***************************************************************************
 *
 * $Log: StPidAmpCut.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpMaker/Infrastructure/StPidAmpCut.hh"
#include "math.h"
//------------------------------------
StPidAmpCut::StPidAmpCut(){

  /* no op */
}

//------------------------------------
StPidAmpCut::~StPidAmpCut(){

  /* no-op */
}


//------------------------------------
StPidAmpCut::StPidAmpCut(string s, double low, double high){
    mName=s;
    mLowEdge=low;
    mHighEdge=high;
    mBeActive=true;
}
        
//------------------------------------
double StPidAmpCut::length(){
    return fabs(mHighEdge-mLowEdge);
}
//------------------------------------
void StPidAmpCut::setCut(double low, double high){
    mLowEdge=low;
    mHighEdge=high;
}

//------------------------------------
bool StPidAmpCut::isInCut(double x){
    if (x>mLowEdge && x<=mHighEdge) return true;
    else return false;
}

//------------------------------------
void StPidAmpCut::enable(){
     mBeActive=true;
}

//------------------------------------
void StPidAmpCut::disable(){
     mBeActive=false;
}
