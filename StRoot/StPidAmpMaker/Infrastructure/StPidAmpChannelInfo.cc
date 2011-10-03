/***************************************************************************
 *
 * $Id: StPidAmpChannelInfo.cc,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             class for identify a channel  
 ***************************************************************************
 *
 * $Log: StPidAmpChannelInfo.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include <float.h>
#include "StPidAmpMaker/Infrastructure/StPidAmpChannelInfo.hh"

//-----------------------------------
StPidAmpChannelInfo::StPidAmpChannelInfo(){

  /* no op */
}
//-----------------------------------
StPidAmpChannelInfo::~StPidAmpChannelInfo(){

  /* no op */
}

//-----------------------------------
StPidAmpChannelInfo::StPidAmpChannelInfo(StPidAmpCutVector cutCollect){
  //in constructor, put cuts into this::name. cast double to float.in name.
  //better to use sth. like nName<<int(pt.begin*100)/100.0;
  //save space for histo title.

   mCutVector=cutCollect;
   makeName();
}



//-----------------------------------
bool StPidAmpChannelInfo::isInChannel(StPidAmpTrk* trk){
// bydefault,
// mCutVector[0] is Nhits cut.
// mCutVector[1] is PtCut
// Never change the order of Cuts in StPidAmpCutVector!!

  bool br=true;

  br=br && ((mCutVector[0]).isInCut(double(trk->nhits())));
  br=br && ((mCutVector[1]).isInCut(trk->pt()));
  
  return br;

}   


//-----------------------------------
void StPidAmpChannelInfo::makeName(){

  strstream tempStream;

  StPidAmpCutIter iter;
  StPidAmpCut theCut;

  for (iter=mCutVector.begin(); iter!=mCutVector.end(); iter++){

  theCut=*iter;

  if (theCut.highEdge()==FLT_MAX) {
  tempStream<<(theCut.name()).c_str()<<int(theCut.lowEdge()*100)/100<<"_"<<"Infinity";
  }else {
    tempStream<<(theCut.name()).c_str()<<int(theCut.lowEdge()*100)/100<<"_"<<int(theCut.highEdge()*100)/100<<"_";
  }

  }

  mName=tempStream.str();

}
//-----------------------------------
