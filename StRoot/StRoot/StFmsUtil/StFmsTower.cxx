// $Id: StFmsTower.cxx,v 1.3 2016/06/07 15:51:44 akio Exp $
//
// $Log: StFmsTower.cxx,v $
// Revision 1.3  2016/06/07 15:51:44  akio
// Making code better based on Coverity reports
//
// Revision 1.2  2015/10/21 15:58:05  akio
// Code speed up (~x2) by optimizing minimization fuctions and showershape function
// Add option to merge small cells to large, so that it finds cluster at border
// Add option to perform 1photon fit when 2photon fit faield
// Add option to turn on/off global refit
// Moment analysis done without ECUTOFF when no tower in cluster exceed ECUTOFF=0.5GeV
//
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsTower.cxx
 \brief     Implementation of StFmsTower, a simple FMS tower wrapper
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#include "StFmsUtil/StFmsTower.h"

#include "StEvent/StFmsHit.h"
#include "StFmsDbMaker/StFmsDbMaker.h"

namespace FMSCluster {
StFmsTower::StFmsTower()
    : mHit(nullptr), mColumn(-1), mRow(-1), mCluster(-1), mX(0.0), mY(0.0), mE(0.0), mW(0.0) {}

StFmsTower::StFmsTower(StFmsHit* fmsHit)
    : mHit(fmsHit), mColumn(-1), mRow(-1), mCluster(-1), mX(0.0), mY(0.0), mE(0.0), mW(0.0) {}

StFmsTower::~StFmsTower() { }

Bool_t StFmsTower::initialize(StFmsDbMaker* database) {
  if (!mHit || !database) {  // Check for invalid input
    return false;
  }  // if
  // Get row and column from the database
  int det=mHit->detectorId();
  mRow = database->getRowNumber(det, mHit->channel());
  mColumn = database->getColumnNumber(det, mHit->channel());
  mE = mHit->energy();
  mW = (database->getXWidth(det) + database->getYWidth(det))/2.0;
  return mRow > -1 && mColumn > -1;
}

Bool_t StFmsTower::isNeighbor(const StFmsTower& other) const {
    int det0=mHit->detectorId();
    int det1=other.hit()->detectorId();
    //within the same detector
    if(det0==det1){
	return abs(mColumn - other.column()) + abs(mRow - other.row()) == 1;
    }
    //different detector (large and small)
    bool ret=false;
    int rowL,colL,rowS,colS;
    if(det0<det1){
	rowL=mRow;    rowS=other.row();
	colL=mColumn; colS=other.column();
    }else{
	rowS=mRow;    rowL=other.row();
	colS=mColumn; colL=other.column();
    }
    if(rowS>1 && rowS<24 && colS<12){ //inside small cells
	ret=false; 
    }else if(rowL<9 || rowL>26 || colL>9){ //inside large cells
	ret=false; 
    }else if((rowS==1 && rowL==9) || (rowS==24 && rowL==26) ){ //large small boundary at top or bottom
	if((colL-1)/2 == (colS-1)/3){
	    if     ((colL-1)%2==0 && (colS-1)%3==2) {ret=false;}
	    else if((colL-1)%2==1 && (colS-1)%3==0) {ret=false;}
	    else                                    {ret=true;}
	}else{
	    ret=true;
	}                     	    
    }else if(colL==9 && colS==12){ //large small boundary at side
	 if((rowL-10)/2 == (rowS-1)/3){
	     if     ((rowL-10)%2==0 && (rowS-1)%3==2) {ret=false;}
	     else if((rowL-10)%2==1 && (rowS-1)%3==0) {ret=false;}
	     else                                     {ret=true;}
	 }else{
	     ret=true;
	 }
    }else{
	 ret=true;
    }
    //printf("det0=%2d det1=%2d rowL=%2d colL=%2d rowS=%2d colS=%2d ret=%d\n",det0,det1,rowL,colL,rowS,colS,int(ret));
    return ret;
}

}  // namespace FMSCluster
