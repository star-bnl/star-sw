/*****************************************
 *
 * $Id: StMcParameterDB.cxx,v 1.5 1999/12/14 07:07:41 calderon Exp $
 *
 * Changed extension to .cxx so that it
 * would be accessible from Root macro
 *
 * $Log: StMcParameterDB.cxx,v $
 * Revision 1.5  1999/12/14 07:07:41  calderon
 * Added Ratio Number of Common Hits / Number of Reconstructed Hits for
 * each detector.
 * Numbering scheme from StEvent & StMcEvent as per SVT request
 * Added Kink, V0 and Xi vertex associations.
 *
 * Revision 1.4  1999/12/08 00:00:25  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
 * Revision 1.3  1999/10/01 14:08:58  calderon
 * Added Local Hit resolution Histogram. It is made by default
 * without any requirement of association, to serve
 * as a diagnostic.
 * Before building track multimap, check the size of the
 * tpc hit map.  If it is too small, print out a warning
 * and exit.
 *
 * Revision 1.2  1999/09/23 21:25:21  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *****************************************/
#include <iostream.h>
#include "StMcParameterDB.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

ClassImp(StMcParameterDB)
    
StMcParameterDB* StMcParameterDB::mParamDB = 0;

StMcParameterDB* StMcParameterDB::instance()
{
    if (!mParamDB) mParamDB = new StMcParameterDB;
    return mParamDB;
}

StMcParameterDB::StMcParameterDB()
    :mXCutTpc(5.*millimeter),mYCutTpc(5.*millimeter),mZCutTpc(3.*millimeter),mReqCommonHitsTpc(3),
     mXCutSvt(1.*millimeter),mYCutSvt(1.*millimeter),mZCutSvt(1.*millimeter),mReqCommonHitsSvt(1),
     mRCutFtpc(3.*millimeter),mPhiCutFtpc(5.*degree),mReqCommonHitsFtpc(2)
{ /*noop*/ }

void StMcParameterDB::setXCutTpc(float val) { mXCutTpc = val ;}
void StMcParameterDB::setYCutTpc(float val) { mYCutTpc = val ;}
void StMcParameterDB::setZCutTpc(float val) { mZCutTpc = val ;}
void StMcParameterDB::setReqCommonHitsTpc(unsigned int val) { mReqCommonHitsTpc = val;}

void StMcParameterDB::setXCutSvt(float val) { mXCutSvt = val ;}
void StMcParameterDB::setYCutSvt(float val) { mYCutSvt = val ;}
void StMcParameterDB::setZCutSvt(float val) { mZCutSvt = val ;}
void StMcParameterDB::setReqCommonHitsSvt(unsigned int val) { mReqCommonHitsSvt = val;}

void StMcParameterDB::setRCutFtpc(float val)   { mRCutFtpc   = val ;}
void StMcParameterDB::setPhiCutFtpc(float val) { mPhiCutFtpc = val ;}
void StMcParameterDB::setReqCommonHitsFtpc(unsigned int val) { mReqCommonHitsFtpc = val;}

ostream& operator<<(ostream &os, const StMcParameterDB& mcDb)
{
    os << " TPC Cuts " << endl;
    os << " X Cut   : " << mcDb.xCutTpc()/millimeter << " mm" << endl; 
    os << " Y Cut   : " << mcDb.yCutTpc()/millimeter << " mm" << endl; 
    os << " Z Cut   : " << mcDb.zCutTpc()/millimeter << " mm" << endl; 
    os << " Required Hits for Associating Tracks: " << mcDb.reqCommonHitsTpc() << endl;
    os << " SVT Cuts " << endl;
    os << " X Cut   : " << mcDb.xCutSvt()/millimeter << " mm" << endl; 
    os << " Y Cut   : " << mcDb.yCutSvt()/millimeter << " mm" << endl; 
    os << " Z Cut   : " << mcDb.zCutSvt()/millimeter << " mm" << endl; 
    os << " Required Hits for Associating Tracks: " << mcDb.reqCommonHitsSvt() << endl;
    os << " FTPC Cuts " << endl;
    os << " R Cut   : " << mcDb.rCutFtpc()/millimeter << " mm" << endl; 
    os << " Phi Cut : " << mcDb.phiCutFtpc()/degree << " degrees" << endl; 
    os << " Required Hits for Associating Tracks: " << mcDb.reqCommonHitsFtpc() << endl;
    
    return os;
}
