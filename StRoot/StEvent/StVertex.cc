/***************************************************************************
 *
 * $Id: StVertex.cc,v 1.1 1999/01/15 20:40:26 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.cc,v $
 * Revision 1.1  1999/01/15 20:40:26  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#include "StVertex.hh"

StVertex::StVertex()
{
    mType = undefined;                           
    mParent = 0;                
    mQualityBitmask = 0;        
    mChiSquared = 0;            
}

StVertex::~StVertex() { /* noop */}


int StVertex::operator==(const StVertex& v) const
{
    return mType == v.mType &&mPosition == v.mPosition;
}

int StVertex::operator!=(const StVertex& v) const
{
    return !(v == *this);
}

void StVertex::setType(StVertexType val) { mType = val; }           

void StVertex::setParent(StGlobalTrack*  val) { mParent = val; }         

void StVertex::setPosition(const StThreeVector<float>& val) { mPosition = val; }       

void StVertex::setPositionError(const StThreeVector<float>& val) { mPositionError = val; }  

void StVertex::setQualityBitmask(unsigned long val) { mQualityBitmask = val; } 

void StVertex::setChiSquared(float val) { mChiSquared = val; }     
