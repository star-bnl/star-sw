/***************************************************************************
 *
 * $Id: StMcVertex.cc,v 1.3 1999/07/29 00:13:14 calderon Exp $
 * $Log: StMcVertex.cc,v $
 * Revision 1.3  1999/07/29 00:13:14  calderon
 * Read ge_volume correctly
 *
 *
 **************************************************************************/
#include "StThreeVectorF.hh"

#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"
static const char rcsid[] = "$Id: StMcVertex.cc,v 1.3 1999/07/29 00:13:14 calderon Exp $";

StMcVertex::StMcVertex()
{
 
    mParent = 0;                
    mDaughters = 0; 
   
    mGeantVolume = "aaaa";
    mTof = 0;
    mGeantProcess = 0;
    
    mDaughters = new StMcTrackCollection();
}
StMcVertex::StMcVertex(float x, float y, float z)
{
 
    mParent = 0;                
    mDaughters = 0; 
   
    mGeantVolume = "none";
    mTof = 0;
    mGeantProcess = 0;
    
    mDaughters = new StMcTrackCollection();
    setPosition((x,y,z));
}

StMcVertex::StMcVertex(g2t_vertex_st* vtx)
{

    
  // Here we use the table based constructor.  
  mPosition.setX(vtx->ge_x[0]);
  mPosition.setY(vtx->ge_x[1]);
  mPosition.setZ(vtx->ge_x[2]);
  mGeantVolume=vtx->ge_volume;
  mTof = vtx->ge_tof;
  mGeantProcess = vtx->eg_proc;
  
  mParent = 0;
  mDaughters = 0; 
    
  mDaughters = new StMcTrackCollection();
  
}

StMcVertex::~StMcVertex()
{
    
    delete mDaughters;
}


int StMcVertex::operator==(const StMcVertex& v) const
{
    return mGeantProcess == v.mGeantProcess &&mPosition == v.mPosition;
}

int StMcVertex::operator!=(const StMcVertex& v) const
{
    return !(v == *this);
}



void StMcVertex::setParent(StMcTrack* val) {  mParent = val; }         

void StMcVertex::setPosition(const StThreeVectorF& val) { mPosition = val; }

void StMcVertex::addDaughter(StMcTrack* val) { mDaughters->push_back(val); }  

void StMcVertex::setGeantVolume(string val) { mGeantVolume = val; } 

void StMcVertex::setTof(float val) { mTof = val; }

void StMcVertex::setGeantProcess(int val) { mGeantProcess = val; }     
