/***************************************************************************
 *
 * StMcVertex.cc
 *
 **************************************************************************/
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"
static const char rcsid[] = "$Id: StMcVertex.cc,v 1.1.1.1 1999/07/13 18:08:06 uid2620 Exp $";

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
  for (int i = 0; i<4; ++i) mGeantVolume[i] = vtx->ge_volume[i];
  mTof = vtx->ge_tof;
  mGeantProcess = vtx->eg_proc;
  
  mParent = 0;
  mDaughters = 0; 
  
  mGeantVolume = "aaaa";
  mTof = 0;
  mGeantProcess = 0;
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

void StMcVertex::setPosition(const StThreeVector<float>& val) { mPosition = val; }

void StMcVertex::addDaughter(StMcTrack* val) { mDaughters->push_back(val); }  

void StMcVertex::setGeantVolume(string val) { mGeantVolume = val; } 

void StMcVertex::setTof(float val) { mTof = val; }

void StMcVertex::setGeantProcess(int val) { mGeantProcess = val; }     
