/***************************************************************************
 *
 * $Id: StMcVertex.cc,v 2.1 1999/11/19 19:06:34 calderon Exp $
 * $Log: StMcVertex.cc,v $
 * Revision 2.1  1999/11/19 19:06:34  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:17  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.4  1999/09/23 21:25:55  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.3  1999/07/29 00:13:14  calderon
 * Read ge_volume correctly
 *
 *
 **************************************************************************/
#include <algorithm>

#include "StMcVertex.hh"
#include "StMcTrack.hh"
#include "tables/St_g2t_vertex_Table.h"

static const char rcsid[] = "$Id: StMcVertex.cc,v 2.1 1999/11/19 19:06:34 calderon Exp $";

StMcVertex::StMcVertex()
{
 
    mParent = 0;
    mGeantVolume = "aaaa";
    mTof = 0;
    mGeantProcess = 0;
}
StMcVertex::StMcVertex(float x, float y, float z)
{
 
    mParent = 0;                
    mGeantVolume = "none";
    mTof = 0;
    mGeantProcess = 0;
    
    setPosition((x,y,z));
}

StMcVertex::StMcVertex(g2t_vertex_st* vtx)
{

    
  mPosition.setX(vtx->ge_x[0]);
  mPosition.setY(vtx->ge_x[1]);
  mPosition.setZ(vtx->ge_x[2]);
  mGeantVolume=vtx->ge_volume;
  mTof = vtx->ge_tof;
  mGeantProcess = vtx->eg_proc;
  
  mParent = 0;
    
}

StMcVertex::~StMcVertex()
{
    mDaughters.clear();  //Not owner, so we don't have to delete.
}


int StMcVertex::operator==(const StMcVertex& v) const
{
    return (mGeantProcess == v.mGeantProcess &&
	    mPosition     == v.mPosition     &&
	    mTof          == v.mTof);
}

int StMcVertex::operator!=(const StMcVertex& v) const
{
    return !(v == *this);
}



void StMcVertex::setParent(StMcTrack* val) {  mParent = val; }         

void StMcVertex::addDaughter(StMcTrack* val) { mDaughters.push_back(val); }  

void StMcVertex::setGeantVolume(string val) { mGeantVolume = val; } 

void StMcVertex::setTof(float val) { mTof = val; }

void StMcVertex::setGeantProcess(int val) { mGeantProcess = val; }     

void StMcVertex::removeDaughter(StMcTrack* trk) {
    StPtrVecMcTrackIterator iter = find(mDaughters.begin(), mDaughters.end(), trk);
  if (iter != mDaughters.end()) mDaughters.erase(iter);
}
