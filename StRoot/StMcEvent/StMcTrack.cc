/***************************************************************************
 *
 * $Id: StMcTrack.cc,v 1.3 1999/09/23 21:25:54 calderon Exp $
 * $Log: StMcTrack.cc,v $
 * Revision 1.3  1999/09/23 21:25:54  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/

#include "StMcTrack.hh"
#include "StMcVertex.hh"
#include "StMcVertex.hh"
#include "StMcTpcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcFtpcHit.hh"
#include "StMcFtpcHitCollection.hh"
#include "StMcTpcHitCollection.hh"
#include "StMcSvtHitCollection.hh"
#include "StMcVertexCollection.hh"

#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

static const char rcsid[] = "$Id: StMcTrack.cc,v 1.3 1999/09/23 21:25:54 calderon Exp $";

StMcTrack::StMcTrack() 
{
    init();
}



StMcTrack::StMcTrack(g2t_track_st* trk) {
    init();
    mMomentum.setX(trk->p[0]);
    mMomentum.setY(trk->p[1]);
    mMomentum.setZ(trk->p[2]);
    mIsShower = trk->is_shower;
    mGeantId = trk->ge_pid;
    mParticleDefinition = StParticleTable::instance()->findParticleByGeantId(trk->ge_pid);
    
    // The information to fill the collections 
    // is not available directly from the tables.  
    // We need to decode from trk->hit_tpc_p, 
    // trk->hit_svt_p, trk->hit_ftp_p, and trk-next_vtx_trk_p 
    // the actual Collection objects, not just the id's stored in the table.
}



StMcTrack::~StMcTrack() { 
  
        
    
    
    delete mIntermediateVertices;
    delete mTpcHits;
    delete mSvtHits;
    delete mFtpcHits;

    //delete mPhysicalParticle;
}

void StMcTrack::init()
{
    
    mStartVertex = 0;
    mStopVertex   = 0;
    mStartVertex = 0;
    mStopVertex = 0;
    //mPhysicalParticle = 0;

    // Create the collections

    mIntermediateVertices = new StMcVertexCollection();
    mTpcHits = new StMcTpcHitCollection();
    mSvtHits = new StMcSvtHitCollection();
    mFtpcHits = new StMcFtpcHitCollection();

    mIsShower = 0;
}

int StMcTrack::operator==(const StMcTrack& t) const
{
    return t.mMomentum == mMomentum && t.mStartVertex == mStartVertex &&
           t.mStopVertex == mStopVertex ;
}

int StMcTrack::operator!=(const StMcTrack& t) const
{
    return !(t == *this);
}

void StMcTrack::setMomentum(const StThreeVectorF& val) { mMomentum = val; }

void StMcTrack::setStartVertex(StMcVertex* val) { mStartVertex = val; }

void StMcTrack::setStopVertex(StMcVertex* val) { mStopVertex = val; }

void StMcTrack::setIntermediateVertices(StMcVertexCollection* val) { mIntermediateVertices = val; }

void StMcTrack::setTpcHits(StMcTpcHitCollection* val) { mTpcHits = val; }

void StMcTrack::setSvtHits(StMcSvtHitCollection* val) { mSvtHits = val; }

void StMcTrack::setFtpcHits(StMcFtpcHitCollection* val) { mFtpcHits = val; }

void StMcTrack::setShower(char val) { mIsShower = val; }

void StMcTrack::setGeantId(long val) { mGeantId = val; }

void StMcTrack::addTpcHit(StMcTpcHit* hit)
{
  mTpcHits->push_back(hit);
}

void StMcTrack::addFtpcHit(StMcFtpcHit* hit)
{
  mFtpcHits->push_back(hit);
}

void StMcTrack::addSvtHit(StMcSvtHit* hit)
{
  mSvtHits->push_back(hit);
}

void StMcTrack::removeTpcHit(StMcTpcHit* hit)
{
  StMcTpcHitIterator iter = find(mTpcHits->begin(), mTpcHits->end(), hit);
  if (iter != mTpcHits->end()) mTpcHits->erase(iter);
}

void StMcTrack::removeFtpcHit(StMcFtpcHit* hit)
{
  StMcFtpcHitIterator iter = find(mFtpcHits->begin(), mFtpcHits->end(), hit);
  if (iter != mFtpcHits->end()) mFtpcHits->erase(iter);
}

void StMcTrack::removeSvtHit(StMcSvtHit* hit)
{
  StMcSvtHitIterator iter = find(mSvtHits->begin(), mSvtHits->end(), hit);
  if (iter != mSvtHits->end()) mSvtHits->erase(iter);
}
