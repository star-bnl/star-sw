/***************************************************************************
 *
 * $Id: StMcTrack.cc,v 2.2 1999/12/03 00:51:53 calderon Exp $
 * $Log: StMcTrack.cc,v $
 * Revision 2.2  1999/12/03 00:51:53  calderon
 * Tested with new StMcEventMaker.  Added messages for
 * diagnostics.
 *
 * Revision 2.1  1999/11/19 19:06:34  calderon
 * Recommit after redoing the files.
 *
 * Revision 2.0  1999/11/17 02:12:16  calderon
 * Completely revised for new StEvent
 *
 * Revision 1.3  1999/09/23 21:25:54  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/

#include "StMcTrack.hh"

#include "StMcVertex.hh"
#include "StMcTpcHit.hh"
#include "StMcSvtHit.hh"
#include "StMcFtpcHit.hh"

#include "StMcContainers.hh"
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

#include "tables/St_g2t_track_Table.h"

static const char rcsid[] = "$Id: StMcTrack.cc,v 2.2 1999/12/03 00:51:53 calderon Exp $";

StMcTrack::StMcTrack() 
{
    initToZero();
}



StMcTrack::StMcTrack(g2t_track_st* trk) {
    initToZero();
    mFourMomentum.setPx(trk->p[0]);
    mFourMomentum.setPy(trk->p[1]);
    mFourMomentum.setPz(trk->p[2]);
    mFourMomentum.setE(trk->e);
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
    // Not owner, so we don't have to delete.
    mIntermediateVertices.clear();
    mTpcHits.clear();
    mSvtHits.clear();
    mFtpcHits.clear();
}

void StMcTrack::initToZero()
{
    
    mStartVertex  = 0;
    mStopVertex   = 0;
    mParticleDefinition = 0;
    mIsShower = 0;
    mGeantId = 0;
}

int StMcTrack::operator==(const StMcTrack& t) const
{
    return (t.mFourMomentum == mFourMomentum &&
	    t.mStartVertex  == mStartVertex   &&
	    t.mStopVertex   == mStopVertex );
}

int StMcTrack::operator!=(const StMcTrack& t) const
{
    return !(t == *this);
}

ostream&  operator<<(ostream& os, const StMcTrack& t)
{
    os << "Particle      : " << t.particleDefinition()->name().c_str() << endl;
    os << "Four Momentum : " << t.fourMomentum() << endl; 
    os << "Pt            : " << t.pt() << endl;
    os << "Rapidity      : " << t.rapidity() << endl;
    os << "PseudoRapidity: " << t.pseudoRapidity() << endl;
    os << "No. Tpc  Hits : " << t.tpcHits().size() << endl;
    os << "No. Svt  Hits : " << t.svtHits().size() << endl;
    os << "No. Ftpc Hits : " << t.ftpcHits().size() << endl;
    os << "Is Shower     : " << t.isShower() << endl;
    os << "Geant Id      : " << t.geantId()  << endl;
    return os;
}

void StMcTrack::setFourMomentum(const StLorentzVectorF& val) { mFourMomentum = val; }

void StMcTrack::setStartVertex(StMcVertex* val) { mStartVertex = val; }

void StMcTrack::setStopVertex(StMcVertex* val) { mStopVertex = val; }

void StMcTrack::setIntermediateVertices(StPtrVecMcVertex& val) { mIntermediateVertices = val; }

void StMcTrack::setTpcHits(StPtrVecMcTpcHit& val) { mTpcHits = val; }

void StMcTrack::setSvtHits(StPtrVecMcSvtHit& val) { mSvtHits = val; }

void StMcTrack::setFtpcHits(StPtrVecMcFtpcHit& val) { mFtpcHits = val; }

void StMcTrack::setShower(char val) { mIsShower = val; }

void StMcTrack::setGeantId(long val) { mGeantId = val; }

void StMcTrack::addTpcHit(StMcTpcHit* hit)
{
  mTpcHits.push_back(hit);
}

void StMcTrack::addFtpcHit(StMcFtpcHit* hit)
{
  mFtpcHits.push_back(hit);
}

void StMcTrack::addSvtHit(StMcSvtHit* hit)
{
  mSvtHits.push_back(hit);
}

void StMcTrack::removeTpcHit(StMcTpcHit* hit)
{
  StPtrVecMcTpcHitIterator iter = find(mTpcHits.begin(), mTpcHits.end(), hit);
  if (iter != mTpcHits.end()) mTpcHits.erase(iter);
}

void StMcTrack::removeFtpcHit(StMcFtpcHit* hit)
{
  StPtrVecMcFtpcHitIterator iter = find(mFtpcHits.begin(), mFtpcHits.end(), hit);
  if (iter != mFtpcHits.end()) mFtpcHits.erase(iter);
}

void StMcTrack::removeSvtHit(StMcSvtHit* hit)
{
  StPtrVecMcSvtHitIterator iter = find(mSvtHits.begin(), mSvtHits.end(), hit);
  if (iter != mSvtHits.end()) mSvtHits.erase(iter);
}

//void StMcTrack::setTopologyMap(StTrackTopologyMap& val) { mTopologyMap = val; }
