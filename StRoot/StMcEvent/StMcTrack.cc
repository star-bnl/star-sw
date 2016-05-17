/*!
 * \class StMcTrack
 * \brief Monte Carlo Track class
 * All information on a simulated track is stored in this class:
 * kinematics, particle identificiation, hits left in the detector
 * and start/stop vertices.
 * \author Manuel Calderon de la Barca Sanchez
 * \date   July 1999
 *
 ***************************************************************************
 *
 * $Id: StMcTrack.cc,v 2.38 2016/05/16 23:47:09 perev Exp $
 *
 ***************************************************************************
 *
 * $Log: StMcTrack.cc,v $
 * Revision 2.38  2016/05/16 23:47:09  perev
 * Coverity fix
 *
 * Revision 2.37  2015/07/22 19:30:02  jwebb
 * Fix minor compiler warnings.
 *
 * Revision 2.36  2014/08/06 19:08:49  perev
 * Warnoff
 *
 * Revision 2.35  2013/03/25 23:33:04  perev
 * Mustafa.Pxl corrs
 *
 * Revision 2.34  2012/03/22 01:03:26  perev
 * Etr add
 *
 * Revision 2.31  2011/10/11 01:22:24  perev
 * Not used anymore or ever
 *
 * Revision 2.30  2011/07/20 17:35:53  perev
 * Fsc added
 *
 * Revision 2.29  2011/03/22 22:30:33  perev
 * Bug#2111 Remove redundant zeroing
 *
 * Revision 2.28  2011/02/16 00:47:59  perev
 * mGeantId>=0 && mGeantId<=0
 *
 * Revision 2.27  2011/01/26 19:46:24  perev
 * FPD ==> STAR Soft
 *
 * Revision 2.26  2007/10/16 19:49:20  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 2.25  2006/09/25 14:20:43  fisyak
 * Add Hpd Hits
 *
 * Revision 2.24  2006/09/22 19:19:34  fisyak
 * Add generic access functions for tracking and calorimeter hits
 *
 * Revision 2.23  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.22  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.21  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 * Revision 2.20  2005/05/27 23:37:25  calderon
 * Update for EEMC, add eprs, esmdu esdmv hits to StMcEvent.
 *
 * Revision 2.19  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 * Revision 2.18  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.17  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.16  2003/12/04 05:56:47  calderon
 * Inclusion of Endcap EMC hit collection in StMcEvent and
 * of the Endcap hit vector in StMcTrack.
 * fix const of StMcVertex::parent() to avoid warnings in user code
 *
 * Revision 2.15  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.14  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
 * Revision 2.2  2003/02/18 00:00:00  gans
 * Introduction of Ctb classes.  Modified several classes
 * accordingly.

 * $Id: StMcTrack.cc,v 2.38 2016/05/16 23:47:09 perev Exp $
 * $Log: StMcTrack.cc,v $
 * Revision 2.38  2016/05/16 23:47:09  perev
 * Coverity fix
 *
 * Revision 2.37  2015/07/22 19:30:02  jwebb
 * Fix minor compiler warnings.
 *
 * Revision 2.36  2014/08/06 19:08:49  perev
 * Warnoff
 *
 * Revision 2.35  2013/03/25 23:33:04  perev
 * Mustafa.Pxl corrs
 *
 * Revision 2.34  2012/03/22 01:03:26  perev
 * Etr add
 *
 * Revision 2.31  2011/10/11 01:22:24  perev
 * Not used anymore or ever
 *
 * Revision 2.30  2011/07/20 17:35:53  perev
 * Fsc added
 *
 * Revision 2.29  2011/03/22 22:30:33  perev
 * Bug#2111 Remove redundant zeroing
 *
 * Revision 2.28  2011/02/16 00:47:59  perev
 * mGeantId>=0 && mGeantId<=0
 *
 * Revision 2.27  2011/01/26 19:46:24  perev
 * FPD ==> STAR Soft
 *
 * Revision 2.26  2007/10/16 19:49:20  fisyak
 * rename Hft => Pxl, remove Hpd, Igt and Fst
 *
 * Revision 2.25  2006/09/25 14:20:43  fisyak
 * Add Hpd Hits
 *
 * Revision 2.24  2006/09/22 19:19:34  fisyak
 * Add generic access functions for tracking and calorimeter hits
 *
 * Revision 2.23  2005/11/22 21:44:52  fisyak
 * Add compress Print for McEvent, add Ssd collections
 *
 * Revision 2.22  2005/09/28 21:30:15  fisyak
 * Persistent StMcEvent
 *
 * Revision 2.21  2005/07/07 18:20:49  calderon
 * Added support for IGT detector.
 *
 * Revision 2.20  2005/05/27 23:37:25  calderon
 * Update for EEMC, add eprs, esmdu esdmv hits to StMcEvent.
 *
 * Revision 2.19  2005/04/18 20:11:33  calderon
 * Addition of Fgt and Fst files.  Modified other files to accomodate changes.
 *
 * Revision 2.18  2005/01/27 23:40:49  calderon
 * Adding persistency to StMcEvent as a step for Virtual MonteCarlo.
 *
 * Revision 2.17  2004/09/14 05:00:30  calderon
 * Added support for Ist, Ssd and changes to Pixel, from "El Kai".
 *
 * Revision 2.16  2003/12/04 05:56:47  calderon
 * Inclusion of Endcap EMC hit collection in StMcEvent and
 * of the Endcap hit vector in StMcTrack.
 * fix const of StMcVertex::parent() to avoid warnings in user code
 *
 * Revision 2.15  2003/08/20 18:50:21  calderon
 * Addition of Tof classes and Pixel classes.  Modified track, event, and
 * container code to reflect this.
 * Fix bug in StMcVertex and in clearing of some hit collections.
 *
 * Revision 2.14  2003/02/19 03:16:05  calderon
 * Introduction of Ctb Hit Class and Ctb Hit Collection class, modified
 * StMcTrack, and StMcEvent accordingly.  Clearing of hits in StMcSvtWaferHitCollection.
 *
 * Revision 2.13  2000/06/22 23:52:27  calderon
 * Alexei fixed typo in ostream<< for Bsdme
 *
 * Revision 2.12  2000/06/06 02:58:42  calderon
 * Introduction of Calorimeter classes.  Modified several classes
 * accordingly.
 *
 * Revision 2.11  2000/05/05 15:25:44  calderon
 * Reduced dependencies and made constructors more efficient
 *
 * Revision 2.10  2000/05/04 22:46:01  calderon
 * read the pdg Id ALSO from the g2t_track table.  This caused problems
 * for embedded tracks.
 *
 * Revision 2.9  2000/04/18 00:55:14  calderon
 * added printout of local momentum to operator<<
 *
 * Revision 2.8  2000/04/12 21:32:05  calderon
 * Chech particle definition pointer before writing name
 *
 * Revision 2.7  2000/04/06 23:29:10  calderon
 * Store the parent track for all tracks.
 *
 * Revision 2.6  2000/04/06 08:34:56  calderon
 * Version using the particle table:
 * 1) Constructor for particle_st*
 * 2) Pointer to parent track from particle table
 * 3) PDG encoding when track is from particle table
 * 4) Generator label, used to index entries in the table for debugging
 *
 * Revision 2.5  2000/03/06 18:05:23  calderon
 * 1) Modified SVT Hits storage scheme from layer-ladder-wafer to
 * barrel-ladder-wafer.
 * 2) Added Rich Hit class and collection, and links to them in other
 * classes.
 *
 * Revision 2.4  2000/01/18 20:52:31  calderon
 * Works with CC5
 *
 * Revision 2.3  1999/12/14 07:04:50  calderon
 * Numbering scheme as per SVT request.
 *
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
#include <algorithm>
#include <assert.h>
#ifndef ST_NO_NAMESPACES
using std::find;
#endif

#include "StMcTrack.hh"

#include "StMcContainers.hh"
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

#include "tables/St_g2t_track_Table.h"
#include "tables/St_particle_Table.h"

static const char rcsid[] = "$Id: StMcTrack.cc,v 2.38 2016/05/16 23:47:09 perev Exp $";

ClassImp(StMcTrack);

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
    assert(mGeantId>=0 && mGeantId<=0xFFFF);
    mPdgId   = trk->eg_pid;
    mKey     = trk->id;
    mParticleDefinition = StParticleTable::instance()->findParticleByGeantId(trk->ge_pid);
    mEventGenLabel = trk->eg_label;
    mIsPrimary = kFALSE;
    // The information to fill the collections 
    // is not available directly from the tables.  
    // We need to decode from trk->hit_tpc_p, 
    // trk->hit_svt_p, trk->hit_ftp_p, and trk-next_vtx_trk_p 
    // the actual Collection objects, not just the id's stored in the table.
}

StMcTrack::StMcTrack(particle_st* trk) {
    initToZero();
    mFourMomentum.setPx(trk->phep[0]);
    mFourMomentum.setPy(trk->phep[1]);
    mFourMomentum.setPz(trk->phep[2]);
    mFourMomentum.setE(trk->phep[3]);
    mParticleDefinition = StParticleTable::instance()->findParticle(trk->idhep);
    mPdgId = trk->idhep;
    mIsPrimary = kFALSE;
    // This constructor is used for particles coming from the
    // particle table.
}



StMcTrack::~StMcTrack() {
    // Not owner, so we don't have to delete.
    mIntermediateVertices.clear();
    mTpcHits.clear();
    mSvtHits.clear();
    mSsdHits.clear();
    mFtpcHits.clear();
    mRichHits.clear();
    mCtbHits.clear();
    mBemcHits.clear();
    mBprsHits.clear();
    mBsmdeHits.clear();
    mBsmdpHits.clear();
    mTofHits.clear();
    mMtdHits.clear();
    mEemcHits.clear();
    mEprsHits.clear();
    mEsmduHits.clear();
    mEsmdvHits.clear();
    mPxlHits.clear();
    mIstHits.clear();
    mFgtHits.clear();
    mEtrHits.clear();
}

void StMcTrack::initToZero()
{
    
    mStartVertex  = 0;
    mStopVertex   = 0;
    mParticleDefinition = 0;
    mParent = 0;
    mIsShower        = 0;
    mGeantId         = 0;
    mPdgId           = 0;
    mKey             = 0;
    mEventGenLabel   = 0;
    mIsPrimary	     = 0;
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
    if (t.particleDefinition())
	os << "Particle      : " << t.particleDefinition()->name().c_str() << endl;
    os << "Four Momentum : " << t.fourMomentum() << endl; 
    os << "Pt            : " << t.pt() << endl;
    os << "Rapidity      : " << t.rapidity() << endl;
    os << "PseudoRapidity: " << t.pseudoRapidity()   << endl;
    os << "No. Tpc   Hits: " << t.tpcHits().size()   << endl;
    os << "No. Svt   Hits: " << t.svtHits().size()   << endl;
    os << "No. Ssd   Hits: " << t.ssdHits().size()   << endl;
    os << "No. Ftpc  Hits: " << t.ftpcHits().size()  << endl;
    os << "No. Rich  Hits: " << t.richHits().size()  << endl;
    os << "No. Ctb   Hits: " << t.ctbHits().size()   << endl;
    os << "No. Bemc  Hits: " << t.bemcHits().size()  << endl;
    os << "No. Bprs  Hits: " << t.bprsHits().size()  << endl;
    os << "No. Bsmde Hits: " << t.bsmdeHits().size() << endl;
    os << "No. Bsmdp Hits: " << t.bsmdpHits().size() << endl;
    os << "No. Tof   Hits: " << t.tofHits().size()   << endl;
    os << "No. Mtd   Hits: " << t.mtdHits().size()   << endl;
    os << "No. Eemc  Hits: " << t.eemcHits().size()  << endl;
    os << "No. Eprs  Hits: " << t.eprsHits().size()  << endl;
    os << "No. Esmdu Hits: " << t.esmduHits().size() << endl;
    os << "No. Esmdv Hits: " << t.esmdvHits().size() << endl;
    os << "No. Pxl Hits  : " << t.pxlHits().size()   << endl;
    os << "No. Ist Hits  : " << t.istHits().size()   << endl;
    os << "No. Fgt Hits  : " << t.fgtHits().size()   << endl;
    os << "No. Fsc Hits  : " << t.fscHits().size()   << endl;
    os << "No. Etr Hits  : " << t.etrHits().size()   << endl;
    os << "Is Shower     : " << t.isShower() << endl;
    os << "Geant Id      : " << t.geantId()  << endl;
    os << "Pdg Code      : " << t.pdgId()  << endl;
    os << "Event Gen. Lab: " << t.eventGenLabel()  << endl;
    os << "Key from g2t  : " << t.key()  << endl;
    
    return os;
}
void StMcTrack::Print(Option_t *option) const {
  TString opt(option);
  if (opt == "") {cout << *this << endl; return;}
  if (opt.Contains("desc",TString::kIgnoreCase)) {
    //       0        1         2         3         4
    //       1234567890123456789012345678901234567890
    cout << "Particle"
	 << " Four Momentum                  " 
	 << "Pt      " 
	 << "Rapidity" 
	 << "  Pseudo" 
	 << " Id"  
	 << "   Pdg"  
	 << "Egl"  
	 << "Key"
	 << "Hit"
	 << "Tpc" 
	 << "Svt" 
	 << "Ssd" 
	 << "Fpc" 
	 << "Rch" 
	 << "Ctb" 
	 << "Bmc" 
	 << "Bpr" 
	 << "Bse" 
	 << "Bsp" 
	 << "Tof" 
	 << "Mtd" 
	 << "Emc" 
	 << "Eps" 
	 << "Esu" 
	 << "Esv" 
	 << "Pxl" 
	 << "Ist"  
	 << "Fgt"  
	 << "Etr"  
	 << "ISh" << endl;
    return;
  }
  TString Name("        ");
  if (particleDefinition()) Name = particleDefinition()->name().c_str();
  Double_t eta = pseudoRapidity();
  if (TMath::Abs(eta) > 999.999) eta = TMath::Sign(999.999, eta);
  Double_t y = rapidity();
  if (TMath::Abs(y) > 999.999) y = TMath::Sign(999.999, y);
    cout << 
      Form("%8s%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%8.3f%3li%6li%3li%3li%6li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li%3li",
	   Name.Data(),
	   fourMomentum().x(), fourMomentum().y(), fourMomentum().z(), fourMomentum().t(), 
	   pt(),
	   y,
	   eta,
	   (long int)(geantId()),
	   (long int)(pdgId()),
	   (long int)(eventGenLabel()),
	   (long int)(key()),
	   (long int)(tpcHits().size()),
	   (long int)(svtHits().size()),
	   (long int)(ssdHits().size()),
	   (long int)(ftpcHits().size()),
	   (long int)(richHits().size()),
	   (long int)(ctbHits().size()),
	   (long int)(bemcHits().size()),
	   (long int)(bprsHits().size()),
	   (long int)(bsmdeHits().size()),
	   (long int)(bsmdpHits().size()),
	   (long int)(tofHits().size()),
	   (long int)(mtdHits().size()),
	   (long int)(eemcHits().size()),
	   (long int)(eprsHits().size()),
	   (long int)(esmduHits().size()),
	   (long int)(esmdvHits().size()),
	   (long int)(pxlHits().size()),
	   (long int)(istHits().size()),
	   (long int)(fgtHits().size()),
	   (long int)(etrHits().size()),
	   (long int)(isShower()))
	 << endl;
}
void StMcTrack::setFourMomentum(const StLorentzVectorF& val) { mFourMomentum = val; }

void StMcTrack::setStartVertex(StMcVertex* val) { mStartVertex = val; }

void StMcTrack::setStopVertex(StMcVertex* val) { mStopVertex = val; }

void StMcTrack::setIntermediateVertices(StPtrVecMcVertex& val) { mIntermediateVertices = val; }

void StMcTrack::setTpcHits(StPtrVecMcTpcHit& val) { mTpcHits = val; }

void StMcTrack::setSvtHits(StPtrVecMcSvtHit& val) { mSvtHits = val; }

void StMcTrack::setSsdHits(StPtrVecMcSsdHit& val) { mSsdHits = val; }

void StMcTrack::setFtpcHits(StPtrVecMcFtpcHit& val) { mFtpcHits = val; }

void StMcTrack::setRichHits(StPtrVecMcRichHit& val) { mRichHits = val; }

void StMcTrack::setCtbHits(StPtrVecMcCtbHit& val) { mCtbHits = val; }

void StMcTrack::setBemcHits(StPtrVecMcCalorimeterHit& val) { mBemcHits = val; }

void StMcTrack::setBprsHits(StPtrVecMcCalorimeterHit& val) { mBprsHits = val; }

void StMcTrack::setBsmdeHits(StPtrVecMcCalorimeterHit& val) { mBsmdeHits = val; }

void StMcTrack::setBsmdpHits(StPtrVecMcCalorimeterHit& val) { mBsmdpHits = val; }

void StMcTrack::setTofHits(StPtrVecMcTofHit& val) { mTofHits = val; }

void StMcTrack::setMtdHits(StPtrVecMcMtdHit& val) { mMtdHits = val; }

void StMcTrack::setEemcHits(StPtrVecMcCalorimeterHit& val) { mEemcHits = val; }

void StMcTrack::setEprsHits(StPtrVecMcCalorimeterHit& val) { mEprsHits = val; }

void StMcTrack::setEsmduHits(StPtrVecMcCalorimeterHit& val) { mEsmduHits = val; }

void StMcTrack::setEsmdvHits(StPtrVecMcCalorimeterHit& val) { mEsmdvHits = val; }

void StMcTrack::setFscHits(StPtrVecMcCalorimeterHit& val) { mFscHits = val; }

void StMcTrack::setPxlHits(StPtrVecMcPxlHit& val) { mPxlHits = val; }

void StMcTrack::setIstHits(StPtrVecMcIstHit& val) { mIstHits = val; }

void StMcTrack::setFgtHits(StPtrVecMcFgtHit& val) { mFgtHits = val; }

void StMcTrack::setEtrHits(StPtrVecMcEtrHit& val) { mEtrHits = val; }

void StMcTrack::setShower(char val) { mIsShower = val; }

void StMcTrack::setGeantId(long val) { mGeantId = val; assert(mGeantId>=0 && mGeantId<=0xFFFF); }

void StMcTrack::setPdgId(long val) { mPdgId = val; }

void StMcTrack::setKey(long val) { mKey = val; }

void StMcTrack::setEventGenLabel(long val) { mEventGenLabel = val; }

void StMcTrack::setParent(StMcTrack* val) { mParent = val; }

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

void StMcTrack::addSsdHit(StMcSsdHit* hit)
{
  mSsdHits.push_back(hit);
}

void StMcTrack::addRichHit(StMcRichHit* hit)
{
  mRichHits.push_back(hit);
}

void StMcTrack::addCtbHit(StMcCtbHit* hit)
{
  mCtbHits.push_back(hit);
}

void StMcTrack::addBemcHit(StMcCalorimeterHit* hit)
{
  mBemcHits.push_back(hit);
}

void StMcTrack::addBprsHit(StMcCalorimeterHit* hit)
{
  mBprsHits.push_back(hit);
}

void StMcTrack::addBsmdeHit(StMcCalorimeterHit* hit)
{
  mBsmdeHits.push_back(hit);
}

void StMcTrack::addBsmdpHit(StMcCalorimeterHit* hit)
{
  mBsmdpHits.push_back(hit);
}

void StMcTrack::addTofHit(StMcTofHit* hit)
{
  mTofHits.push_back(hit);
}

void StMcTrack::addMtdHit(StMcMtdHit* hit)
{
  mMtdHits.push_back(hit);
}

void StMcTrack::addEemcHit(StMcCalorimeterHit* hit)
{
  mEemcHits.push_back(hit);
}

void StMcTrack::addEprsHit(StMcCalorimeterHit* hit)
{
  mEprsHits.push_back(hit);
}

void StMcTrack::addEsmduHit(StMcCalorimeterHit* hit)
{
  mEsmduHits.push_back(hit);
}

void StMcTrack::addEsmdvHit(StMcCalorimeterHit* hit)
{
  mEsmdvHits.push_back(hit);
}

void StMcTrack::addFpdHit(StMcCalorimeterHit* hit)
{
  mFpdHits.push_back(hit);
}

void StMcTrack::addFscHit(StMcCalorimeterHit* hit)
{
  mFscHits.push_back(hit);
}

void StMcTrack::addPxlHit(StMcPxlHit* hit)
{
  mPxlHits.push_back(hit);
}

void StMcTrack::addIstHit(StMcIstHit* hit)
{
  mIstHits.push_back(hit);
}

void StMcTrack::addFgtHit(StMcFgtHit* hit)
{
  mFgtHits.push_back(hit);
}

void StMcTrack::addEtrHit(StMcEtrHit* hit)
{
  mEtrHits.push_back(hit);
}

// Not very elegant.  Maybe should have kept all collections as
// vector<StMcHit*> so that then we could have used the same
// routine for all of them... 
void StMcTrack::removeTpcHit(StMcTpcHit* hit)
{
    StMcTpcHitIterator iter = find (mTpcHits.begin(), mTpcHits.end(), hit);
    if (iter != mTpcHits.end()) {
	mTpcHits.erase(iter);
    }
}

void StMcTrack::removeFtpcHit(StMcFtpcHit* hit)
{
    StMcFtpcHitIterator iter = find (mFtpcHits.begin(), mFtpcHits.end(),hit);
    if (iter != mFtpcHits.end()) {
	mFtpcHits.erase(iter);
    }
}

void StMcTrack::removeSvtHit(StMcSvtHit* hit)
{
    StMcSvtHitIterator iter = find(mSvtHits.begin(), mSvtHits.end(), hit);
    if (iter != mSvtHits.end()) {
	mSvtHits.erase(iter);
    }
}

void StMcTrack::removeSsdHit(StMcSsdHit* hit)
{
    StMcSsdHitIterator iter = find(mSsdHits.begin(), mSsdHits.end(), hit);
    if (iter != mSsdHits.end()) {
	mSsdHits.erase(iter);
    }
}

void StMcTrack::removeRichHit(StMcRichHit* hit)
{
    StMcRichHitIterator iter = find(mRichHits.begin(), mRichHits.end(), hit);
    if (iter != mRichHits.end()){
	mRichHits.erase(iter);
    }
}

void StMcTrack::removeCtbHit(StMcCtbHit* hit)
{
    StMcCtbHitIterator iter = find(mCtbHits.begin(), mCtbHits.end(), hit);
    if (iter != mCtbHits.end()){
	mCtbHits.erase(iter);
    }
}

void StMcTrack::removeCalorimeterHit(StPtrVecMcCalorimeterHit& vch, StMcCalorimeterHit* hit)
{
    StMcCalorimeterHitIterator iter = find(vch.begin(), vch.end(), hit);
    if (iter != vch.end()) {
	vch.erase(iter);
    }
    
}
void StMcTrack::removeBemcHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mBemcHits, hit);
}
void StMcTrack::removeBprsHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mBprsHits, hit);
}
void StMcTrack::removeBsmdeHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mBsmdeHits, hit);
}
void StMcTrack::removeBsmdpHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mBsmdpHits, hit);
}

void StMcTrack::removeTofHit(StMcTofHit* hit)
{
    StMcTofHitIterator iter = find(mTofHits.begin(), mTofHits.end(), hit);
    if (iter != mTofHits.end()){
        mTofHits.erase(iter);
    }
}

void StMcTrack::removeMtdHit(StMcMtdHit* hit)
{
    StMcMtdHitIterator iter = find(mMtdHits.begin(), mMtdHits.end(), hit);
    if (iter != mMtdHits.end()){
        mMtdHits.erase(iter);
    }
}

void StMcTrack::removeEemcHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mEemcHits, hit);
}

void StMcTrack::removeEprsHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mEprsHits, hit);
}

void StMcTrack::removeEsmduHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mEsmduHits, hit);
}

void StMcTrack::removeEsmdvHit(StMcCalorimeterHit* hit)
{
    removeCalorimeterHit(mEsmdvHits, hit);
}

void StMcTrack::removePxlHit(StMcPxlHit* hit)
{
    StMcPxlHitIterator iter = find (mPxlHits.begin(), mPxlHits.end(),hit);
    if (iter != mPxlHits.end()) {
	mPxlHits.erase(iter);
    }    
}

void StMcTrack::removeIstHit(StMcIstHit* hit)
{
    StMcIstHitIterator iter = find (mIstHits.begin(), mIstHits.end(),hit);
    if (iter != mIstHits.end()) {
	mIstHits.erase(iter);
    }
}


void StMcTrack::removeFgtHit(StMcFgtHit* hit)
{
    StMcFgtHitIterator iter = find (mFgtHits.begin(), mFgtHits.end(),hit);
    if (iter != mFgtHits.end()) {
	mFgtHits.erase(iter);
    }    
}

void StMcTrack::removeEtrHit(StMcEtrHit* hit)
{
    StMcEtrHitIterator iter = find (mEtrHits.begin(), mEtrHits.end(),hit);
    if (iter != mEtrHits.end()) {
	mEtrHits.erase(iter);
    }    
}

//void StMcTrack::setTopologyMap(StTrackTopologyMap& val) { mTopologyMap = val; }
StParticleDefinition* StMcTrack::particleDefinition() { 
  if (mParticleDefinition) return mParticleDefinition; 
  if (mGeantId > 0) mParticleDefinition = StParticleTable::instance()->findParticleByGeantId(mGeantId);
  if (mParticleDefinition) return mParticleDefinition; 
  if (mPdgId)  mParticleDefinition = StParticleTable::instance()->findParticle(mPdgId);
  return mParticleDefinition;
}
//--------------------------------------------------------------------------------
const StPtrVecMcHit *StMcTrack::Hits(StDetectorId Id) const {
  StPtrVecMcHit *coll = 0;
  switch (Id) {
  case kTpcId:                  coll = (StPtrVecMcHit *) &mTpcHits; break; 	                
  case kSvtId:       	  	coll = (StPtrVecMcHit *) &mSvtHits; break; 	    
  case kRichId:      	  	coll = (StPtrVecMcHit *) &mRichHits; break;                 
  case kFtpcWestId:  	  	
  case kFtpcEastId:  	  	coll = (StPtrVecMcHit *) &mFtpcHits; break;             
  case kTofId:       	  	coll = (StPtrVecMcHit *) &mTofHits; break; 	           	 
  case kMtdId:       	  	coll = (StPtrVecMcHit *) &mMtdHits; break; 	           	 
  case kCtbId:       	  	coll = (StPtrVecMcHit *) &mCtbHits; break; 	           	 
  case kSsdId:       	  	coll = (StPtrVecMcHit *) &mSsdHits; break; 	         
  case kBarrelEmcTowerId:    	break; 
  case kBarrelEmcPreShowerId:	break; 
  case kBarrelSmdEtaStripId: 	break;
  case kBarrelSmdPhiStripId: 	break;    
  case kEndcapEmcTowerId:    	break;     
  case kEndcapEmcPreShowerId:	break;     
  case kEndcapSmdUStripId:   	break;    
  case kEndcapSmdVStripId:   	break;
  case kZdcWestId:           	
  case kZdcEastId:   	  	break;                   
  case kMwpcWestId:  	  	
  case kMwpcEastId:  	  	break;              
  case kPhmdCpvId:   	  	break;               
  case kPhmdId:      	  	break;                  
  case kPxlId:     	  	coll = (StPtrVecMcHit *) &mPxlHits; break;       
  case kIstId:       	  	coll = (StPtrVecMcHit *) &mIstHits; break;                  
  case kFgtId:  	        coll = (StPtrVecMcHit *) &mFgtHits; break;   
  case kEtrId:  	        coll = (StPtrVecMcHit *) &mEtrHits; break;   
  default:                      break;          
  };
  return coll;
}
//--------------------------------------------------------------------------------
const StPtrVecMcCalorimeterHit *StMcTrack::CalorimeterHits(StDetectorId Id) const {
  StPtrVecMcCalorimeterHit *coll = 0;
  switch (Id) {
  case kTpcId:                  break; 	                
  case kSvtId:       	  	break; 	    
  case kRichId:      	  	break;                 
  case kFtpcWestId:  	  	
  case kFtpcEastId:  	  	break;             
  case kTofId:       	  	break; 	           	 
  case kMtdId:       	  	break; 	           	 
  case kCtbId:       	  	break; 	           	 
  case kSsdId:       	  	break; 	         
  case kBarrelEmcTowerId:    	coll = (StPtrVecMcCalorimeterHit *) &mBemcHits;  break; 
  case kBarrelEmcPreShowerId:	coll = (StPtrVecMcCalorimeterHit *) &mBprsHits;  break; 
  case kBarrelSmdEtaStripId: 	coll = (StPtrVecMcCalorimeterHit *) &mBsmdeHits; break;
  case kBarrelSmdPhiStripId: 	coll = (StPtrVecMcCalorimeterHit *) &mBsmdpHits; break;    
  case kEndcapEmcTowerId:    	coll = (StPtrVecMcCalorimeterHit *) &mEemcHits;  break;     
  case kEndcapEmcPreShowerId:	coll = (StPtrVecMcCalorimeterHit *) &mEprsHits;  break;     
  case kEndcapSmdUStripId:   	coll = (StPtrVecMcCalorimeterHit *) &mEsmduHits; break;    
  case kEndcapSmdVStripId:   	coll = (StPtrVecMcCalorimeterHit *) &mEsmdvHits; break;
  case kZdcWestId:           	
  case kZdcEastId:   	  	break;                   
  case kMwpcWestId:  	  	
  case kMwpcEastId:  	  	break;              
  case kPhmdCpvId:   	  	break;               
  case kPhmdId:      	  	break;                  
  case kPxlId:     	  	break;       
  case kIstId:       	  	break;                  
  case kFgtId:   	        break;   
  case kEtrId:   	        break;   
  default:                      break;          
  };
  return coll;
}
