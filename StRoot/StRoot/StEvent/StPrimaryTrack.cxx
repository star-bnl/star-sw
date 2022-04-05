/***************************************************************************
 *
 * $Id: StPrimaryTrack.cxx,v 2.15 2013/07/23 11:21:49 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryTrack.cxx,v $
 * Revision 2.15  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.13  2013/04/10 19:15:53  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.11  2013/01/15 23:21:05  fisyak
 * improve printouts
 *
 * Revision 2.10  2012/10/23 20:18:33  fisyak
 * Add/modify print outs
 *
 * Revision 2.9  2012/05/07 14:42:57  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.8  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.7  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.6  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.5  2001/03/24 03:34:52  perev
 * clone() -> clone() const
 *
 * Revision 2.4  2000/04/03 15:12:50  ullrich
 * Fixed bug in vertex(). Always returned 0 instead of
 * data member mVertex.
 *
 * Revision 2.3  1999/11/09 15:44:08  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.2  1999/10/28 22:26:10  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "TClass.h"
#include "TString.h"
#include "StPrimaryTrack.h"
#include "StPrimaryVertex.h"
#include "StTrackGeometry.h"
ClassImp(StPrimaryTrack)

static const char rcsid[] = "$Id: StPrimaryTrack.cxx,v 2.15 2013/07/23 11:21:49 jeromel Exp $";
StPrimaryTrack::StPrimaryTrack(): mVertex(0) {/* noop */} 
const StVertex*  StPrimaryTrack::vertex() const{ return mVertex; }
void StPrimaryTrack::setVertex(StVertex* val) {
  StPrimaryVertex *p = dynamic_cast<StPrimaryVertex*>(val);
  if (p) mVertex = p;
}

void StPrimaryTrack::Streamer(TBuffer &R__b)
{
    // Stream an object of class .

    if (R__b.IsReading()) {
       UInt_t R__s, R__c;
       Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
       if (R__v > 1) {
          Class()->ReadBuffer(R__b, this, R__v, R__s, R__c);
          return;
       }
       //====process old versions before automatic schema evolution
       StTrack::Streamer(R__b);
      
//     R__b >> mVertex;
       R__b >> (StPrimaryVertex*&)mVertex;

       R__b.CheckByteCount(R__s, R__c, Class());
       //====end of old versions
      
    } else {
       Class()->WriteBuffer(R__b,this);
    }
} 
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StPrimaryTrack& track) {
    os << *((StTrack *) &track);
  Double_t charge = track.geometry()->charge();
  StThreeVectorD g3 = track.geometry()->momentum(); // p of global track
  os << Form(" q/pT %8.3f eta %8.3f phi %8.3f",
	     charge/g3.perp(),g3.pseudoRapidity(),g3.phi());
  os << Form(" pxyz %8.3f%8.3f%8.3f",g3.x(),g3.y(),g3.z());
  Double_t chi2_0 = track.fitTraits().chi2(0); if (chi2_0 > 9999.) chi2_0 = 9999.;
  Double_t chi2_1 = track.fitTraits().chi2(1); if (chi2_1 > 9999.) chi2_1 = 9999.;
  os << Form(" NF %2d chi2 %8.3f/%8.3f", track.fitTraits().numberOfFitPoints(),chi2_0,chi2_1);
#if 0
  if (track.idTruth())
    os << Form(" IdT:%5i Q:%3i", track.idTruth(), track.qaTruth());
#endif
 return os;
}
