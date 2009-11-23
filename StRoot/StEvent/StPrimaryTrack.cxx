/***************************************************************************
 *
 * $Id: StPrimaryTrack.cxx,v 2.8 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StPrimaryTrack.cxx,v $
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
#include "StPrimaryTrack.h"
#include "StPrimaryVertex.h"

ClassImp(StPrimaryTrack)

static const char rcsid[] = "$Id: StPrimaryTrack.cxx,v 2.8 2009/11/23 16:34:06 fisyak Exp $";

StPrimaryTrack::StPrimaryTrack() : mVertex(0) {/* noop */}

StPrimaryTrack::StPrimaryTrack(const StPrimaryTrack& track) :
    StTrack(track)
{
    mVertex = track.mVertex;
}

StPrimaryTrack&
StPrimaryTrack::operator=(const StPrimaryTrack& track)
{
    if (this != &track) {
        static_cast<StTrack&>(*this) = track;
        mVertex = track.mVertex;
    }
    return *this;
}

StPrimaryTrack::~StPrimaryTrack() {/* noop */}

StTrackType
StPrimaryTrack::type() const { return primary; }

const StVertex*
StPrimaryTrack::vertex() const { return mVertex; }

void
StPrimaryTrack::setVertex(StVertex* val)
{
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
