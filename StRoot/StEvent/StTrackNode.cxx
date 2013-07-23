/***************************************************************************
 *
 * $Id: StTrackNode.cxx,v 2.17 2013/07/23 11:21:49 jeromel Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackNode.cxx,v $
 * Revision 2.17  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.15  2013/04/10 19:15:53  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.13  2003/11/25 04:10:47  perev
 * bug in erase fixed
 *
 * Revision 2.12  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.11  2003/04/30 20:37:06  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 2.10  2002/04/24 02:26:57  ullrich
 * Replaced iterator loop by index loop in entries(StTrackType).
 *
 * Revision 2.9  2002/04/18 23:38:21  jeromel
 * Implementation of the SVT 2 tables scheme ...
 *
 * Revision 2.8  2001/04/05 04:00:58  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.7  2000/08/17 00:35:27  ullrich
 * Added hooks for handling tpt tracks.
 *
 * Revision 2.6  2000/03/23 13:49:31  ullrich
 * Not implemented track type 'secondary' now handled
 * in a more clean way; entries(secondary) returns 0.
 *
 * Revision 2.5  1999/12/01 20:04:58  ullrich
 * Fixed bug in track() method.
 *
 * Revision 2.4  1999/11/09 15:44:17  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.3  1999/11/05 15:27:10  ullrich
 * Added non-const versions of several methods
 *
 * Revision 2.2  1999/10/28 22:27:44  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StTrackNode.h"
#include "StGlobalTrack.h"
#include "StPrimaryTrack.h"

ClassImp(StTrackNode)

static const char rcsid[] = "$Id: StTrackNode.cxx,v 2.17 2013/07/23 11:21:49 jeromel Exp $";

StTrackNode::StTrackNode() { /* noop */ }

StTrackNode::~StTrackNode() { /* noop */ }

void
StTrackNode::addTrack(StTrack* track)
{
    if (track) {
        switch (track->type()) {
        case primary:
        case estPrimary:
            mReferencedTracks.push_back(track);
            break;
        case secondary:                // not implemented yet
            cerr << "StTrackNode::addTrack(): track type 'secondary' not implemented yet." << endl;
            break;
        case global:
        case tpt:
        case estGlobal:
            mOwnedTracks.push_back(track);
            break;
        default:
            cerr << "StTrackNode::addTrack(): cannot add, unknown track type." << endl;
            return;
            break;
        }
        track->setNode(this);
    }
}

void
StTrackNode::removeTrack(StTrack* track)
{
    StPtrVecTrackIterator  iter;
    StSPtrVecTrackIterator iterS;
    if (track) {
        switch (track->type()) {
        case primary:
        case estPrimary:
            for (iter = mReferencedTracks.begin(); iter < mReferencedTracks.end(); iter++)
                if (*iter == track) mReferencedTracks.erase(iter);
            break;
        case secondary:                // not implemented yet
            cerr << "StTrackNode::removeTrack(): track type 'secondary' not implemented yet." << endl;
            break;
        case tpt:
        case global:
        case estGlobal:
            for (iterS = mOwnedTracks.begin(); iterS < mOwnedTracks.end(); iterS++)
                if (*iterS == track) mOwnedTracks.erase(iterS);
            break;
        default:
            cerr << "StTrackNode::removeTrack(): cannot remove, unknown track type." << endl;
            break;
        }
        track->setNode(0);
    }
}

unsigned int
StTrackNode::entries() const
{
    return mReferencedTracks.size() + mOwnedTracks.size();
}

const StTrack*
StTrackNode::track(unsigned int i) const
{
    if (i < mOwnedTracks.size())
        return mOwnedTracks[i];
    else {
        i -= mOwnedTracks.size();
        if (i < mReferencedTracks.size())
            return mReferencedTracks[i];
        else
            return 0;
    }
}

StTrack*
StTrackNode::track(unsigned int i)
{
    if (i < mOwnedTracks.size())
        return mOwnedTracks[i];
    else {
        i -= mOwnedTracks.size();
        if (i < mReferencedTracks.size())
            return mReferencedTracks[i];
        else
            return 0;
    }
}

unsigned int
StTrackNode::entries(StTrackType type) const
{
    unsigned int i;
    //VPunused StSPtrVecTrackConstIterator iterS;
    //VPunused StPtrVecTrackConstIterator  iter;
    unsigned int                counter;

    switch (type) {
    case primary:
    case estPrimary:
        for (counter=0, i=0; i < mReferencedTracks.size(); i++)
            if (mReferencedTracks[i]->type() == type) counter++;
        return counter;
        break;
    case secondary:                // not implemented yet
        cerr << "StTrackNode::entries(): track type 'secondary' not implemented yet." << endl;
        return 0;
        break;
    case tpt:
    case global:
    case estGlobal:
        for (counter=0, i=0; i < mOwnedTracks.size(); i++)
            if (mOwnedTracks[i]->type() == type) counter++;
        return counter;
        break;
    default:
        cerr << "StTrackNode::entries(): unknown track type." << endl;
        return 0;
        break;
    }
}

const StTrack*
StTrackNode::track(StTrackType type, unsigned int i) const
{
    int          j;
    unsigned int k;

    switch (type) {
    case primary:
    case estPrimary:
        for (j=-1, k=0; k < mReferencedTracks.size(); k++) {
            if (mReferencedTracks[k]->type() == type) j++;
            if (j == static_cast<int>(i)) return mReferencedTracks[k];
        }
        return 0;
        break;
    case secondary:                // not implemented yet
        cerr << "StTrackNode::track(): track type 'secondary' not implemented yet." << endl;
        return 0;
        break;
    case tpt:
    case global:
    case estGlobal:
        for (j=-1, k=0; k < mOwnedTracks.size(); k++) {
            if (mOwnedTracks[k]->type() == type) j++;
            if (j == static_cast<int>(i)) return mOwnedTracks[k];
        }
        return 0;
        break;
    default:
        cerr << "StTrackNode::track(): unknown track type." << endl;
        return 0;
        break;
    }
}
 
StTrack*
StTrackNode::track(StTrackType type, unsigned int i)
{
    int          j;
    unsigned int k;

    switch (type) {
    case primary:
    case estPrimary:
        for (j=-1, k=0; k < mReferencedTracks.size(); k++) {
            if (mReferencedTracks[k]->type() == type) j++;
            if (j == static_cast<int>(i)) return mReferencedTracks[k];
        }
	return 0;
        break;
    case secondary:                // not implemented yet
        cerr << "StTrackNode::track(): track type 'secondary' not implemented yet." << endl;
        return 0;
        break;
    case tpt:
    case global:
    case estGlobal:
        for (j=-1, k=0; k < mOwnedTracks.size(); k++) {
            if (mOwnedTracks[k]->type() == type) j++;
            if (j == static_cast<int>(i)) return mOwnedTracks[k];
        }
        return 0;
        break;
    default:
        cerr << "StTrackNode::track(): unknown track type." << endl;
        return 0;
        break;
    }
}

