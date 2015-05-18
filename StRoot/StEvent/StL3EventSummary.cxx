/***************************************************************************
 *
 * $Id: StL3EventSummary.cxx,v 2.5 2015/05/15 18:25:06 ullrich Exp $
 *
 * Author: Christof Struck, July 2001
 ***************************************************************************
 *
 * Description: L3 Event Summary Information
 *
 ***************************************************************************
 *
 * $Log: StL3EventSummary.cxx,v $
 * Revision 2.5  2015/05/15 18:25:06  ullrich
 * Fixed nonsense if() statement in constructor.
 *
 * Revision 2.4  2004/03/26 20:59:28  dietel
 * Check pointer to L3_SUMD in constructor
 *
 * Revision 2.3  2001/11/14 23:29:35  struck
 * minor changes in 'unbiasedTrigger' function, trigger word added for debugging purposes
 *
 * Revision 2.2  2001/08/20 21:29:53  ullrich
 * Added method setCounters().
 *
 * Revision 2.1  2001/08/02 01:26:31  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/

#include "StL3EventSummary.h"
#include "StL3AlgorithmInfo.h"

#include "StMessMgr.h"

static const char rcsid[] = "$Id: StL3EventSummary.cxx,v 2.5 2015/05/15 18:25:06 ullrich Exp $";

ClassImp(StL3EventSummary)


StL3EventSummary::StL3EventSummary()
{
    mNumberOfProcessedEvents = 0;
    mNumberReconstructedEvents = 0;
    mNumberOfTracks = 0;
    mNumberOfAlgorithms = 0;
    mZVertexTrigger = false;
    mUnbiasedTrigger = false;
    mUnbiasedPreScale = 0;
}


StL3EventSummary::StL3EventSummary(Bank_L3_SUMD *raw)
{
    if(!raw) {
        
        gMessMgr->Warning() << "No L3_SUMD bank" << endm;
        
        mNumberOfProcessedEvents = -1;
        mNumberReconstructedEvents = -1;
        mNumberOfTracks = 0;
        mNumberOfAlgorithms = 0;
        mZVertexTrigger = false;
        mUnbiasedTrigger = true;
        mUnbiasedPreScale = 0;
        
    }
    else {
        mNumberOfProcessedEvents = raw->nProcessed;
        mNumberReconstructedEvents = raw->nReconstructed;
        mNumberOfTracks = 0;
        mNumberOfAlgorithms = raw->nAlg;
        mZVertexTrigger = false;
        mUnbiasedTrigger = false;
        mUnbiasedPreScale = 0;
    }
}


void StL3EventSummary::setNumberOfTracks(int ntracks)
{
    mNumberOfTracks = ntracks;
}


void StL3EventSummary::addAlgorithm(StL3AlgorithmInfo *alg)
{
    // add to structural vector
    mL3Algorithms.push_back(alg);
    
    // if this algorithm built this event
    if (alg->build()) {
        mL3AcceptAlgorithms.push_back(alg);
        // check for zVertex trigger and 'TRUE'-algorithm
        // cs 11/09/01: TRUE algorithm check now in l3RawReaderMaker
        //if (alg->id()==1) mUnbiasedTrigger = true;
        if (alg->id()==5) mZVertexTrigger = true;
    }
}
