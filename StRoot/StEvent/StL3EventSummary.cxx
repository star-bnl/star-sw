/***************************************************************************
 *
 * $Id: StL3EventSummary.cxx,v 2.1 2001/08/02 01:26:31 ullrich Exp $
 *
 * Author: Christof Struck, July 2001
 ***************************************************************************
 *
 * Description: L3 Event Summary Information
 *
 ***************************************************************************
 *
 * $Log: StL3EventSummary.cxx,v $
 * Revision 2.1  2001/08/02 01:26:31  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/

#include "StL3EventSummary.h"
#include "StL3AlgorithmInfo.h"


static const char rcsid[] = "$Id: StL3EventSummary.cxx,v 2.1 2001/08/02 01:26:31 ullrich Exp $";

ClassImp(StL3EventSummary)


StL3EventSummary::StL3EventSummary()
{
  mNumberOfProcessedEvents = 0;
  mNumberReconstructedEvents = 0;
  mNumberOfTracks = 0;
  mNumberOfAlgorithms = 0;
  mZVertexTrigger = false;
  mUnbiasedTrigger = false;
}


StL3EventSummary::StL3EventSummary(Bank_L3_SUMD *raw)
{
  mNumberOfProcessedEvents = raw->nProcessed;
  mNumberReconstructedEvents = raw->nReconstructed;
  mNumberOfTracks = 0;
  mNumberOfAlgorithms = raw->nAlg;
  mZVertexTrigger = false;
  mUnbiasedTrigger = false;
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
	if (alg->id()==1) mUnbiasedTrigger = true;
	//if (alg->id()==XXXX) mZVertexTrigger = true;
  }
}
