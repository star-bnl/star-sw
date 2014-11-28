/***************************************************************************
 *
 * $Id: StL3AlgorithmInfo.cxx,v 2.3 2003/05/23 20:40:44 ullrich Exp $
 *
 * Author: Christof Struck, July 2001
 ***************************************************************************
 *
 * Description: L3 Algorithm Information
 *
 ***************************************************************************
 *
 * $Log: StL3AlgorithmInfo.cxx,v $
 * Revision 2.3  2003/05/23 20:40:44  ullrich
 * Removed dependcies on DAQ lib in header file.
 *
 * Revision 2.2  2001/08/20 21:29:00  ullrich
 * Changed counter type from UInt_t to Int_t.
 *
 * Revision 2.1  2001/08/02 01:26:31  ullrich
 * Initial Revision.
 *
 **************************************************************************/

#include <string.h>
#include "StL3AlgorithmInfo.h"
#include "StDaqLib/L3/L3_Banks.hh"

static const char rcsid[] = "$Id: StL3AlgorithmInfo.cxx,v 2.3 2003/05/23 20:40:44 ullrich Exp $";

ClassImp(StL3AlgorithmInfo)


StL3AlgorithmInfo::StL3AlgorithmInfo()
{
  initArrays();
  mId = -1;
  mOn = false;
  mAccept = false;
  mBuild = false;
  mNumberOfProcessedEvents = 0;
  mNumberOfAcceptedEvents = 0;
  mNumberOfBuildEvents = 0;
}

StL3AlgorithmInfo::StL3AlgorithmInfo(Algorithm_Data* raw)
{
  initArrays();
  mId = raw->algId;
  mOn = raw->on;
  mAccept = raw->accept;
  mBuild = raw->build;
  mNumberOfProcessedEvents = raw->nProcessed;
  mNumberOfAcceptedEvents = raw->nAccept;
  mNumberOfBuildEvents = raw->nBuild;
  mDataArray.Set(mDataSize, (Float_t* )raw->data);
}


StL3AlgorithmInfo::~StL3AlgorithmInfo()
{
}


void StL3AlgorithmInfo::initArrays()
{
  mDataSize = 10;
  mIntParameterSize = 5;
  mFloatParameterSize = 5;

  mDataArray.Set(mDataSize);
  mIntParameterArray.Set(mIntParameterSize);
  mFloatParameterArray.Set(mFloatParameterSize);
}


void StL3AlgorithmInfo::setCounters(int nProcessed, int nAccepted, int nBuild)
{
  mNumberOfProcessedEvents = nProcessed;
  mNumberOfAcceptedEvents = nAccepted;
  mNumberOfBuildEvents = nBuild;
}


void StL3AlgorithmInfo::setParameters(int* gi, float* gf)
{
  mIntParameterArray.Set(mIntParameterSize, (Int_t* )gi);
  mFloatParameterArray.Set(mFloatParameterSize, (Float_t* )gf);
}


void StL3AlgorithmInfo::setPreScale(int pre)
{
  mPreScale = pre;
}


void StL3AlgorithmInfo::setPostScale(int post)
{
  mPostScale = post;
}
