/***************************************************************************
 *
 * $Id: StSvtCluster.cc,v 1.1 2000/07/06 03:50:33 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: SVT Cluster BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtCluster.cc,v $
 * Revision 1.1  2000/07/06 03:50:33  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// It is a collection of StSvtHybridCluster objects.                      // 
// It represents the entire Cluster data.                                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "StSvtCluster.hh"

ClassImp(StSvtCluster)

  StSvtCluster::StSvtCluster(char* config,int event, int trigger):StSvtHybridCollection(config)
{
  //    event number and trigger type

  mEventNumber = event;
  mTriggerType = trigger;
}

StSvtCluster::StSvtCluster(const StSvtCluster& mClu)
{
 mEventNumber = mClu.mEventNumber;
 mTriggerType = mClu.mTriggerType;

}

/*StSvtCluster& operator = (const StSvtCluster& mClu)
{
 mEventNumber = mClu.mEventNumber;
 mTriggerType = mClu.mTriggerType;
 return *this;
 }*/

StSvtCluster::~StSvtCluster()
{}


