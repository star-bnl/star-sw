/***************************************************************************
 *
 * $Id: StSvtCluster.hh,v 1.1 2000/07/06 03:50:33 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: SVT Cluster BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtCluster.hh,v $
 * Revision 1.1  2000/07/06 03:50:33  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/

#ifndef STSVTCLUSTER_HH
#define STSVTCLUSTER_HH

#include <iostream.h>
#include "StSvtClassLibrary/StSvtHybridCollection.hh"


class StSvtCluster : public StSvtHybridCollection
{
public:
   StSvtCluster(char * config,int event = 0, int trigger = 0);
   StSvtCluster(const StSvtCluster&);
   ~ StSvtCluster();
   StSvtCluster& operator = (const StSvtCluster&);   

  int getEventNumber() {return mEventNumber;}
  int getTriggerType() {return mTriggerType;}

protected:
 
  int mEventNumber; // Event Number
  int mTriggerType; // Trigger Type
  
  ClassDef(StSvtCluster,1)

};

#endif
