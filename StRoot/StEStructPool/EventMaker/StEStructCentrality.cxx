/**********************************************************************
 *
 * $Id: StEStructCentrality.cxx,v 1.1 2003/10/15 18:20:51 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  allows run-time definition of event class centrality
 *               this used to be persistent with dst...
 *
 ***********************************************************************/
#include "StEStructCentrality.h"
#include "Stiostream.h"

ClassImp(StEStructCentrality)

StEStructCentrality* StEStructCentrality::mInstance=0;

StEStructCentrality* StEStructCentrality::Instance(){

  if(!mInstance) mInstance=new StEStructCentrality();    
  return mInstance;
}


//--------------------------------------------------------------------
StEStructCentrality::~StEStructCentrality(){ 

if(mcentralities) delete [] mcentralities;

}

//--------------------------------------------------------------------
int StEStructCentrality::centrality(const int ncharge){

  if(!mcentralities) {
    cout<<" Error:: centralitiy requested without initialization "<<endl;
    return -1;
  }

  int retVal=0;
  for(retVal=0;retVal<mnumCentralities-1;retVal++)
   if(ncharge>=mcentralities[retVal] && ncharge<mcentralities[retVal+1]) break;

  return retVal;

}

//--------------------------------------------------------------------
void StEStructCentrality::setCentralities(const int* centralities, const int num){

  if(!centralities || num==0) return;
  if(mcentralities) delete [] mcentralities;
  mcentralities = new int[num];
  for(int i=0;i<num;i++)mcentralities[i]=centralities[i];
  mnumCentralities=num;
}

/***********************************************************************
 *
 * $Log: StEStructCentrality.cxx,v $
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/






