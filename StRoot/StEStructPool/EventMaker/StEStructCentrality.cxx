/**********************************************************************
 *
 * $Id: StEStructCentrality.cxx,v 1.2 2004/02/27 02:28:02 prindle Exp $
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
// Feb 26, 2004 djp Change from UInt_t to Double_t and check that
//                  input is within range covered by definition.
int StEStructCentrality::centrality(const double impact){

  if(!mcentralities) {
    cout<<" Error:: centrality requested without initialization "<<endl;
    return -1;
  }

  int found  = 0;
  int retVal = 0;
  for(retVal=0;retVal<mnumCentralities-1;retVal++) {
    if(impact>=mcentralities[retVal] && impact<mcentralities[retVal+1]) {
      found = 1;
      break;
    }
  }
  if (!found) {
    return -1;
  }
  return retVal;

}

//--------------------------------------------------------------------
void StEStructCentrality::setCentralities(const double* centralities, const int num){

  if(!centralities || num==0) return;
  if(mcentralities) delete [] mcentralities;
  mcentralities = new double[num];
  for(int i=0;i<num;i++)mcentralities[i]=centralities[i];
  mnumCentralities=num;
}

/***********************************************************************
 *
 * $Log: StEStructCentrality.cxx,v $
 * Revision 1.2  2004/02/27 02:28:02  prindle
 * Small modification to StEStructCentrality in EventMaker branch.
 * Many modifications to Fluctuations branch, although that branch is not
 * stable yet.
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/






