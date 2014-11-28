/**********************************************************************
 *
 * $Id: StEStructCentrality.cxx,v 1.8 2010/03/02 21:47:18 prindle Exp $
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

    if (mcentralities) {
        delete [] mcentralities;
    }
    if (mpts) {
        delete [] mpts;
    }
    if (mptcents) {
        delete [] mptcents;
    }

}

//--------------------------------------------------------------------
// Feb 26, 2004 djp Change from UInt_t to Double_t and check that
//                  input is within range covered by definition.
int StEStructCentrality::centrality(const double impact) {
    if(!mcentralities) {
         if(warningCount<10){
          warningCount++;
	  cout<<" Error:: centrality requested without initialization (warning #"<<warningCount<<")"<<endl;
	}
        return -1;
    }

    mValue = impact;
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

int StEStructCentrality::ptIndex(const double pt) {
    if(!mpts) {
        if(warningCount<10){
          warningCount++;
	  cout<<" Error:: Pt index requested without initialization (warning #"<<warningCount<<")"<<endl;
	}
        return -1;
    }

    int found  = 0;
    int retVal = 0;
    for(retVal=0;retVal<mnumpts-1;retVal++) {
        if(pt>=mpts[retVal] && pt<mpts[retVal+1]) {
          found = 1;
          break;
        }
    }
    if (!found) {
        return -1;
    }
    return retVal;
}

int StEStructCentrality::ptCentrality(const double cent) {
    if(!mptcents) {
        if(warningCount<10){
          warningCount++;
	  cout<<" Error:: Pt centrality index requested without initialization (warning #"<<warningCount<<")"<<endl;
	}
        return -1;
    }

    int found  = 0;
    int retVal = 0;
    for(retVal=0;retVal<mnumptcents-1;retVal++) {
        if(cent>=mptcents[retVal] && cent<mptcents[retVal+1]) {
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

    if (!centralities || num==0) {
        return;
    }
    if (mcentralities) {
        delete [] mcentralities;
    }
    mcentralities = new double[num];
    for (int i=0;i<num;i++) {
        mcentralities[i]=centralities[i];
    }
    mnumCentralities=num;
}

int StEStructCentrality::numCentralities() {
  if(!mcentralities) {  // this is set with mnumCentralities in setCentralities
        if(warningCount<10){
          warningCount++;
	  cout<<" Error:: numCentralities requested without initialization (warning #"<<warningCount<<")"<<endl;
	}
    return -1;
  }

  return mnumCentralities;
}

double StEStructCentrality::centralityLimit( const int index ) {
    if ((index < 0) || (mnumCentralities <index)) {
        return -1;
    }
    return mcentralities[index];
}


void StEStructCentrality::setPts( const double* pts,   const int numPt,
                                  const double* cents, const int numCent ) {
    if (!pts || numPt==0 || !cents || numCent==0) {
        return;
    }
    if(mpts) {
        delete [] mpts;
    }
    if(mptcents) {
        delete [] mptcents;
    }
    mpts = new double[numPt];
    for (int i=0;i<numPt;i++) {
        mpts[i] = pts[i];
    }
    mnumpts = numPt;
    mptcents = new double[numCent];
    for (int i=0;i<numCent;i++) {
        mptcents[i] = cents[i];
    }
    mnumptcents=numCent;
}
void StEStructCentrality::setPtLimit( const int index, double pt ) {
    if ((index < 0) || (mnumpts <index)) {
        return;
    }
    mpts[index] = pt;
}
int StEStructCentrality::numPts() {
    return mnumpts;
}
int StEStructCentrality::numPtCentralities() {
    return mnumptcents;
}
double StEStructCentrality::ptLimit( const int index ) {
    if ((index < 0) || (mnumpts <index)) {
        return -1;
    }
    return mpts[index];
}
double StEStructCentrality::ptCentralityLimit( const int index ) {
    if ((index < 0) || (mnumptcents <index)) {
        return -1;
    }
    return mptcents[index];
}
void StEStructCentrality::Print() {
  if(!mcentralities) {
    cout << "Print::Centralities not initialized!" << endl;
    return;
  }
  cout << "Centrality bin definitions:" << endl;
  cout << "  bin\tmin\tmax" <<endl;
  for(int i=0; i<numCentralities()-1; i++) cout << "  " << i << "\t" << minCentrality(i) << "\t" << maxCentrality(i) << endl;
}
  

/***********************************************************************
 *
 * $Log: StEStructCentrality.cxx,v $
 * Revision 1.8  2010/03/02 21:47:18  prindle
 *   Support to retrieve track radius when it crosses endplate
 *   Add way to retrieve centrality
 *
 * Revision 1.7  2007/01/26 17:19:49  msd
 * Added Print function.
 *
 * Revision 1.6  2006/04/04 22:12:29  porter
 * Set up StEtructCentrality for use in event cut selection - includes impact para for generators
 *
 * Revision 1.5  2005/09/29 17:41:50  msd
 * Added initialization check to numCentralities
 *
 * Revision 1.4  2004/09/24 01:46:45  prindle
 * Added call for setPtLimit. I use this in fluctuations which prevented
 * a fresh CVS checkout from compiling
 *
 * Revision 1.3  2004/06/09 22:39:06  prindle
 * Expanded centrality class.
 * Call to set centrality from event reader.
 *
 *
 * CVS :nded ----------------------------------------------------------------------
 *
 * Revision 1.2  2004/02/27 02:28:02  prindle
 *
 * Small modification to StEStructCentrality in EventMaker branch.
 * Many modifications to Fluctuations branch, although that branch is not
 * stable yet.
 *
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/






