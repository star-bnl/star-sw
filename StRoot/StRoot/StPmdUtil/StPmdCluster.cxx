/************************************************************
 *
 * $Id: StPmdCluster.cxx,v 1.4 2010/05/28 17:24:19 rashmi Exp $
 *
 * Author:  Subhasis Chattopadhyay
 ************************************************************
 * 
 * Description: StPmdCluster is base class for PMD cluster. 
 *
 ************************************************************
 *
 * $Log: StPmdCluster.cxx,v $
 * Revision 1.4  2010/05/28 17:24:19  rashmi
 * Added initialization of mCluSigma in constructor
 *
 * Revision 1.3  2003/10/14 07:28:53  subhasis
 * CluX, CluY added
 *
 * Revision 1.2  2003/05/12 12:07:12  subhasis
 * Mapping added
 *
 ************************************************************/
#include "StPmdCluster.h"


ClassImp(StPmdCluster)

//_____________________________________________________________________________

StPmdCluster::StPmdCluster(TArrayI *cluster) : StObject() 
{
}
//________________________________________________

StPmdCluster::StPmdCluster() : StObject() 
{
  mCluEdep = 0.0; mCluEta = 0.0; mCluPhi = 0.0; 
  mNumofMems = 0.0 ; mModule = 0;
  mCluSigmaS = 0.0; mCluSigmaL = 0.0;
  mCluPID=0;  mCluEdepPID=0;  mMcCluPID=0;
  mCluX = 0.0; mCluY = 0.0;  

}
//__________________________________________________

void 
StPmdCluster::print(ostream *os)
{
//! Printing member function.
  *os << " Module " << Module();
  *os << " Members " << NumofMems();
  *os << " Edep " << CluEdep();
  *os << " eta " << CluEta();
  *os <<"  phi " << CluPhi()<< endl;
}

ostream &operator<<(ostream &os, StPmdCluster &cl)
{
  cl.print(&os); return os;
}

//--------------------------------------------------
void 
StPmdCluster::Browse(TBrowser *b)
{
  cout << (*this) << endl;
  StObject::Browse(b);
}

void StPmdCluster::addHitCollection(StPmdHit* hits)
{
      mHitCollection.Add(hits);
 }



