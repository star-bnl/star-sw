/************************************************************
 *
 * $Id: StPmdCluster.cxx,v 1.1 2002/08/27 12:21:35 subhasis Exp $
 *
 * Author:  Subhasis Chattopadhyay
 ************************************************************
 * 
 * Description: StPmdCluster is base class for PMD cluster. 
 *
 ************************************************************
 *
 * $Log: StPmdCluster.cxx,v $
 * Revision 1.1  2002/08/27 12:21:35  subhasis
 * First version
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
  mNumofMems = 0; mModule = 0;
  
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

