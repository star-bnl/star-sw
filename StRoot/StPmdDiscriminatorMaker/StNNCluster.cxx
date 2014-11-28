/************************************************************
 *
 * $Id: StNNCluster.cxx,v 1.1 2003/05/29 13:20:12 subhasis Exp $
 *
 * Author:  Subhasis Chattopadhyay
 ************************************************************
 * 
 * Description: StNNCluster is class for PMD NN cluster. 
 *
 ************************************************************
 *
 * $Log: StNNCluster.cxx,v $
 * Revision 1.1  2003/05/29 13:20:12  subhasis
 *  NN Input cluster
 *
 ************************************************************/
#include "StNNCluster.h"
#include "StEventTypes.h"


ClassImp(StNNCluster)

//_____________________________________________________________________________

StNNCluster::StNNCluster() : StObject() 
{
mpmdcluster=NULL;
mcpvcluster=NULL;
}
//________________________________________________

StNNCluster::StNNCluster(StPhmdCluster* pmd, StPhmdCluster* cpv) : StObject() 
{
mpmdcluster=pmd;
mcpvcluster=cpv;
}

