/*****************************************************************
 * $Id: StMuPmdCluster.cxx,v 1.1 2004/04/02 03:36:20 jeromel Exp $
 *
 * Class : StMuPmdCluster
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the Cluster class for PMD in MuDst
 * ****************************************************************
 * $Log: StMuPmdCluster.cxx,v $
 * Revision 1.1  2004/04/02 03:36:20  jeromel
 * New files for PMD
 *
 * ****************************************************************/

#include "StMuPmdCluster.h"

ClassImp(StMuPmdCluster)

StMuPmdCluster::StMuPmdCluster()
{
}

StMuPmdCluster::~StMuPmdCluster()
{
}

StMuPmdCluster::StMuPmdCluster(StMuPmdCluster* cl)
{
  mSuperModule = cl->GetSuperModule();
  mNcell = cl->GetNcell();
  mEta = cl->GetEta();
  mPhi = cl->GetPhi();
  mSigma = cl->GetSigma();
  mEnergy = cl->GetEnergy();
  mEnergyPID = cl->GetEnergyPID();
  mPID = cl->GetPID();
  mMcPID = cl->GetMcPID();
}
