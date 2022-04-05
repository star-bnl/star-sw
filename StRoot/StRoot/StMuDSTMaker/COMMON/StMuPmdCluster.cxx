/*****************************************************************
 * $Id: StMuPmdCluster.cxx,v 1.2 2004/10/19 01:44:29 mvl Exp $
 *
 * Class : StMuPmdCluster
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the Cluster class for PMD in MuDst
 * ****************************************************************
 * $Log: StMuPmdCluster.cxx,v $
 * Revision 1.2  2004/10/19 01:44:29  mvl
 * Changed names of getters and setters (Star coding conventions)
 *
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
  mSuperModule = cl->superModule();
  mNcell = cl->ncell();
  mEta = cl->eta();
  mPhi = cl->phi();
  mSigma = cl->sigma();
  mEnergy = cl->energy();
  mEnergyPID = cl->energyPID();
  mPID = cl->PID();
  mMcPID = cl->mcPID();
}
