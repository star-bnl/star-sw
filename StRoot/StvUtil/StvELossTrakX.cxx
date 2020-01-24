// $Id: StvELossTrakX.cxx,v 1.1.2.1 2020/01/24 20:41:14 perev Exp $
//
//
// Class StvELossTrak
// ------------------
#include "StvELossTrakX.h"

#include "StvUtil/StvDebug.h"
ClassImp(StvELossTrakX)

//_____________________________________________________________________________
void StvELossTrakX::Add(double len)
{
  StvELossTrak::Add(len);
  double Tend = fE-fM;
  Tend+= (fDir)? -fELoss: fELoss;
  double dEdXend = gdrelx(fA,fZ,fDens,Tend,fM)*fDens*fCharge2;
  double dEdXave = (fdEdX + dEdXend)/2;
  double ELoss = dEdXave*fLen;
  fd2EdXdE = (dEdXend-fdEdX)/ELoss;
  if (fDir)fd2EdXdE = -fd2EdXdE; 

}
