// @(#)root/eg:$Id: StExampleFilter.cxx,v 1.2 2009/04/20 20:44:17 perev Exp $
// Author: Victor Perev  17/03/2009

//______________________________________________________________________________
#include "stdlib.h"
#include "math.h"

#include "StExampleFilter.h"
#include "StGenParticle.h"

/// 	IMPORTANT IMPORTANT IMPORTANT
/// Defining the static instance of user filter provides creating this
/// class during the loading of library. Afterward GEANT could select 
/// the needed filter by name. 
/// more info in http://www.star.bnl.gov/~perev/SIM/mcFi;lter.ppt
static StExampleFilter qwerty;
//______________________________________________________________________________
int StExampleFilter::RejectEG(const StGenParticles &ptl) const
{
//  ptl.Print("************** In RejectEG ************** ");
// Condition: number of tracks etaGate[0]<eta<= etaGate[1] must be bigger  etaGate[2]
const static double etaGate[3]={0.8,1.2, 3};
  const StGenParticle *tk=0;
  int n = ptl.Size();
  int ntk=0;
  for (int i=0;i<n;i++) {
    tk = ptl(i); if (!tk) 	continue;
    if (tk->GetStatusCode()!=1) continue;
    if (tk->Eta() < etaGate[0]) continue;
    if (tk->Eta() > etaGate[1]) continue;
    ntk++;
  }

  if (ntk<etaGate[2]) return 1;
  return 0;
}
//______________________________________________________________________________
int StExampleFilter::RejectGT(const StGenParticles &ptl) const
{
//  ptl.Print("************** In RejectGT ************** ");
  return 0;
}
//______________________________________________________________________________
int StExampleFilter::RejectGE(const StGenParticles &ptl) const
{
//  ptl.Print("************** In RejectGE ************** ");
  return 0;
}
