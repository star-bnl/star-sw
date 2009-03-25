// $Id: StMCPrimaryGenerator.cxx,v 1.1 2009/03/25 23:15:10 perev Exp $
//
//
// Class StMCPrimaryGenerator
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include "StMCPrimaryGenerator.h"
#include "TVirtualMC.h"
#include "TPDGCode.h"
#include "TRandom.h"



ClassImp(StMCPrimaryGenerator)

//_____________________________________________________________________________
StMCPrimaryGenerator::StMCPrimaryGenerator(int ntrk,int pdg)
  : GCall("StMCPrimaryGenerator","")
{
   SetNTrk(ntrk); SetPDG(pdg); memset(fVtx,0,sizeof(fVtx));
}   
//_____________________________________________________________________________
void StMCPrimaryGenerator::SetVtx(const double *vtx)
{
 for (int i=0;i<3;i++){fVtx[i]=vtx[i];}
}
//_____________________________________________________________________________
void StMCPrimaryGenerator::SetVtx(const float  *vtx)
{
 for (int i=0;i<3;i++){fVtx[i]=vtx[i];}
}

//_____________________________________________________________________________
void StMCPrimaryGenerator::PushTrack(
  int toBeDone, int parent, int pdg
 ,double px,  double py,  double pz,double e 
 ,double vx,  double vy,  double vz,double tof
 ,double polx,double poly,double polz
 ,TMCProcess mech, int& ntr, double weight, int is)
{
 TVirtualMC::GetMC()->GetStack()->PushTrack(
  toBeDone,parent,pdg,px,  py,  pz,e ,vx,  vy,  vz,tof
 ,polx,poly,polz, mech, ntr, weight,is);
}





		
		
		
