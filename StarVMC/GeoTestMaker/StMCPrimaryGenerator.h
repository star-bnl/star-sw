// $Id: StMCPrimaryGenerator.h,v 1.1 2009/03/25 23:15:10 perev Exp $
//
//
// Class StMCPrimaryGenerator
// ------------------


#ifndef STAR_StMCPrimaryGenerator_H
#define STAR_StMCPrimaryGenerator_H

#include "GCall.h"
#include "TVirtualMCStack.h"
class StMCPrimaryGenerator : public GCall
{
  public:
    StMCPrimaryGenerator(int ntrk=1,int pdg=0);
    virtual ~StMCPrimaryGenerator(){}    
    // methods
    void SetNTrk(int ntrk)	{fNTrk=ntrk;}
    void SetPDG (int pdg )	{fPDG =pdg ;}
    void SetVtx (const double *vtx );
    void SetVtx (const float  *vtx );
    void PushTrack(int toBeDone, int parent, int pdg
	                  ,double px,  double py,  double pz,double e 
			  ,double vx,  double vy,  double vz,double tof
			  ,double polx,double poly,double polz
			  ,TMCProcess mech, int& ntr, double weight, int is);
  protected:
    // data members
    int fNTrk;
    int fPDG;
    double fVtx[3];
    ClassDef(StMCPrimaryGenerator,0) // Extended TParticle
};

#endif //STAR_StMCPrimaryGenerator_H 
   


