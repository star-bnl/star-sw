/* $Id: StarGenerator.cxx,v 1.2 2004/07/16 22:52:35 potekhin Exp $ */

#include <TGenerator.h>
#include <TMCProcess.h>

// #include "StarCollisionGeometry.h"
// #include "StarConfig.h"

#include "StarGenerator.h"
#include "StarStack.h"

// #include "StarRun.h"
// #include "StarMC.h"
// #include "StarVertexGenerator.h"

ClassImp(StarGenerator)


//_______________________________________________________________________
StarGenerator::StarGenerator():
  _stack(0),_seed(0)
 /*
  fMCEvGen(0),
  fThetaMin(0),
  fThetaMax(0),
  fPhiMin(0),
  fPhiMax(0),
  fPMin(0),
  fPMax(0),
  fPtMin(0),
  fPtMax(0),
  fYMin(0),
  fYMax(0),
  fVMin(3),
  fVMax(3),
  fNpart(0),
  fParentWeight(0),
  fChildWeight(0),
  fAnalog(0),
  fVertexSmear(kNoSmear),
  fVertexSource(kInternal),
  fCutVertexZ(0),
  fTrackIt(0),
  fOrigin(3),
  fOsigma(3),
  fVertex(3),
  fCollisionGeometry(0)
				*/

{
  //
  // Default constructor
  //

  /*
    if (gStarce) {
	if (gStarce->GetDebug()>0)
	    printf("\n StarGenerator Default Constructor\n\n");
	StarMC * mc = gStarce->GetMCApp();
	if (mc) mc->SetGenerator(this);
    }

    SetThetaRange(); ResetBit(kThetaRange);
    SetPhiRange(); ResetBit(kPhiRange);
    SetMomentumRange(); ResetBit(kMomentumRange);
    SetPtRange(); ResetBit(kPtRange);
    SetYRange(); ResetBit(kYRange);
    SetNumberParticles();
    SetTrackingFlag();
    SetCutVertexZ();


    fOrigin[0]=fOrigin[1]=fOrigin[2]=0;
    fOsigma[0]=fOsigma[1]=fOsigma[2]=0;
    fVertex[0]=fVertex[1]=fVertex[2]=0;

    fVMin[0]=fVMin[1]=fVMin[2]=0;
    fVMax[0]=fVMax[1]=fVMax[2]=10000;
  */
}

//_______________________________________________________________________
StarGenerator::StarGenerator(Int_t npart):
  _stack(0),_seed(0)

 /*
  fMCEvGen(0),
  fThetaMin(0),
  fThetaMax(0),
  fPhiMin(0),
  fPhiMax(0),
  fPMin(0),
  fPMax(0),
  fPtMin(0),
  fPtMax(0),
  fYMin(0),
  fYMax(0),
  fVMin(3),
  fVMax(3),
  fNpart(0),
  fParentWeight(0),
  fChildWeight(0),
  fAnalog(0),
  fVertexSmear(kNoSmear),
  fVertexSource(kInternal),
  fCutVertexZ(0),
  fTrackIt(0),
  fOrigin(3),
  fOsigma(3),
  fVertex(3),
  fCollisionGeometry(0)
					  */
{
  //
  // Standard constructor
  //

  /*
    if (gStarce) {
	if (gStarce->GetDebug()>0)
	    printf("\n StarGenerator Constructor initializing number of particles \n\n");
	StarMC * mc = gStarce->GetMCApp();
	if (mc) mc->SetGenerator(this);
    }
    
    SetThetaRange(); ResetBit(kThetaRange);
    SetPhiRange(); ResetBit(kPhiRange);
    SetMomentumRange(); ResetBit(kMomentumRange);
    SetPtRange(); ResetBit(kPtRange);
    SetYRange(); ResetBit(kYRange);
    SetTrackingFlag();
    SetCutVertexZ();

    fOrigin[0]=fOrigin[1]=fOrigin[2]=0;
    fOsigma[0]=fOsigma[1]=fOsigma[2]=0;
    fVertex[0]=fVertex[1]=fVertex[2]=0;

    fVMin[0]=fVMin[1]=fVMin[2]=0;
    fVMax[0]=fVMax[1]=fVMax[2]=10000;

    SetNumberParticles(npart);

    StarConfig::Instance()->Add(this);
  */
}

//_______________________________________________________________________
StarGenerator::StarGenerator(const StarGenerator &gen): 
  _stack(0),_seed(0)

 /*
  TNamed(gen),
  StarRndm(gen),
  fMCEvGen(0),
  fThetaMin(0),
  fThetaMax(0),
  fPhiMin(0),
  fPhiMax(0),
  fPMin(0),
  fPMax(0),
  fPtMin(0),
  fPtMax(0),
  fYMin(0),
  fYMax(0),
  fVMin(3),
  fVMax(3),
  fNpart(0),
  fParentWeight(0),
  fChildWeight(0),
  fAnalog(0),
  fVertexSmear(kNoSmear),
  fVertexSource(kInternal),
  fCutVertexZ(0),
  fTrackIt(0),
  fOrigin(3),
  fOsigma(3),
  fVertex(3),
						       */
{
  //
  // Copy constructor
  //

  /*
  gen.Copy(*this);
  */
}


/*

//_______________________________________________________________________
StarGenerator & StarGenerator::operator=(const StarGenerator &gen)
{
  //
  // Assignment operator
  //
  gen.Copy(*this);
  return (*this);
}

//_______________________________________________________________________
void StarGenerator::Copy(TObject & gen) const
{
  //
  // Copy *this onto gen
  //
  Fatal("Copy","Not implemented!\n");
}

*/

//_______________________________________________________________________
StarGenerator::~StarGenerator()
{
  //
  // Destructor
  //

  /*
  fOrigin.Set(0);
  fOsigma.Set(0);
  if (fMCEvGen) {
    delete fMCEvGen;
    fMCEvGen=0;
  }
  */
}


//_______________________________________________________________________
void StarGenerator::Init()
{   
  //
  // Dummy initialisation
  //
}

/*

//_______________________________________________________________________
void StarGenerator::SetOrigin(Float_t ox, Float_t oy, Float_t oz)
{
  //
  // Set the vertex for the generated tracks
  //
  fOrigin[0]=ox;
  fOrigin[1]=oy;
  fOrigin[2]=oz;
}

//_______________________________________________________________________
void StarGenerator::SetOrigin(const TLorentzVector &o)
{
  //
  // Set the vertex for the generated tracks
  //
  fOrigin[0]=o[0];
  fOrigin[1]=o[1];
  fOrigin[2]=o[2];
}

//_______________________________________________________________________
void StarGenerator::SetSigma(Float_t sx, Float_t sy, Float_t sz)
{
  //
  // Set the spread of the vertex
  //
  fOsigma[0]=sx;
  fOsigma[1]=sy;
  fOsigma[2]=sz;
}

//_______________________________________________________________________
void StarGenerator::SetMomentumRange(Float_t pmin, Float_t pmax)
{
  //
  // Set the momentum range for the generated particles
  //
  fPMin = pmin;
  fPMax = pmax;
  SetBit(kMomentumRange);
}

//_______________________________________________________________________
void StarGenerator::SetPtRange(Float_t ptmin, Float_t ptmax)
{
  //
  // Set the Pt range for the generated particles
  //
  fPtMin = ptmin;
  fPtMax = ptmax;
  SetBit(kPtRange);
}

//_______________________________________________________________________
void StarGenerator::SetPhiRange(Float_t phimin, Float_t phimax)
{
  //
  // Set the Phi range for the generated particles
  //
  fPhiMin = TMath::Pi()*phimin/180;
  fPhiMax = TMath::Pi()*phimax/180;
  SetBit(kPhiRange);
}

//_______________________________________________________________________
void StarGenerator::SetYRange(Float_t ymin, Float_t ymax)
{
  //
  // Set the Rapidity range for the generated particles
  //
  fYMin=ymin;
  fYMax=ymax;
  SetBit(kYRange);
}

//_______________________________________________________________________
void StarGenerator::SetVRange(Float_t vxmin, Float_t vxmax,
			     Float_t vymin, Float_t vymax,
			     Float_t vzmin, Float_t vzmax)
{
  //
  // Set the vertex range for the generated particles
  //
  fVMin[0]=vxmin; fVMin[1]=vymin; fVMin[2]=vzmin;
  fVMax[0]=vxmax; fVMax[1]=vymax; fVMax[2]=vzmax;
  SetBit(kVertexRange);
}

//_______________________________________________________________________
void StarGenerator::SetThetaRange(Float_t thetamin, Float_t thetamax)
{
  //
  // Set the theta range for the generated particles
  //
  fThetaMin = TMath::Pi()*thetamin/180;
  fThetaMax = TMath::Pi()*thetamax/180;
  SetBit(kThetaRange);
}

void StarGenerator::Vertex()
{
  //
  // Obtain vertex for current event from external source or calculated (internal)
  //
    if (fVertexSource == kInternal) {
	VertexInternal();
    } else if (fVertexSource == kContainer) {
	;
    } else if (fVertexSource == kExternal) {
	VertexExternal();
    }
}

//_______________________________________________________________________
void StarGenerator::VertexExternal()
{
    //
    // Obtain vertex from external source (vertex generator)
    //
    TVector3 vertex = fVertexGenerator->GetVertex();
    fVertex[0] = vertex.X();
    fVertex[1] = vertex.Y();
    fVertex[2] = vertex.Z();
}

//_______________________________________________________________________
void StarGenerator::VertexInternal()
{
    // 
    // Obtain calculated vertex 
    // Default is gaussian smearing
    Float_t random[6];
    Float_t dv[3];
    Int_t j;
    dv[2] = 1.e10;
    if (!TestBit(kVertexRange)) {
	while(TMath::Abs(dv[2]) > fCutVertexZ*fOsigma[2]) {
	    Rndm(random,6);
	    for (j=0; j < 3; j++) {
		dv[j] = fOsigma[j]*TMath::Cos(2*random[2*j]*TMath::Pi())*
		    TMath::Sqrt(-2*TMath::Log(random[2*j+1]));
	    }
	}
	for (j=0; j < 3; j++) fVertex[j] = fOrigin[j] + dv[j];
    } else {
	Rndm(random,3);
	for (j=0; j < 3; j++)
	    fVertex[j] =  fVMin[j] + random[j] * (fVMax[j] - fVMin[j]);
    }
}

//_______________________________________________________________________
void  StarGenerator::PushTrack(Int_t done, Int_t parent, Int_t pdg,
                               Float_t *pmom, Float_t *vpos, Float_t *polar,
                               Float_t tof, TMCProcess mech, Int_t &ntr,
                               Float_t weight, Int_t is)
{
  //
  // Loads one track on the stack
  //

  if (_stack)
    _stack->PushTrack(done, parent, pdg, pmom, vpos, polar, tof,
                     mech, ntr, weight, is);
  else 
    gStarce->GetMCApp()->PushTrack(done, parent, pdg, pmom, vpos, polar, tof,
                     mech, ntr, weight, is);
}

//_______________________________________________________________________
void  StarGenerator::PushTrack(Int_t done, Int_t parent, Int_t pdg,
                      Double_t px, Double_t py, Double_t pz, Double_t e,
                      Double_t vx, Double_t vy, Double_t vz, Double_t tof,
                      Double_t polx, Double_t poly, Double_t polz,
                      TMCProcess mech, Int_t &ntr, Float_t weight, Int_t is)
{
  //
  // Loads one track on the stack
  //
  
  if (_stack)
     _stack->PushTrack(done, parent, pdg, px, py, pz, e, vx, vy, vz, tof,
                      polx, poly, polz, mech, ntr, weight, is);
  else 
     gStarce->GetMCApp()->PushTrack(done, parent, pdg, px, py, pz, e, vx, vy, vz, tof,
                        polx, poly, polz, mech, ntr, weight, is);
}


//_______________________________________________________________________
void StarGenerator:: KeepTrack(Int_t itrack)
{
  //
  // Declare a track permanent on the stack
  //
  if (_stack)
     _stack->KeepTrack(itrack);
  else 
     gStarce->GetMCApp()->KeepTrack(itrack);
   
}

//_______________________________________________________________________
void StarGenerator:: SetHighWaterMark(Int_t nt)
{
  //
  // Internal function to set the maximum index used in the stack
  //
  if (_stack)
     _stack->SetHighWaterMark(nt);
  else 
     gStarce->GetMCApp()->SetHighWaterMark(nt);
   
}
void StarGenerator::FinishRun()
{
    ;
}
*/
