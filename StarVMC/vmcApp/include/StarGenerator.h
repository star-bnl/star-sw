#ifndef STARGENERATOR_H
#define STARGENERATOR_H

/* $Id: StarGenerator.h,v 1.2 2004/07/16 22:52:53 potekhin Exp $ */

///////////////////////////////////////////////////////////
//                                                       //
//  Class to generate the particles for the MC           //
//                                                       //
///////////////////////////////////////////////////////////

#include <TArrayF.h>
#include <TLorentzVector.h>
#include <TMCProcess.h>
#include <TPDGCode.h>

// #include "StarRndm.h"

//class StarVertexGenerator;
//class StarCollisionGeometry;

#include <StarStack.h>


class TGenerator;


typedef enum { kNoSmear, kPerEvent, kPerTrack }  VertexSmear_t;
typedef enum { kExternal, kInternal, kContainer} VertexSource_t;


class StarGenerator : public TNamed  // , public StarRndm
{

 public:
    StarGenerator();
    StarGenerator(Int_t npart);
    StarGenerator(const StarGenerator &gen);
    virtual ~StarGenerator();

    virtual void Generate()=0;

    // Stack 
    void SetStack (StarStack *stack_) {_stack = stack_;}
    StarStack* GetStack(){return _stack;}

    void SetSeed (int s_) {_seed = s_;}

    virtual void Init(void);

    /*
    void Copy(TObject &gen) const;
    virtual void SetOrigin(Float_t ox, Float_t oy, Float_t oz);
    virtual void SetOrigin(const TLorentzVector &o);
    virtual void SetSigma(Float_t sx, Float_t sy, Float_t sz);
    virtual void SetMomentumRange(Float_t pmin=0, Float_t pmax=1.e10);
    virtual void SetPtRange(Float_t ptmin=0, Float_t ptmax=1.e10);
    virtual void SetPhiRange(Float_t phimin = 0., Float_t phimax = 360.);
    virtual void SetYRange(Float_t ymin=-100, Float_t ymax=100);
    virtual void SetVRange(Float_t vxmin, Float_t vxmax,
			   Float_t vymin, Float_t vymax,
			   Float_t vzmin, Float_t vzmax);
    virtual void SetNumberParticles(Int_t npart=100) {fNpart=npart;}
    virtual Int_t NumberParticles() const {return fNpart;}
    virtual void SetThetaRange(Float_t thetamin=0, Float_t thetamax=180);
    virtual void Generate()=0;
    virtual void SetParentWeight(Float_t wgt) {fParentWeight=wgt;}
    virtual void SetChildWeight(Float_t wgt)  {fChildWeight=wgt;}    
    virtual void SetAnalog(Int_t flag=1) {fAnalog=flag;}	
    virtual void SetVertexSmear(VertexSmear_t smear) {fVertexSmear = smear;}
    virtual void SetCutVertexZ(Float_t cut=999999.) {fCutVertexZ = cut;}
    virtual void SetVertexSource(VertexSource_t source = kInternal) {fVertexSource = source;}
    virtual void SetVertex(Float_t vx, Float_t vy, Float_t vz)
	{fVertex[0] = vx; fVertex[1] = vy; fVertex[2] = vz;}
    void SetVertexGenerator(StarVertexGenerator* vertexGenerator)
      {fVertexGenerator = vertexGenerator; fVertexSource = kExternal; fVertexSmear = kPerEvent;}
    virtual void SetTrackingFlag(Int_t flag=1) {fTrackIt=flag;}
    void Vertex();
    void VertexExternal();
    virtual void VertexInternal();
    virtual void FinishRun();
    virtual void SetMC(TGenerator *theMC) {fMCEvGen =theMC;}
    StarGenerator & operator=(const StarGenerator &gen);

  // Getters

    virtual void GetOrigin(Float_t &ox, Float_t &oy, Float_t &oz) const
	{ox=fOrigin.At(0);oy=fOrigin.At(1);oz=fOrigin.At(2);}
    virtual void GetOrigin(TLorentzVector &o) const
	{o[0]=fOrigin.At(0);o[1]=fOrigin.At(1);o[2]=fOrigin.At(2);o[3]=0;}
  // Collision Geometry
    virtual Bool_t ProvidesCollisionGeometry() const {return kFALSE;}
    virtual Bool_t NeedsCollisionGeometry()    const {return kFALSE;}    
    virtual StarCollisionGeometry* CollisionGeometry() const {return fCollisionGeometry;}
    virtual void SetCollisionGeometry(StarCollisionGeometry* geom) {fCollisionGeometry = geom;}
 protected:
    virtual  void  PushTrack(Int_t done, Int_t parent, Int_t pdg,
                               Float_t *pmom, Float_t *vpos, Float_t *polar,
                               Float_t tof, TMCProcess mech, Int_t &ntr,
                               Float_t weight = 1, Int_t is = 0);
    virtual  void  PushTrack(Int_t done, Int_t parent, Int_t pdg,
                      Double_t px, Double_t py, Double_t pz, Double_t e,
                      Double_t vx, Double_t vy, Double_t vz, Double_t tof,
                      Double_t polx, Double_t poly, Double_t polz,
                      TMCProcess mech, Int_t &ntr, Float_t weight = 1, Int_t is = 0);
    virtual void   KeepTrack(Int_t itrack); 
    virtual void   SetHighWaterMark(Int_t nt);
    */

 protected:
    StarStack*   _stack;      // stack handle
    int          _seed;       // seed

    /*    TGenerator* fMCEvGen;      //!Pointer to the generator
    Float_t     fThetaMin;     //Minimum theta of generation in radians
    Float_t     fThetaMax;     //Maximum theta of generation in radians
    Float_t     fPhiMin;       //Minimum phi of generation in radians
    Float_t     fPhiMax;       //Maximum phi of generation in radians
    Float_t     fPMin;         //Minimum momentum of generation in GeV/c
    Float_t     fPMax;         //Minimum momentum of generation in GeV/c
    Float_t     fPtMin;        //Minimum transverse momentum
    Float_t     fPtMax;        //Maximum transverse momentum
    Float_t     fYMin;         //Minimum rapidity
    Float_t     fYMax;         //Maximum rapidity
    TArrayF     fVMin;         //Minimum Decaylength
    TArrayF     fVMax;         //Minimum Decaylength    
    Int_t       fNpart;        //Maximum number of particles per event
    Float_t     fParentWeight; //Parent Weight
    Float_t     fChildWeight;  //ChildWeight
    Int_t       fAnalog;       //Flag for anolog or pt-weighted generation
   //
    VertexSmear_t     fVertexSmear;  //Vertex Smearing mode
    VertexSource_t    fVertexSource; //Vertex source (internal/external)
    Float_t     fCutVertexZ;    // Vertex cut in units of sigma_z
    Int_t       fTrackIt;    // if 1, Track final state particles 
    StarVertexGenerator* fVertexGenerator;  //! Generator for the vertex
    TArrayF     fOrigin;     // Origin of event
    TArrayF     fOsigma;     // Sigma of the Origin of event
    TArrayF     fVertex;     //! Vertex of current event

    StarCollisionGeometry* fCollisionGeometry; //!Collision geometry

    enum {kThetaRange    = BIT(14),
	  kVertexRange   = BIT(15),
	  kPhiRange      = BIT(16),
	  kPtRange       = BIT(17),
	  kYRange        = BIT(18),
	  kMomentumRange = BIT(19)     
    };

*/
    ClassDef(StarGenerator,1) // Base class for event generators
};

#endif
