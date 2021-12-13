/*!
 * \class StGimParticle 
 * \author V.Perev  Mar/2009
 *
 * Defines  equivalent of HEPEVT particle
 * 
 */
// @(#)STAR/eg:$Id: StGimParticle.h,v 1.3 2009/06/23 19:52:05 jeromel Exp $

#ifndef STAR_StGimParticle
#define STAR_StGimParticle
#include <vector>
#include "StGenParticle.h"

/// Implementation of StGenParticle similar to ROOT TParticle 
class StGimParticle : public StGenParticle {
public:


                                // ****** constructors and destructor
   StGimParticle(int idx);

   virtual ~StGimParticle();

virtual int          	     GetStatusCode()     const 	{return fStatusCode;}
virtual int          	     GetPdgCode()        const 	{return fPdgCode   ;}
virtual int          	     GetGeaCode()        const 	{return fGeaCode   ;}
virtual const StGenParticle *GetMother(int i=0)  const 	{return fMother[i] ;} 
virtual const StGenParticle *GetDaughter(int i)  const; 
virtual       double         GetMass         ()  const 	{return fP[4];}
virtual       int            GetNDaughters   ()  const 	{return fDaughter.size();}
virtual       void           Momentum(double p4[4]) const ;
virtual       void           Vertex(double v[3]) const;
virtual       double         Time() 		 const 	{return fV[3];}

// 	setters
  void Clear();
  void SetPdg(int pdg) 					{fPdgCode=pdg;}
  void SetGea(int gea) 					{fGeaCode=gea;}
  void SetVert(float v[4]);	
  void SetMom (float p[5]);	
  void SetMother(int i,const StGimParticle *m) {fMother[i]=m;} 
  void AddDaughter(const StGimParticle *m); 


protected:

  char fBeg[1];
  int          fPdgCode;              // PDG   code of the particle
  int          fGeaCode;              // Geant code of the particle
  int          fStatusCode;           // generation status code
  const StGimParticle *fMother[2];    // mother particles
  double       fWeight;               // particle weight
  double       fP[5];                 // 4vector  of momentum + mass
  double       fV[4];                 // 3vector of production vertex +time in cm
  char fEnd[1];
  std::vector<const StGimParticle*>     fDaughter;        

};

#endif

