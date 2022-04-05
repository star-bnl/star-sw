#ifndef __StarGenPPEvent_h__
#define __StarGenPPEvent_h__

#include "StarGenEvent.h"

/**
\class StarGenPPEvent
\brief Event record class tailored to PP kinematics 

\author Jason C. Webb

*/

class StarGenPPEvent : public StarGenEvent 
{
 public:

  StarGenPPEvent( const Char_t *name="ppevent", const Char_t *title="proton+proton event");
 ~StarGenPPEvent(){ /* nada */ };

 // Default copy ctor, assignment operator

  Int_t idBlue;       ///< Blue beam ID
  Int_t idYell;       ///< Yellow beam ID
  Int_t process;      ///< Process ID
  Int_t subprocess;   ///< Subprocess ID

  Int_t idParton1;    ///< ID of blue beam parton
  Int_t idParton2;    ///< ID of yellow beam parton
  Double_t xParton1;  ///< x of blue beam parton
  Double_t xParton2;  ///< x of yellow beam parton
  Double_t xPdf1;     ///< PDF times x for blue beam parton
  Double_t xPdf2;     ///< PDF times x for yellow beam parton
  Double_t Q2fac;     ///< Factorization scale
  Double_t Q2ren;     ///< Renormalization scale
  Bool_t   valence1;  ///< True if blue beam parton is in valence
  Bool_t   valence2;  ///< True if yellow beam parton is in valence
  
  Double_t sHat,tHat,uHat; ///< Mandelstam variables
  
  Double_t ptHat;     ///< pT of the recoiling particles
  Double_t thetaHat;  ///< theta of the recoiling particles 
  Double_t phiHat;    ///< phi of the recoiling particles

  Double_t weight;    ///< Weight of the event 

  ClassDef(StarGenPPEvent,1);

};
 
#endif
