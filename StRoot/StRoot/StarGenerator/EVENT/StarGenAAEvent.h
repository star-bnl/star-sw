#ifndef __StarGenAAEvent_h__
#define __StarGenAAEvent_h__

#include "StarGenEvent.h"

/**
\class StarGenAAEvent
\author Jason C. Webb
\brief Event record tailored to heavy ion collisions

Based on g2t_event.

*/

class StarGenAAEvent : public StarGenEvent 
{
 public:

  StarGenAAEvent( const Char_t *name="AAevent", const Char_t *title="nucleus+nucleus event");
 ~StarGenAAEvent(){ /* nada */ };

  Int_t idBlue;       ///< Blue beam ID
  Int_t idYell;       ///< Yellow beam ID
  Int_t process;      ///< Process ID
  Int_t subprocess;   ///< Subprocess ID

  Int_t idParton1;    ///< ID of blue beam parton of the hard interaction
  Int_t idParton2;    ///< ID of yellow beam parton of the hard interaction
  Double_t xParton1;  ///< x of blue beam parton of the hard interaction 
  Double_t xParton2;  ///< x of yellow beam parton of the hard interaction
  Double_t xPdf1;     ///< PDF times x for blue beam parton of the hard interaction
  Double_t xPdf2;     ///< PDF times x for yellow beam parton of the hard interaction
  Double_t Q2fac;     ///< Factorization scale of the hard interaction
  Double_t Q2ren;     ///< Renormalization scale of the hard interaction
  Bool_t   valence1;  ///< True if blue beam parton is in valence  of the hard interaction
  Bool_t   valence2;  ///< True if yellow beam parton is in valence of the hard interaction
  
  Double_t sHat,tHat,uHat; ///< Mandelstam variables
  
  Double_t ptHat;
  Double_t thetaHat;
  Double_t phiHat;

  Double_t impactParameter;             ///< Impact parameter [fm]
  Double_t reactionPlane;               ///< Phi of the reaction plane 0 to 2pi [radians]

  Int_t numberOfBinary;                 ///< Number of binary collisions
  Int_t numberOfParticipantNeutrons[2]; ///< 0=Blue 1=Yellow
  Int_t numberOfParticipantProtons[2];  ///< 0=Blue 1=Yellow
  Int_t numberRejected;                 ///< ?
  Int_t numberWounded[2];               ///< Number of wounded nucleons 0=Blue 1=Yellow
  Int_t numberOfJets;                   ///< Number of jets

  Double_t weight;

  ClassDef(StarGenAAEvent,1);

};
 
#endif
