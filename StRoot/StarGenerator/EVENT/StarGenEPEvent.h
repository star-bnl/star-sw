#ifndef __StarGenEPEvent_h__
#define __StarGenEPEvent_h__

#include "StarGenEvent.h"

/**
   \class StarGenEPEvent
   \author Jason C. Webb
   \brief Event record class tailored to DIS kinemaics


*/

class StarGenEPEvent : public StarGenEvent 
{
 public:

  StarGenEPEvent( const Char_t *name="epevent", const Char_t *title="electron+proton event");
 ~StarGenEPEvent(){ /* nada */ };

  Int_t idBlue;       ///< Blue beam ID
  Int_t idYell;       ///< Yellow beam ID
  Int_t process;      ///< Process ID
  Int_t subprocess;   ///< Subprocess ID

  Int_t idParton;     ///< ID of the struck parton

  Double_t xParton;   ///< x of blue struck parton
  Double_t xPdf;      ///< PDF times x for the struck parton
  Double_t Q2;        ///< Virtuality of the photon

  Bool_t   valence;   ///< true if valence quark

  Double_t y;         ///< rapidity
  Double_t W2;
  Double_t nu;
  
  Double_t weight;

  ClassDef(StarGenEPEvent,1);

};
 
#endif
