/**********************************************************************
* StEmcEffPedSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/

#ifndef STAR_StEmcEffPedSpectra
#define STAR_StEmcEffPedSpectra
#include "StEmcSpectra.h"
#include "TArrayI.h"
#include "TArrayF.h"
#include "TMatrix.h"

class StEmcEffPedSpectra : public StEmcSpectra 
{
  private:
                                                               
  protected:   
  public: 
           
                   StEmcEffPedSpectra(const char*);  
  virtual          ~StEmcEffPedSpectra();
  Bool_t           CalcPedestal(Int_t);
           

  ClassDef(StEmcEffPedSpectra,1)
};
#endif
