/**********************************************************************
* StEmcEqualSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/

#ifndef STAR_StEmcEqualSpectra
#define STAR_StEmcEqualSpectra
#include "StEmcSpectra.h"
#include "TArrayI.h"
#include "TArrayF.h"
#include "TMatrix.h"

class StEmcEqualSpectra : public StEmcSpectra 
{
  private:
                                                               
  protected:   
  public: 
           
                   StEmcEqualSpectra(const char*);  
  virtual          ~StEmcEqualSpectra();
           void    DrawEqualConst();
           Bool_t  Equalize(Int_t,Int_t,Int_t);
           

  ClassDef(StEmcEqualSpectra,1)
};
#endif
