/**********************************************************************
* StEmcEffPedSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/
/*!\class StEmcEffPedSpectra
\author Alexandre A. P. Suaide

This class makes EMC effective pedestal calculation. It is derived from StEmcSpectra.
*/

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
           
                   StEmcEffPedSpectra(const char*); ///< Default constructor
  virtual          ~StEmcEffPedSpectra();///< Default destructor
  Bool_t           CalcPedestal(Int_t);///< Calculates effective pedestal
           

  ClassDef(StEmcEffPedSpectra,1)
};
#endif
