/**********************************************************************
* StEmcEqualSpectra
* Author: Alexandre A. P. Suaide 
*
* This is responsible for emc equalization
***********************************************************************/

/*!\class StEmcEqualSpectra
\author Alexandre A. P. Suaide

This class makes EMC equalization. It is derived from StEmcSpectra.
*/

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
           
                   StEmcEqualSpectra(const char*); ///< Default constructor 
  virtual          ~StEmcEqualSpectra(); ///< Default destructor
           void    DrawEqualConst(); ///< Draw Equalization constants
           Bool_t  Equalize(Int_t,Int_t,Int_t); ///< Equalize two bins
           

  ClassDef(StEmcEqualSpectra,1)
};
#endif
