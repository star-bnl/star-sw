//////////////////////////////////////////////////////////////////////
//
// Revision 1.0  2000/12/11 Pablo Yepes
// First Version of StPeCL0
//
//////////////////////////////////////////////////////////////////////
//
// StPeCL0
//
// Event class for Peripheral Collisions
//
//////////////////////////////////////////////////////////////////////
#ifndef StPeCL0_h
#define StPeCL0_h
#include "Rtypes.h"
#include "TObject.h"
#ifndef __CINT__
#include "PhysicalConstants.h"
#include "StEventTypes.h"
#endif /* __CINT__ */

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

const int nL0Trays = 120 ;
const int nL0Slats  = 2 ;
const int nL0Depth3 =  2 ;
const int nL0Phi    = 4 ;
const int nL0Eta    = 4 ;
const int nL0Range  = 256 ;

class StPeCL0: public TObject {

public:

  StPeCL0();
  ~StPeCL0();

  void setLinearLuts ( ) ;
  void setP4Luts ( ) ;
  void setP4SLuts ( ) ;
  void setP4PLuts ( ) ;
  void setCountingLuts ( ) ;
  void setYear1Input ( ) ;
  void setYear2Input ( ) ;

#ifndef __CINT__
  Int_t process ( StEvent *event ) ; 
  Int_t process(StMuDst* mudst); 
#endif /* __CINT__ */
  Int_t dsm1Sum ( ) ;
  Int_t dsm2Sum ( ) ;
  void  printSlats ( ) ;
  void  printWeightedSlats ( ) ;
  void  printPatches ( ) ;
  void  printWeightedPatches ( ) ;
  void  setInfoLevel ( int _in ) { infoLevel = _in ; } ;
  
  Int_t    infoLevel ;
  Int_t    minAdc ;
  Int_t    maxAdc ;

  Byte_t   lut1[nL0Trays][nL0Slats][nL0Range]; // Lookup table values for input
  Byte_t   lut2[nL0Phi][nL0Eta][nL0Range];     // Lookup table values for depth 2
  Byte_t   lut3[2][nL0Range];                  // Lookup table values for depth 3


  Int_t    cabling[nL0Trays] ; // relation bewtween CTB trays and 
  Byte_t   array1[nL0Trays][nL0Slats]  ; // values at input  
  Byte_t   array2[nL0Phi][nL0Eta]      ; // values at depth 2
  Byte_t   array3[nL0Depth3]           ; // values at depth 3
  Byte_t   weighted1[nL0Trays][nL0Slats]  ; // values at input after lookup table 
  Byte_t   weighted2[nL0Phi][nL0Eta]      ; // values at depth 2 after lookup table
  Byte_t   weighted3[nL0Depth3]           ; // values at depth 3 after lookup table

  Float_t threshold ;

  ClassDef(StPeCL0,1)
};

#endif





