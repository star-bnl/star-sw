/*!
 *\class StPmdGeom
 *\author
 */
/*********************************************************
 *
 * $Id: StPmdGeom.h,v 1.1 2002/08/27 12:14:51 subhasis Exp $
 *
 * Author: Dipak Mishra
 *
 ************************************************************
 *
 * Description: This is the class of PMD geometry for offline 
 *
 *************************************************************
 *
 * $Log: StPmdGeom.h,v $
 * Revision 1.1  2002/08/27 12:14:51  subhasis
 * First version
 *
 *************************************************************/
#ifndef STAR_StPmdGeom
#define STAR_StPmdGeom
#include <stdlib.h>
#include <TMatrix.h>
#include <iostream.h>
#include <math.h>

class StPmdGeom {  
 private:

  static Float_t mxcon[17];    //! 'X' position of the supermodule by GEANT
  static Float_t mycon[17];    //! 'Y' position of the supermodule by GEANT
  static Float_t mdetxcon[12]; //! 'X' position of the supermodule after conversion 
  static Float_t mdetycon[12]; //! 'Y' position of the supermodule after conversion
  Float_t mconst1;             //! constant used for calculation
  Float_t mconst2;             //! constant used for calculation
  Float_t mcell_rad;           //! cell radius
  Float_t mcelldia_x;          //! cell diameter
  Float_t mcelldia_y;          //! distance of the centers between two cells along 'Y' direction
  Float_t mzreal;              //! distance of PMD from the vertex
  void commonconstants();      //! function for defining constant values
 public: 
  StPmdGeom();                 //! A constructor
  virtual  ~StPmdGeom();       //! A destructor

  Int_t    NModule( Int_t,  Int_t, Int_t& ); 
  void Cell_xy(Int_t,Int_t,Int_t,Float_t&,Float_t&,Float_t&,Float_t&) ; 
  void DetCell_xy(Int_t,Float_t,Float_t,Float_t&,Float_t&,Float_t&,Float_t&);
  void Sim2Detmap(Int_t&,Int_t&,Int_t&);
  void Cell_eta_phi(Float_t,Float_t,Float_t&,Float_t&);
  ClassDef(StPmdGeom, 1)
};

inline void StPmdGeom::commonconstants()
{
  mconst1=0.5282;
  mconst2=1.732*0.5282;  
  mcell_rad = 0.5282;
  mcelldia_x = 1.0564;
  mcelldia_y = 0.91484;   
  mzreal = 550.0;
}
#endif













