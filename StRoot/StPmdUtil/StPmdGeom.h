/*!
 *\class StPmdGeom
 *\author
 */
/*********************************************************
 *
 * $Id: StPmdGeom.h,v 1.4 2003/10/14 07:30:19 subhasis Exp $
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
 * Revision 1.4  2003/10/14 07:30:19  subhasis
 * mzreal changed to 535
 *
 * Revision 1.3  2003/09/02 17:58:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/05/12 12:07:13  subhasis
 * Mapping added
 *
 *************************************************************/
#ifndef STAR_StPmdGeom
#define STAR_StPmdGeom
#include <stdlib.h>
#include <TMatrix.h>
#include <Stiostream.h>
#include <math.h>

class StPmdGeom {  
 private:
  
  static Float_t mxcon[17];    //! 'X' position of the supermodule by GEANT
  static Float_t mycon[17];    //! 'Y' position of the supermodule by GEANT
  static Float_t mdetxcon[12]; //! 'X' position of the supermodule after conversion 
  static Float_t mdetycon[12]; //! 'Y' position of the supermodule after conversion
  static Int_t inorm[192];
  static Int_t jnorm[192];
  static Int_t imirr[192];
  static Int_t jmirr[192];
  

  Float_t mconst1;             //! constant used for calculation
  Float_t mconst2;             //! constant used for calculation
  Float_t mcell_rad;           //! cell radius
  Float_t mcelldia_x;          //! cell diameter
  Float_t mcelldia_y;          //! distance of the centers between two cells along 'Y' direction
  Float_t mzreal;              //! distance of PMD from the vertex
  void commonconstants();      // function for defining constant values
 public: 
  StPmdGeom();                 //! A constructor
  virtual  ~StPmdGeom();       //! A destructor

  Int_t    NModule( Int_t,  Int_t, Int_t& ); 
  void Cell_xy(Int_t,Int_t,Int_t,Float_t&,Float_t&,Float_t&,Float_t&) ; 
  void DetCell_xy(Int_t,Float_t,Float_t,Float_t&,Float_t&,Float_t&,Float_t&);
  void IntDetCell_xy(Int_t,Int_t,Int_t,Float_t&,Float_t&,Float_t&,Float_t&);
  void Sim2Detmap(Int_t&,Int_t&,Int_t&);
  void Cell_eta_phi(Float_t,Float_t,Float_t&,Float_t&);

  void ChainMapping(Int_t&,Int_t&,Int_t&,Int_t&,Int_t&);
  void chain1(int&,int&,int&,int&);
  void chain2(int&,int&,int&,int&);
  void chain3(int&,int&,int&,int&);
  void chain5(int&,int&,int&,int&);
  void chain9(int&,int&,int&,int&);
  void chain10(int&,int&,int&,int&);
  void chain12(int&,int&,int&,int&);
  void chain15(int&,int&,int&,int&);
  void chain17(int&,int&,int&,int&);
  void chain21(int&,int&,int&,int&);
  void chain22(int&,int&,int&,int&);
  void chain23(int&,int&,int&,int&);
  void chain34(int&,int&,int&,int&);
  void chain45(int&,int&,int&,int&);
  
  ClassDef(StPmdGeom, 1)
};
//! for defining constants 
inline void StPmdGeom::commonconstants()
{
  mconst1=0.5282;
  mconst2=1.732*0.5282;  
  mcell_rad = 0.5282;
  mcelldia_x = 1.0564;
  mcelldia_y = 0.91484;   
  // changed to 535mzreal = 550.0;
  mzreal = 535.0;
}
#endif













