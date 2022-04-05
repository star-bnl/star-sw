/*!
 *\class StPmdGeom
 *\author
 */
/*********************************************************
 *
 * $Id: StPmdGeom.h,v 1.13 2010/04/15 06:55:44 rashmi Exp $
 *
 * Author: Dipak Mishra
 *
 ************************************************************
 *
 * Description: This is the class of PMD geometry for offline 
 * and mapping of all the electronic chains
 *
 *************************************************************
 *
 * $Log: StPmdGeom.h,v $
 * Revision 1.13  2010/04/15 06:55:44  rashmi
 * functions to draw XY and eta/phi coverage & modifcations to mapping
 *
 * Revision 1.12  2007/11/02 11:04:32  rashmi
 * public GetPmdZ added to get PMD z position
 *
 * Revision 1.11  2007/04/17 11:19:53  rashmi
 * Chain19 mapping corrected, functions to return nboards in a chain/SMs added
 *
 * Revision 1.10  2007/03/21 16:40:10  rashmi
 * StPmdGeom after new mapping (run7) and with DrawPmd function for viewing PMD Geometry
 *
 * Revision 1.9  2005/01/04 19:31:08  subhasis
 * Mapping for year2005 Run, Code from Rashmi's area
 *
 * Revision 1.8  2004/04/01 15:40:25  subhasis
 * zdist changed after survey result
 *
 * Revision 1.7  2004/03/23 08:49:15  subhasis
 * biardDetail put by had
 *
 * Revision 1.6 
 * Mapping modified according to final mounting of FEE: Dipak
 *
 * Revision 1.5  2003/11/27 12:31:52  subhasis
 * ADC2EDEP added by Supriya
 *
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
  static Int_t jmirr[192] ;
  Int_t m_RunNo;  
//  FILE *infile;
//  FILE *fp1;
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
  void readBoardDetail();      // function for defining constant values
  void readBoardDetail(Int_t runno1);      // function for defining constant values
  void GetRunYear(Int_t, Int_t&,Int_t&);
  Int_t GetNBoardsChain(Int_t);
  void GetNBoardsSM(Int_t,Int_t*);

  
  void drawPMD(Int_t,Int_t,Int_t);
  void DrawRhombus(Int_t,Int_t,Int_t,Int_t,Int_t,Int_t&,Float_t*,Float_t*);
  void drawPMDXY(Int_t,Int_t,Int_t);
  void drawPMDetaphi(Int_t,Int_t,Int_t);

  Int_t ChainMapping(Int_t&,Int_t&,Int_t&,Int_t&,Int_t&,Int_t&);
  Int_t ChainMapping(Int_t&,Int_t&,Int_t&,Int_t&,Int_t&,Int_t&,Int_t);

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
  void chain39(int&,int&,int&,int&);
  void chain41(int&,int&,int&,int&);
  void chain45(int&,int&,int&,int&);
  void chain46(int&,int&,int&,int&);
// year=2005 2005 , Rashmi's routine  
  void chain1(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain2(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain3(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain4(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain5(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain6(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain7(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain8(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain9(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain10(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain11(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain12(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain13(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain14(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain15(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain16(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain17(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain18(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain19(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain20(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain21(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain22(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain23(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain24(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain25(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain26(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain27(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain28(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain29(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain30(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain31(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain32(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain33(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain34(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain35(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain36(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain37(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain38(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain39(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain40(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain41(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain42(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain43(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain44(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain45(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain46(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain47(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  void chain48(Int_t&,Int_t&,Int_t&,Int_t&,Int_t);
  
  void ADC2Edep(Int_t, Float_t&); //! To convert the ADC value to Edep
  Float_t GetPmdZ();
  
  ClassDef(StPmdGeom, 1)
    };
    //! for defining constants 
    //inline void StPmdGeom::SetRunNumber(Int_t var){m_RunNo =var ;}
    
    inline Float_t StPmdGeom::GetPmdZ(){
      return mzreal;
    }

inline void StPmdGeom::commonconstants()
{
  mconst1=0.5282;
  mconst2=1.732*0.5282;  
  mcell_rad = 0.5282;
  mcelldia_x = 1.0564;
  mcelldia_y = 0.91484;   
  // changed to 535mzreal = 550.0;
  //  mzreal = 542.0;
  mzreal = 539.0;  // changed on 1/4/04 after survey results
}
#endif



























