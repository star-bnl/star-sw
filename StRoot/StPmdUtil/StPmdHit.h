/*!
 *\class StPmdHit
 *\author
*/
/*************************************************
 * $Id: StPmdHit.h,v 1.2 2002/09/09 11:27:59 subhasis Exp $
 * 
 * Author: Subhasis Chattopadhyay
 * Revision: Prem Ghosh
 * 5/9/02: ADC functionality added.
 * 
 *************************************************
 *
 * Description: This is the class for PMD hit objects
 *
 **************************************************
 * $Log: StPmdHit.h,v $
 * Revision 1.2  2002/09/09 11:27:59  subhasis
 * ADC added
 *
 **************************************************/
#ifndef STAR_StPmdHit
#define STAR_StPmdHit

#include <math.h>
#include <iostream.h>
#include "TArrayI.h"
#include "StObject.h"

class StPmdHit : public StObject {
  //! hit objects
private:
  Int_t             mGsuper;   //! global supermodule no.
  Int_t             mSubdet;   //! detector (PMD/CPV)
  Int_t             mRow;      //! row no. in the supermodule 
  Int_t             mCol;      //! col no. in the supermodule
  Float_t           mEdep;     //! energy deposition
  Int_t           mAdc;     //! adc
public: 
  StPmdHit();                      //! A constructor
  StPmdHit(TArrayI*);              //! A constructor
  ~StPmdHit();                     //! A destructor

  /*! member functions */
  Int_t           Gsuper() const;  //! function for supermodule no.
  Int_t           module() ;       //! function for module
  Int_t           SubDetector() const; //! function for subdetector
  Int_t           Row() const;         //! function for row
  Int_t           Column() const;      //! function for col
  Float_t           Edep() const;      //! function for edep
  Int_t           Adc() const;      //! function for adc
  void           setGsuper(Int_t);
  void            setSubDetector(Int_t);
  void            setRow(Int_t);
  void            setColumn(Int_t);
  void            setEdep(Float_t);
  void            setAdc(Int_t);
  virtual void      print(ostream *os);
  virtual void      Browse(TBrowser *b);
  ClassDef(StPmdHit,1)
};
/*! Inline functions for hit objects */

inline              StPmdHit::~StPmdHit(){ /* Nobody */ }
inline   Int_t    StPmdHit::Gsuper() const {return mGsuper;}
inline   Int_t    StPmdHit::module()  {if(mGsuper<=12){return mGsuper;}
                                     else {return -1;}}
inline   Int_t    StPmdHit::SubDetector() const     {return mSubdet;} 
inline   Int_t    StPmdHit::Row() const     {return mRow;} 
inline   Int_t    StPmdHit::Column() const     {return mCol;} 
inline   Float_t    StPmdHit::Edep() const  {return mEdep;}
inline   Int_t    StPmdHit::Adc() const  {return mAdc;}
inline void  StPmdHit::setGsuper(Int_t var) {mGsuper = var;}
inline void  StPmdHit::setSubDetector(Int_t var)  {mSubdet = var;}
inline void  StPmdHit::setRow(Int_t var)  {mRow = var;}
inline void  StPmdHit::setColumn(Int_t var)  {mCol = var;}
inline void  StPmdHit::setEdep(Float_t var)  {mEdep = var;}
inline void  StPmdHit::setAdc(Int_t var)  {mAdc = var;}

#endif



