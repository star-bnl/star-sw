/*!
 *\class StPhmdHit
 *\author
*/
/********************************************************************
 *
 * $Id: StPhmdHit.h,v 2.4 2003/09/02 17:58:05 perev Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: This is the class for PMD hit objects
 *
 ********************************************************************
 *
 * $Log: StPhmdHit.h,v $
 * Revision 2.4  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.3  2003/04/22 00:08:14  ullrich
 * Removed //! comments
 *
 * Revision 2.2  2002/12/21 00:32:33  ullrich
 * Corrected typo in module().
 *
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#ifndef StPhmdHit_hh
#define StPhmdHit_hh

#include <math.h>
#include <Stiostream.h>
#include "StObject.h"

class StPhmdHit : public StObject {
public: 
    StPhmdHit();                     
    ~StPhmdHit();                     

    int             superModule() const;  // function for supermodule no.
    int             module() ;            // function for module
    int             subDetector() const;  // function for subdetector
    int             row() const;          // function for row
    int             column() const;       // function for col
    float           energy() const;       // function for edep
    int             adc() const;          // function for adc
    
    void            setSuperModule(int);
    void            setSubDetector(int);
    void            setRow(int);
    void            setColumn(int);
    void            setEnergy(float);
    void            setAdc(int);
    
private:
    Int_t           mSuperModuleNumber;   // global supermodule no.
    Int_t           mSubDetector;         // detector (PMD/CPV)
    Int_t           mRow;                 // row no. in the supermodule 
    Int_t           mCol;                 // col no. in the supermodule
    Float_t         mEnergy;              // energy deposition
    Int_t           mAdc;                 // adc
    ClassDef(StPhmdHit,1)
};

inline   int    StPhmdHit::superModule() const {return mSuperModuleNumber;}
inline   int    StPhmdHit::module()
{
    // return module in range 0-11
    if (mSuperModuleNumber < 12) 
	return mSuperModuleNumber;
    else
	return -1;
}
inline int    StPhmdHit::subDetector() const {return mSubDetector;} 
inline int    StPhmdHit::row() const         {return mRow;} 
inline int    StPhmdHit::column() const      {return mCol;} 
inline float  StPhmdHit::energy() const      {return mEnergy;}
inline int    StPhmdHit::adc() const         {return mAdc;}
inline void   StPhmdHit::setSuperModule(int var) {mSuperModuleNumber = var;}
inline void   StPhmdHit::setSubDetector(int var) {mSubDetector = var;}
inline void   StPhmdHit::setRow(int var)     {mRow = var;}
inline void   StPhmdHit::setColumn(int var)  {mCol = var;}
inline void   StPhmdHit::setEnergy(float var){mEnergy = var;}
inline void   StPhmdHit::setAdc(int var)     {mAdc = var;}

ostream&  operator<<(ostream&, const StPhmdHit&);
#endif



