/***************************************************************************
 *
 * $Id: StRTpcFieldCage.h,v 1.1 2002/02/06 18:39:13 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Field Cage Geometry Interface 
 *
 ***************************************************************************
 *
 * $Log: StRTpcFieldCage.h,v $
 * Revision 1.1  2002/02/06 18:39:13  hardtke
 * Add tpc Field Cage structure
 *
 *
 **************************************************************************/
#ifndef __STRTPCFieldCage__
#define __STRTPCFieldCage__
//#include <TObject.h>
#include "StMessMgr.h"
#include "StTpcFieldCageI.h"
#include "tables/St_tpcFieldCage_Table.h"

class StRTpcFieldCage : public StTpcFieldCageI {

private:

  St_tpcFieldCage* mFieldCage;

public:

  StRTpcFieldCage(St_tpcFieldCage* In=0){AddData(In);}
  ~StRTpcFieldCage(){}
  void AddData(St_tpcFieldCage* In) {
   mFieldCage = In;
 } 

  //Implements Abstract Interface 
 
  double InnerFieldCageShift()    const; //Shift in cm of IFC relative to OFC
  double EastClockError()    const; //(radians) phi rotation of east tpc wheel
  double WestClockError()    const; //(radians) phi rotation of west tpc wheel 


 ClassDef(StRTpcFieldCage,0)

};
inline double  StRTpcFieldCage::InnerFieldCageShift() const { return (*mFieldCage)[0].innerFieldCageShift;}
inline double  StRTpcFieldCage::EastClockError() const { return (*mFieldCage)[0].eastClockError;}
inline double  StRTpcFieldCage::WestClockError() const { return (*mFieldCage)[0].westClockError;}

#endif









