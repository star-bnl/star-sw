/**********************************************************************
* StSmdPedMaker
* Author: Alexandre A. P. Suaide 
*
* This maker does calibration on the EMC detector
***********************************************************************/

/*!\class StSmdPedMaker
\author Alexandre A. P. Suaide

*/

#ifndef STAR_StSmdPedMaker
#define STAR_StSmdPedMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#define maxdet 8

class StEmcCollection;
class StEmcGeom;

class StSmdPedMaker : public StMaker 
{
  private:
         StEmcCollection *mEmc;
         StEmcGeom       *mGeo[2];
         
         Float_t   mSmdPedX[2][3][18000];
         Float_t   mSmdPedX2[2][3][18000];
         Float_t   mSmdPedSum[2][3][18000];
         Float_t   mSmdPed[2][3][18000];
         Float_t   mSmdRMS[2][3][18000]; 
         
         Float_t   mPedInterval;
         Int_t     mPedDate;
         Int_t     mPedTime;  
         Int_t     mPedStatus;  
         Int_t     mNEvents;
         Int_t     mMinEvents;  
         Bool_t    mSaveToDB; 
         
         void      FillPedestal(); 
         void      CalculatePedestals();
         void      SavePedestals(Int_t,Int_t);
         void      ZeroAll();
         Bool_t    GetEvent(); 
         Float_t   GetDeltaTime(Int_t, Int_t, Int_t, Int_t);
   
      
   public:
   
                   StSmdPedMaker(const char *name="SmdPed"); ///< Default constructor
   virtual        ~StSmdPedMaker(); ///< Default destructor
   virtual Int_t   Init(); ///< Init method
   virtual Int_t   Make(); ///< Make mathod - process each event
   virtual Int_t   Finish(); ///< Finish method - save final numbers
   
           void    setPedInterval(Float_t a) { mPedInterval =a;}
           void    setMinEvents(Int_t a)     { mMinEvents =a;}
           void    setSaveToDB(Bool_t a)     { mSaveToDB =a;}
           

   ClassDef(StSmdPedMaker, 1)  
};

#endif
