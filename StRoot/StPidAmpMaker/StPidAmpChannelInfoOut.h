/***************************************************************************
 *
 * $Id: StPidAmpChannelInfoOut.h,v 1.1.1.1 2000/03/09 17:48:33 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             TObject version of StPidAmpChannelInfo
 ***************************************************************************
 *
 * $Log: StPidAmpChannelInfoOut.h,v $
 * Revision 1.1.1.1  2000/03/09 17:48:33  aihong
 * Installation of package
 *
 **************************************************************************/


 

#ifndef ROOT_StPidAmpChannelInfoOut
#define ROOT_StPidAmpChannelInfoOut

#ifndef ROOT_TObject
//*KEEP, TObject.
#include "TObject.h"
//*KEND.
#endif

class StPidAmpChannelInfoOut : public TObject {
//this is a TObject version of StPidAmpChannelInfo
//so that the StPidAmpChannelInfo can be write out vi ROOT IO.
//Differ to StPidAmpChannel, 
//this class do not implement STL, so be careful with index, bounding, etc.

 public:

    StPidAmpChannelInfoOut();
    StPidAmpChannelInfoOut(const StPidAmpChannelInfoOut&);
    StPidAmpChannelInfoOut(Int_t nhitsStart, Int_t nhitsEnd, Double_t ptStart, Double_t ptEnd);
    virtual ~StPidAmpChannelInfoOut();
   // StPidAmpChannelInfoOut(Int_t nhitsStart, Int_t nhitsEnd, Double_t ptStart, Double_t ptEnd, Double_t xStart, Double_t xEnd);
    void SetNHitsRange(Int_t nhitsStart, Int_t nhitsEnd);
    void SetPtRange(Double_t ptStart, Double_t ptEnd);
    //SetXRange(Double_t xStart, Double_t xEnd);

    Int_t NHitsStart() const;
    Int_t   NHitsEnd() const;
    Double_t PtStart() const;
    Double_t   PtEnd() const;
    //Double_t XStart() cont;
    //Double_t XEnd() const;
    
    Bool_t IsInChannel(Int_t nhits, Double_t pt);        
    //   Bool_t IsInChannel(Int_t nhits, Double_t pt,Double_t x);        



 private:

    Int_t    mNHitsStart;
    Int_t    mNHitsEnd;
    Double_t mPtStart;
    Double_t mPtEnd;
 
    //Double_t mXStart;
    //Double_t mXEnd;


 ClassDef(StPidAmpChannelInfoOut,1)

};

#endif
