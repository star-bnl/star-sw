/***************************************************************************
 *
 * $Id: StPidAmpChannelInfoOut.h,v 1.2 2003/09/02 17:58:09 perev Exp $
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
 * Revision 1.2  2003/09/02 17:58:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2000/07/22 22:27:14  aihong
 * move files from StPidAmpMaker to StEventUtilities
 *
 * Revision 1.3  2000/04/09 16:36:43  aihong
 * change for adapting NHitDcaNet added
 *
 * Revision 1.2  2000/03/24 15:11:09  aihong
 * add PrintContent()
 *
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

#include <Stiostream.h>

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
    StPidAmpChannelInfoOut(Int_t nhitsStart, Int_t nhitsEnd, Double_t ptStart, Double_t ptEnd, Double_t dcaStart, Double_t dcaEnd);
    void SetNHitsRange(Int_t nhitsStart, Int_t nhitsEnd);
    void SetPtRange(Double_t ptStart, Double_t ptEnd);
    void SetDcaRange(Double_t dcaStart, Double_t dcaEnd);

    void PrintContent();

    Int_t  NHitsStart() const;
    Int_t    NHitsEnd() const;
    Double_t  PtStart() const;
    Double_t    PtEnd() const;
    Double_t DcaStart() const;
    Double_t   DcaEnd() const;
    
    Bool_t IsInChannel(Int_t nhits, Double_t pt);        
    Bool_t IsInChannel(Int_t nhits, Double_t pt,Double_t dca);        



 private:

    Int_t    mNHitsStart;
    Int_t    mNHitsEnd;
    Double_t mPtStart;
    Double_t mPtEnd;
 
    Double_t mDcaStart;
    Double_t mDcaEnd;


 ClassDef(StPidAmpChannelInfoOut,1)

};


ostream& operator<<(ostream& s, const StPidAmpChannelInfoOut& infoOut);


#endif
