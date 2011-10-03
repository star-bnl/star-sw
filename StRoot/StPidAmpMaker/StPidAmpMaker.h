/***************************************************************************
 *
 * $Id: StPidAmpMaker.h,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpMaker is a mediator between StEvent and StPidAmpManager
 ***************************************************************************
 *
 * $Log: StPidAmpMaker.h,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpMaker_h
#define StPidAmpMaker_h

#include <iostream>

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "TString.h"
#include "TH1.h"
#include "TH3.h"

#include "StPidAmpManager.h"
#include "StPidAmpTrkVector.h"

class StPidAmpMaker : public StMaker {
public:

    StPidAmpMaker(const Char_t *name="StPidAmpMaker");
    virtual ~StPidAmpMaker();
    
    virtual void Clear(Option_t *option="");
    virtual Int_t  Init();
    virtual Int_t  Make();
    virtual Int_t  Finish();

    void SetNHitsFilter2LastCollection(Int_t nhits);
    void AddDefaultChannelCollection(TString fitOpt="BAR", TString drawOpt="B");
    void AddNHitsChannelCollection(Int_t x1, Int_t x2,TString fitOpt="BAR", TString drawOpt="B");
    void AddNHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt="BAR", TString drawOpt="B");
    void AddNHitsChannelCollection(Int_t x1, Int_t x2,Int_t x3, Int_t x4,TString fitOpt="BAR", TString drawOpt="B");
    void AddNHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt="BAR", TString drawOpt="B");

    void AddPtChannelCollection(Double_t x1, Double_t x2,TString fitOpt="BAR", TString drawOpt="B");
    void AddPtChannelCollection(Double_t x1, Double_t x2, Double_t x3,TString fitOpt="BAR", TString drawOpt="B");
    void AddPtChannelCollection(Double_t x1, Double_t x2,Double_t x3, Double_t x4,TString fitOpt="BAR", TString drawOpt="B");
    void AddPtChannelCollection(Double_t x1, Double_t x2, Double_t x3, Double_t x4, Double_t x5,TString fitOpt="BAR", TString drawOpt="B");
    

    void AddPtNHitsChannelCollection(Int_t n, Int_t* nhitsAry,Int_t p, Double_t* ptAry,TString fitOpt="BAR", TString drawOpt="B");
  
    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StPidAmpMaker.h,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
private:

    TString drawOpt;
    TH3D*   dependHisto;//!
    //       ^^^^^^^^^^  3D histogram of nhits,pt,X (X reserve for future).

    Char_t collectionName[256];
    
    StPidAmpTrkVector*  ampTrks; //!
    StPidAmpManager* theManager; //!

    ClassDef(StPidAmpMaker,1)
};
#endif

// if no specific Add*ChannelCollection() is called, a default ChannelCollection will be booked.
// if there is a call to Add*ChannelCollection, the default ChannelCollection will not be booked.
// so if you wanna compare default vs.NHits, you have make a call 
// addDefaultChannelCollection() after you called AddNHitsChannelCollection()
