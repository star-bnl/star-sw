/***************************************************************************
 *
 * $Id: StPidAmpManager.h,v 1.4 2000/04/12 20:14:29 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpManager manages multiple StPidAmpChannelCollections
 ***************************************************************************
 *
 * $Log: StPidAmpManager.h,v $
 * Revision 1.4  2000/04/12 20:14:29  aihong
 * change to adapt to ROOT 2.24 and bug fixed with help from valery
 *
 * Revision 1.3  2000/04/11 15:45:25  aihong
 * change to adapt dividing trks by channel for faster filling
 *
 * Revision 1.2  2000/04/09 16:36:43  aihong
 * change for adapting NHitDcaNet added
 *
 * Revision 1.1.1.1  2000/03/09 17:48:33  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpManager_h
#define StPidAmpManager_h



#include <iostream>

#include "TString.h"
#include "TH1.h"
#include "TH3.h"

#include "StPidAmpMaker/Infrastructure/StPidAmpChannelCollectionVector.hh"
#include "StPidAmpMaker/StPidAmpTrkVector.h"


class StPidAmpManager{


 public:

     StPidAmpManager();
     StPidAmpManager(const StPidAmpManager&);
     virtual  ~StPidAmpManager();

     void setNHits4BGNet(Int_t theNHits);
     void bookADefaultChannelCollection(TString fitOpt, TString drawOpt);

 
     void bookAPtChannelCollection(Double_t x1, Double_t x2, Double_t x3,TString fitOpt, TString drawOpt);
     void bookAPtChannelCollection(Double_t x1, Double_t x2, Double_t x3, Double_t x4,TString fitOpt, TString drawOpt);
     void bookAPtChannelCollection(Double_t x1, Double_t x2, Double_t x3, Double_t x4, Double_t x5,TString fitOpt, TString drawOpt);

 
     void bookANHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt, TString drawOpt);
     void bookANHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4,TString fitOpt, TString drawOpt);
     void bookANHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt, TString drawOpt);

     void bookANHitsDcaChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt, TString drawOpt, Double_t d1, Double_t d2, Double_t d3);
     void bookANHitsDcaChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4,TString fitOpt, TString drawOpt, Double_t d1, Double_t d2, Double_t d3);
     void bookANHitsDcaChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt, TString drawOpt, Double_t d1, Double_t d2, Double_t d3);



     void bookAPtNHitsChannelCollection(Int_t n, Int_t* nitsAry,Int_t p, Double_t* ptAry,TString fitOpt, TString drawOpt);


     void passTrksAddress(StPidAmpTrkVector* trks); //!
     void printAllSetsNames();
     void printNSets();//print # of sets in store
     void process(TH3D* histo); 
     void clearStore();

     StPidAmpChannelCollectionVector* netSets(); //!
     

 private:

      



  

     StPidAmpChannelCollectionVector* mChannelCollections; //!
     StPidAmpTrkVector* mTrks; //!


     ClassDef(StPidAmpManager,1)
    
};

ostream& operator<<(ostream& s, StPidAmpManager& store);



#endif
