//
//  $Id: Stl3CounterMaker.h,v 1.2 2002/02/20 22:09:49 struck Exp $
//
//  $Log: Stl3CounterMaker.h,v $
//  Revision 1.2  2002/02/20 22:09:49  struck
//  added some debugging info
//
//  Revision 1.1  2002/02/13 22:36:31  struck
//  major code clean-up for Stl3RawReaderMaker, first version of Stl3CounterMaker
//
//


#ifndef STAR_Stl3CounterMaker
#define STAR_Stl3CounterMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                       Stl3CounterMaker                               //
//                                                                      //
//  calculates trigger counters                                         //
//  - nProcessed                                                        //
//  - nAccepted                                                         //
//  - nBuild                                                            //
//  for each l3 algorithm switched on for the given run                 //
//  and the global counters                                             //
//  - nProcessed                                                        //
//  - nReconstructed                                                    //
//  and writes this to the database on a daq-file basis                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StDAQMaker/StDAQReader.h"
#include "TString.h"


// global upper limits
// correct numbers should come from the database
#define MaxNumberOfGl3Nodes   10
#define MaxNumberOfAlgorithms 20

#define IntParameterSize       5
#define FloatParameterSize     5

// some foreward declaration since includes don't work
// in these cases 
class St_l3GlobalCounter;
class St_l3AlgorithmCount;


// ol' fashioned structs for counter bookkeeping
struct AlgorithmCounter {
      int  algId;
      int  nProcessed;
      int  nAccept;
      int  nBuild;
};

struct GlobalCounter {
      int  nProcessed;
      int  nReconstructed;
};


class Stl3CounterMaker : public StMaker {
 
 private:
    // General stuff
    TDataSet*          DAQReaderSet; //!
    StL3Reader*        ml3reader;  //!
    TString*           mDaqFileName; //!
    TString*           mDbTableFileName; //!
    int                mDaqFileSequenceNumber; //!
    int                mRunNumber; //!
    

    // Database
    TDataSet*            mDbSet; //!
    St_l3GlobalCounter*  mGlobalCounterTable; //!
    St_l3AlgorithmCount* mAlgorithmCounterTable; //!

    // switches
    bool               mL3On;
    bool               mStoreDbTables;

    // counter
    GlobalCounter      mGlobalCounter[MaxNumberOfGl3Nodes];
    AlgorithmCounter   mAlgorithmCounter[MaxNumberOfGl3Nodes][MaxNumberOfAlgorithms];

    int                mNumberOfGl3Nodes;
    int                mNumberOfAlgorithms;
    int                mEventCounter;

 public: 
                  Stl3CounterMaker(const char *name="l3Counter");
   virtual       ~Stl3CounterMaker();
   virtual Int_t Init();
   virtual Int_t Make();
   virtual Int_t GetCounters();
   virtual Int_t Finish();

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: Stl3CounterMaker.h,v 1.2 2002/02/20 22:09:49 struck Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(Stl3CounterMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
