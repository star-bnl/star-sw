//
//  $Id: Stl3RawReaderMaker.h,v 1.5 2001/08/29 20:24:49 struck Exp $
//
//  $Log: Stl3RawReaderMaker.h,v $
//  Revision 1.5  2001/08/29 20:24:49  struck
//  makes Solaris compiler happy
//
//  Revision 1.4  2001/08/20 22:32:00  struck
//  first version filling L3 counters and algorithm info into StEvent
//
//


#ifndef STAR_Stl3RawReaderMaker
#define STAR_Stl3RawReaderMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                       Stl3RawReaderMaker                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StDAQMaker/StDAQReader.h"
#include "TTree.h"
#include "Stl3MiniEvent.h"

// some foreward declaration since includes don't work
// in these cases 
class globalTrack;
class StEvent;
class StL3Trigger;
class StPrimaryVertex;


// ol' fashioned structs for counter bookkeeping
struct AlgorithmCounter {
      unsigned int  algId;
      int  nProcessed;
      int  nAccept;
      int  nBuild;
};

struct GlobalCounter {
      int  nProcessed;
      int  nReconstructed;
};


class Stl3RawReaderMaker : public StMaker {
 
 private:
    // General stuff
    TDataSet*          DAQReaderSet ; //!
    StL3Reader*        ml3reader ;  //!

    // Mini Event
    Stl3MiniEvent*     mL3Event ;  //!
    TTree*             mGlobalTrackTree ;  //!

    // StEvent
    StEvent*           mStEvent; //!
    StL3Trigger*       myStL3Trigger ; //!

    // switches
    bool               mWriteMiniEvent ; //!
    bool               mWriteStEvent ; //!
    int                mCalculateVertex ; //!
    bool               mL3On;

    // counter
    GlobalCounter      mGlobalCounter[10];
    AlgorithmCounter   mAlgorithmCounter[10][20];

    int                mNumberOfGl3Nodes;
    int                mNumberOfAlgorithms;

    // limits
    int                mMaxNumberOfGl3Nodes;
    int                mMaxNumberOfAlgorithms;


 protected:
 public: 
                  Stl3RawReaderMaker(const char *name="l3RawReader");
   virtual       ~Stl3RawReaderMaker();
   virtual Int_t Init();
   virtual Int_t Make();

   // filling routines
   // L3 Mini Event
   Int_t fillTree() ;
   Int_t fillMiniEventWithL3GlobalTracks() ;
   Int_t fillMiniEventWithi960Clusters() ;
   
   // StEvent
   Int_t fillStEvent() ;
   Int_t fillStEventWithL3GlobalTracks()  ;
   Int_t fillStEventWithi960Hits() ;

   // vertexd finding routines
   Int_t findVertexMethod1(StPrimaryVertex& ) ;
   Int_t findVertexMethod2(StPrimaryVertex& ) ;
 
   // getters
   TTree* GetGlobalTrackTree() {return mGlobalTrackTree;} ;

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: Stl3RawReaderMaker.h,v 1.5 2001/08/29 20:24:49 struck Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(Stl3RawReaderMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
