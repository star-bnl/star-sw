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
class globalTrack ;
class StL3Trigger ;
class StPrimaryVertex ;

class Stl3RawReaderMaker : public StMaker {
 
 private:
    // General stuff
    TDataSet* DAQReaderSet ; //!
    StL3Reader*  ml3reader ;  //!
    // Mini Event
    Stl3MiniEvent*  mL3Event ;  //!
    TTree* mGlobalTrackTree ;  //!
    // StEvent
    StL3Trigger* myStL3Trigger ; //!
    // switches
    Int_t WriteMiniEvent ; //!
    Int_t WriteStEvent ; //!

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
  {static const char cvs[]="Tag $Name:  $ $Id: Stl3RawReaderMaker.h,v 1.2 2000/09/13 14:57:26 flierl Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(Stl3RawReaderMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
