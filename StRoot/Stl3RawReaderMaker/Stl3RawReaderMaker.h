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
//#include "StDaqLib/L3/L3_Reader.hh"
#include "TTree.h"
#include "Stl3MiniEvent.h"
//#include "StEventTypes.h"

class globalTrack ;
class StL3Trigger ;

class Stl3RawReaderMaker : public StMaker {
 
 private:
  TDataSet* DAQReaderSet ;
  // Mini Event
  StL3Reader*  ml3reader ; 
  Stl3MiniEvent*  mL3Event ;
  TTree* mGlobalTrackTree ;
  // StEvent
  StL3Trigger* myStL3Trigger ;

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
   Int_t findVertexMethod1(globalTrack* tracks, Int_t nOfTracks, Float_t* vertex) ;
   
   // switches
   Int_t WriteMiniEvent ;
   Int_t WriteStEvent ;

   // getters
   TTree* GetGlobalTrackTree() {return mGlobalTrackTree;} ;

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: Stl3RawReaderMaker.h,v 1.1 2000/09/06 21:05:41 flierl Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(Stl3RawReaderMaker, 0)   //StAF chain virtual base class for Makers
};

#endif
