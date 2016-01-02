#ifndef StPicoKfVertexEvent__h
#define StPicoKfVertexEvent__h

/* **************************************************
 *  A class to save event information with KF vertex.
 *
 *  Authors:  **Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */

#include "StarClassLibrary/StThreeVectorF.hh"

class TTree;
class TFile;
class StPicoEvent;

class StPicoKfVertexEvent
{
public:
   StPicoKfVertexEvent(char const*);
   void addEvent(StPicoEvent const&,StThreeVectorF const* kfFullEvent,
       StThreeVectorF const* kfSubEvt1=NULL,StThreeVectorF const* kfSubEvt2=NULL,
       int nTracksFullEvt=0,int nTracksSubEvt1=0,int nTracksSubEvt2=0);
   void closeFile();

private:

   int   mRunId;
   int   mEventId;
   int   mRefMult;
   int   mNTracks;
   int   mNTracksSubEvt1;
   int   mNTracksSubEvt2;
   int   mGRefMult;

   float mVx;
   float mVy;
   float mVz;

   float mKfVx;
   float mKfVy;
   float mKfVz;

   float mKfSubEvt1Vx;
   float mKfSubEvt1Vy;
   float mKfSubEvt1Vz;

   float mKfSubEvt2Vx;
   float mKfSubEvt2Vy;
   float mKfSubEvt2Vz;

   TFile* mOutputFile;
   TTree* mTree;
};

#endif
