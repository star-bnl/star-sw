////////////////////////////////////////////////////////////////////////
//
//                       StWriteEvent and StWriteTrack classes
//                       =======================
//
//  The StFlowNanoEvent class is a naive/simple example of an event structure.
//     public:
//        Int_t          fNtrack;
//        EventHeader    fEvtHdr;
//        TClonesArray  *fTracks;
//
//   The StFlowNanoEventHeader class has 3 data members (integers):
//     public:
//        Int_t          fEvtNum;
//        Int_t          fRun;
//        Int_t          fDate;
//
//
//   The StFlowNanoEvent data member fTracks is a pointer to a TClonesArray.
//   It is an array of a variable number of tracks per Event.
//   Each element of the array is an object of class Track with the members:
//     private:
//        Float_t      fPt;
//        Float_t      fPhi;
//        Float_t      fEta;
//
//   During the processing of the StFlowNanoEvent (optionally) also a large number
//   of histograms can be filled. The creation and handling of the
//   histograms is taken care of by the HistogramManager class.
//
////////////////////////////////////////////////////////////////////////

#include "StFlowNanoEvent.h"

ClassImp(StFlowNanoEventHeader)
ClassImp(StFlowNanoEvent)
ClassImp(StFlowNanoTrack)

TClonesArray *StFlowNanoEvent::fgTracks = 0;

//-----------------------------------------------------------------------
StFlowNanoEvent::StFlowNanoEvent()
{
   // Create an StFlowNanoEvent object.
   // When the constructor is invoked for the first time, the class static
   // variable fgTracks is 0 and the TClonesArray fgTracks is created.

   if (!fgTracks) fgTracks = new TClonesArray("StFlowNanoTrack", 1000);
   fTracks = fgTracks;
   fNtrack = 0;
}

//-----------------------------------------------------------------------
StFlowNanoEvent::~StFlowNanoEvent()
{
   Clear();
}

//-----------------------------------------------------------------------
void StFlowNanoEvent::AddTrack(Float_t pt, Float_t phi, Float_t eta )
{
   // Add a new track to the list of tracks for this StFlowNanoEvent.
   // To avoid calling the very time consuming operator new for each track,
   // the standard but not well know C++ operator "new with placement"
   // is called. If tracks[i] is 0, a new Track object will be created
   // otherwise the previous Track[i] will be overwritten.

   TClonesArray &tracks = *fTracks;
   new(tracks[fNtrack++]) StFlowNanoTrack(pt,phi,eta);
}

//-----------------------------------------------------------------------
void StFlowNanoEvent::Clear(Option_t *option)
{
   fTracks->Clear(option);
}

//-----------------------------------------------------------------------
void StFlowNanoEvent::Reset(Option_t *option)
{
// Static function to reset all static objects for this StFlowNanoEvent
//   fgTracks->Delete(option);
   delete fgTracks; fgTracks = 0;
}

//-----------------------------------------------------------------------
void StFlowNanoEvent::SetHeader(Int_t i, Int_t run, Int_t date)
{
   fNtrack = 0;
   fEvtHdr.Set(i, run, date);
}

//-----------------------------------------------------------------------
StFlowNanoTrack::StFlowNanoTrack(Float_t pt, Float_t phi, Float_t eta) : TObject()
{
   // Create a track object.
   // Note that in this example, data members do not have any physical meaning.

   fPt = pt;
   fPhi = phi;
   fEta = eta;
}
