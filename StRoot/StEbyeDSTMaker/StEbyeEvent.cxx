/**********************************************************************
 *
 * $Id: StEbyeEvent.cxx,v 1.1.1.1 2000/08/01 13:57:55 jgreid Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 *         incorporates elements of code by
 *         Poskanzer, Snellings, & Voloshin
 *
 **********************************************************************
 *
 * Description:  This maker defines the track structure for the
 *               event-by-event DST.
 *
 **********************************************************************
 *
 * $Log: StEbyeEvent.cxx,v $
 * Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
 * EbyE DST creation and access tools
 *
 *
 *********************************************************************/

#include "StEbyeEvent.h"
#include "StEbyeTrack.h"

ClassImp(StEbyeEvent)

TClonesArray *StEbyeEvent::fgTracks = 0;

StEbyeEvent::StEbyeEvent() {

  // Create an StEbyeEvent object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  
  if (!fgTracks) fgTracks = new TClonesArray("StEbyeTrack", 1200);
  fTracks = fgTracks;
  mNtrack = 0;
}

void StEbyeEvent::AddTrack(StEbyeTrack* inputTrack) {

  // Add a new track to the list of tracks for this StEbyeEvent.
  // To avoid calling the very time consuming operator new for each track,
  // the standard but not well know C++ operator "new with placement"
  // is called. If tracks[i] is 0, a new Track object will be created
  // otherwise the previous Track[i] will be overwritten.

  TClonesArray &tracks = *fTracks;
  new(tracks[mNtrack++]) StEbyeTrack(inputTrack);
}

void StEbyeEvent::Clear(Option_t *option) {

  fTracks->Clear(option);
  mNtrack=0;
}
