/**********************************************************************
 *
 * $Id: StEbyeEvent.cxx,v 1.3 2001/11/06 17:05:26 posk Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 +         Art Poskanzer, LBNL Nov. 2000
 +         Alexander Wetzler, IKF
 *
 **********************************************************************
 *
 * Description:  This maker defines the track structure for the
 *               event-by-event DST.
 *
 **********************************************************************
 *
 * $Log: StEbyeEvent.cxx,v $
 * Revision 1.3  2001/11/06 17:05:26  posk
 * New 40 Gev centrality bins. Using only sin terms at 40 GeV.
 *
 * Revision 1.2  2001/05/14 23:03:58  posk
 * Can select PID for event plane particles. Protons not used for 1st har.
 * event plane.
 *
 * Revision 1.1.1.1  2000/11/22 19:45:09  jcs
 * Imported sources for MICRODST
 *
 * Revision 1.2  2000/09/01 22:59:11  jgreid
 * version 1 revision ; multiple file handling + additional data members added
 *
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

Float_t StEbyeEvent::Centrality() {

  /*
   *Calculate Centrality division online for different veto 
   *calorimeter callibrations
   */
  
  Float_t evetoCal;
  Int_t centrality;
  Float_t ZdcDividers[] = {9250.,14670.,21190.,26080.,29340.};


  //1996 data set
  if (mRunID > 1389 && mRunID < 1614) {evetoCal = mEveto;}
  //1999 40GeV data set
  else if (mRunID > 3003  && mRunID < 3166) {evetoCal = mEveto + 5052.;}
  //2000 data set
  else if (mRunID >  4016 && mRunID < 4102) {evetoCal = mEveto*.89227 + 4171.;}
  //anything else
  else {evetoCal = 0;}

  //Set centrality
  if (mEveto < ZdcDividers[0]) { centrality = 1; }
  else if (mEveto < ZdcDividers[1]) { centrality = 2; }
  else if (mEveto < ZdcDividers[2]) { centrality = 3; }
  else if (mEveto < ZdcDividers[3]) { centrality = 4; }
  else if (mEveto < ZdcDividers[4]) { centrality = 5; }
  else { centrality = 6; }

  return centrality;
}

void StEbyeEvent::SetCentrality(Float_t mEveto) {
  Float_t ZdcDividers[] = {9250.,14670.,21190.,26080.,29340.};

  if (mEveto < ZdcDividers[0]) { mCentrality = 1; }
  else if (mEveto < ZdcDividers[1]) { mCentrality = 2; }
  else if (mEveto < ZdcDividers[2]) { mCentrality = 3; }
  else if (mEveto < ZdcDividers[3]) { mCentrality = 4; }
  else if (mEveto < ZdcDividers[4]) { mCentrality = 5; }
  else { mCentrality = 6; }

}

void StEbyeEvent::SetCentrality(UInt_t N) {

  //UInt_t centralityDivider[] = {20,100,180,270,360,460,560,660,870};
  // One-Half of the Flow Divider positions since we only have 
  //  negative particles
  UInt_t centralityDivider[] = {10,50,90,135,180,230,280,330,435};

  if (N < centralityDivider[0]) { mCentrality = 0; }
  else if (N < centralityDivider[1]) { mCentrality = 1; }
  else if (N < centralityDivider[2]) { mCentrality = 2; }
  else if (N < centralityDivider[3]) { mCentrality = 3; }
  else if (N < centralityDivider[4]) { mCentrality = 4; }
  else if (N < centralityDivider[5]) { mCentrality = 5; }
  else if (N < centralityDivider[6]) { mCentrality = 6; }
  else if (N < centralityDivider[7]) { mCentrality = 7; }
  else if (N < centralityDivider[8]) { mCentrality = 8; }
  else { mCentrality = 9; }

}

void StEbyeEvent::Clear(Option_t *option) {

  fTracks->Clear(option);
  mNtrack=0;
}
