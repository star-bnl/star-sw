/**********************************************************************
 *
 * $Id: StEbyeEvent.h,v 1.3 2001/11/06 17:05:28 posk Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 +         Art Poskanzer, LBNL Nov. 2000
 +         Alexander Wetzler, IKF
 *
 **********************************************************************
 *
 * Description:  This maker defines the event structure for the
 *               event-by-event DST.
 *
 **********************************************************************
 *
 * $Log: StEbyeEvent.h,v $
 * Revision 1.3  2001/11/06 17:05:28  posk
 * New 40 Gev centrality bins. Using only sin terms at 40 GeV.
 *
 * Revision 1.2  2001/05/14 23:04:02  posk
 * Can select PID for event plane particles. Protons not used for 1st har.
 * event plane.
 *
 * Revision 1.1.1.1  2000/11/22 19:45:09  jcs
 * Imported sources for MICRODST
 *
 * Revision 1.3  2000/09/01 22:59:11  jgreid
 * version 1 revision ; multiple file handling + additional data members added
 *
 * Revision 1.2  2000/08/03 20:12:13  jgreid
 * added CTBm() convenience function
 *
 * Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
 * EbyE DST creation and access tools
 *
 *
 *********************************************************************/

#ifndef _StEbyeEvent
#define _StEbyeEvent

#include <iostream.h>
#include "TObject.h"
#include "TClonesArray.h"
#include "StEbyeTrack.h"

class StEbyeEvent : public TObject {

 private:

  static const Int_t mVersion = 1;   // DST version number is 1
  static const Int_t mExpt = 49;     // Experiemnt is NA49

  Int_t mNtrack;                     // track number

  Int_t mEventID;                    // event ID
  Int_t mRunID;                      // run ID
  Int_t mOrigMult;                   // number of StEvent tracks
  Int_t mCentMult;                   // multiplicity used to determine centrality

  Float_t mCentrality;               // centrality measure

  Float_t mVx;                       // primary vertex position
  Float_t mVy;
  Float_t mVz;

  TClonesArray *fTracks;
  static TClonesArray *fgTracks;

  // added for Flow Analysis

  Float_t mEveto;
  Int_t mADCS3;
  Int_t mS1;
  Int_t mS2;
  Int_t mS4;
  
  Float_t mVsx;                      // primary vertex position sigma
  Float_t mVsy;
  Float_t mVsz;

  Float_t mVpchi2;                   // primary vertex position pchi2

  Int_t mVnFitTrack;                 // number of tracks used to fit vertex Position;
  Int_t mViflag;                     // primary vertex iflag
  
  Float_t mTPCpressure;              // pressure in TPCs

  Float_t mTPCvDriftV1;              // drift velocity in TPCs
  Float_t mTPCvDriftV2;
  Float_t mTPCvDriftMR;
  Float_t mTPCvDriftML;

  Float_t mTPCGasGainV1;             // gas gain in TPCs
  Float_t mTPCGasGainV2;
  Float_t mTPCGasGainMR;
  Float_t mTPCGasGainML;

  Float_t mTPCtempV1;                //temperature in TPCs
  Float_t mTPCtempV2;
  Float_t mTPCtempMR;
  Float_t mTPCtempML;

  // end of additions
  
 public:
  StEbyeEvent();
  virtual ~StEbyeEvent() { Clear(); }

  void Clear(Option_t *option ="");

  Int_t Version() const { return mVersion; };
  Int_t Expt() const { return mExpt; };

  Int_t EventID() const { return mEventID; }; 
  Int_t RunID() const { return mRunID; };
  Int_t OrigMult() const { return mOrigMult; };
  Int_t CentMult() const { return mCentMult; };

  /* calculating centrality online now
   * Float_t Centrality() const { return mCentrality; };
   */

  Float_t Centrality();
  
  Float_t Vx() const { return mVx; }
  Float_t Vy() const { return mVy; }
  Float_t Vz() const { return mVz; }

  // added for Flow Analysis

  Float_t Eveto() {return mEveto;}
  Int_t ADCS3() {return mADCS3;}
  Int_t S1() {return mS1;}
  Int_t S2() {return mS2;}
  Int_t S4() {return mS4;}
  
  Float_t Vsx() {return mVsx;}                  
  Float_t Vsy() {return mVsy;}
  Float_t Vsz() {return mVsz;}

  Float_t Vpchi2() {return mVpchi2;}  

  Int_t VnFitTrack() {return mVnFitTrack;} 
  Int_t Viflag() {return mViflag;}                     
  
  Float_t TPCpressure() {return mTPCpressure;}         

  Float_t TPCvDriftV1() {return mTPCvDriftV1;}           
  Float_t TPCvDriftV2() {return mTPCvDriftV2;}
  Float_t TPCvDriftMR() {return mTPCvDriftMR;}
  Float_t TPCvDriftML() {return mTPCvDriftML;}

  Float_t TPCGasGainV1() {return mTPCGasGainV1;}   
  Float_t TPCGasGainV2() {return mTPCGasGainV2;}
  Float_t TPCGasGainMR() {return mTPCGasGainMR;}
  Float_t TPCGasGainML() {return mTPCGasGainML;}

  Float_t TPCtempV1() {return mTPCtempV1;}         
  Float_t TPCtempV2() {return mTPCtempV2;}
  Float_t TPCtempMR() {return mTPCtempMR;}
  Float_t TPCtempML() {return mTPCtempML;}

  // end of additions
  
  void AddTrack(StEbyeTrack* pEbyeTrack);

  void SetNtrack(const Int_t ntrk) { mNtrack = ntrk; }
  void SetEventID(const Int_t id) { mEventID = id; }
  void SetRunID(const Int_t id) { mRunID = id; }
  void SetOrigMult(const Int_t tracks) { mOrigMult = tracks; }
  void SetCentMult(const Int_t tracks) { mCentMult = tracks; }

  void SetCentrality(const Float_t mEveto);
  void SetCentrality(const UInt_t N);

  void SetVx(const Float_t vx) { mVx = vx; }
  void SetVy(const Float_t vy) { mVy = vy; }
  void SetVz(const Float_t vz) { mVz = vz; }

  void SetVertex(const Float_t vx, const Float_t vy, const Float_t vz) {
    mVx = vx; mVy = vy; mVz = vz; }

  Int_t Ntrack() { return mNtrack; }
  TClonesArray *Tracks() { return fTracks; }

  // added for Flow Analysis

  void SetEveto(Float_t eveto) {mEveto = eveto;}
  void SetADCS3(Int_t adcs3) {mADCS3 = adcs3;}
  void SetS1(Int_t s1) {mS1 = s1;}
  void SetS2(Int_t s2) {mS2 = s2;}
  void SetS4(Int_t s4) {mS4 = s4;}
  
  void SetVsx(Float_t vs) {mVsx = vs;}                  
  void SetVsy(Float_t vs) {mVsy = vs;}                  
  void SetVsz(Float_t vs) {mVsz = vs;}                  

  void SetVpchi2(Float_t vpchi2) {mVpchi2 = vpchi2;}  

  void SetVnFitTrack(Int_t vnfittrack) {mVnFitTrack = vnfittrack;} 
  void SetViflag(Int_t viflag) {mViflag = viflag;}                     
  
  void SetTPCpressure(Float_t tpcpressure) {mTPCpressure = tpcpressure;}
   
  void SetTPCvDriftV1(Float_t tpcvdrift) {mTPCvDriftV1 = tpcvdrift;}           
  void SetTPCvDriftV2(Float_t tpcvdrift) {mTPCvDriftV2 = tpcvdrift;}           
  void SetTPCvDriftMR(Float_t tpcvdrift) {mTPCvDriftMR = tpcvdrift;}           
  void SetTPCvDriftML(Float_t tpcvdrift) {mTPCvDriftML = tpcvdrift;}           

  void SetTPCGasGainV1(Float_t tpcgasgain) {mTPCGasGainV1 = tpcgasgain;}   
  void SetTPCGasGainV2(Float_t tpcgasgain) {mTPCGasGainV2 = tpcgasgain;}   
  void SetTPCGasGainMR(Float_t tpcgasgain) {mTPCGasGainMR = tpcgasgain;}   
  void SetTPCGasGainML(Float_t tpcgasgain) {mTPCGasGainML = tpcgasgain;}   

  void SetTPCtempV1(Float_t tpctemp) {mTPCtempV1 = tpctemp;}         
  void SetTPCtempV2(Float_t tpctemp) {mTPCtempV2 = tpctemp;}         
  void SetTPCtempMR(Float_t tpctemp) {mTPCtempMR = tpctemp;}         
  void SetTPCtempML(Float_t tpctemp) {mTPCtempML = tpctemp;}         

  // end of additions
  
  
  ClassDef(StEbyeEvent,1)
};

#endif
