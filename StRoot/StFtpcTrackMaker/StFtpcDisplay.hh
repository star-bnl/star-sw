// $Id: StFtpcDisplay.hh,v 1.2 2000/05/12 12:59:14 oldi Exp $
// $Log: StFtpcDisplay.hh,v $
// Revision 1.2  2000/05/12 12:59:14  oldi
// removed delete operator for mSegment in StFtpcConfMapper (mSegment was deleted twice),
// add two new constructors for StFtpcTracker to be able to refit already existing tracks,
// minor cosmetics
//
// Revision 1.1  2000/05/10 13:39:14  oldi
// Initial version of StFtpcTrackMaker
//

//////////////////////////////////////////////////////////////////////////////////
//                                                                              //
// StFtpcDiplsay class - displays tracks and clusters                           //
//                                                                              //
//////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcDisplay
#define STAR_StFtpcDisplay

#include "TObjArray.h"
#include "TObject.h"
#include "TCanvas.h"
#include "TNode.h"

#include "MIntArray.h"

class StFtpcDisplay : public TObject {

private:
            TObjArray  *mTrack;         // found tracks
            TObjArray  *mHit;           // found hits
            TObjArray  *mGeantTrack;    // geant tracks
            TObjArray  *mGeantHit;      // geanthits
  
                Int_t   mNumRowSegment; // number of row segments
                Int_t   mNumPhiSegment; // number of phi segments
                Int_t   mNumEtaSegment; // number of eta segments
                Int_t   mBounds;        // row * phi * eta segments

               Bool_t   mIsGeant;       // indicator if input is geant

               TCanvas *mX_Y_Z;         // canvas for both Ftpcs
               TCanvas *mX_Y_Zplus;     // canvas for positive Ftpc
               TCanvas *mX_Y_Zminus;    // canvas for negative Ftpc

                 TNode *mNode0;         // node for both Ftpcs
                 TNode *mNode1;         // node for positive Ftpcs
                 TNode *mNode2;         // node for negative Ftpcs

           TPolyLine3D *l;              // tracks
           TPolyLine3D *k;              // pointer to tracks
               Float_t *value;          // hit coordinates of both Ftpcs
               Float_t *value_plus;     // hit coordinates of positive Ftpc
               Float_t *value_minus;    // hit coordinates of negative Ftpc

public:
            StFtpcDisplay();                                    // default constructor
            StFtpcDisplay(TObjArray *hits, TObjArray *tracks);  // usual constructor 
            StFtpcDisplay(TObjArray *hits, TObjArray *tracks, TObjArray *geanthits, TObjArray *geanttracks);  // constructor for evaluator 
  virtual  ~StFtpcDisplay();                                    // destructor

  Bool_t  IsGeant() { return mIsGeant; }   // returns true if geant option is true

  //void  Info();                                                                              // prints general information
  void  TrackInfo();                                                                           // plots single tracks
  void  ShowClusters();                                                                        // displays clusters
  void  ShowTracks(Int_t trackanz = 0, Int_t trackarray[] = NULL);                             // displays tracks
  void  ShowEvalTracks(MIntArray *splitArr = 0, MIntArray *uncleanArr = 0);                    // displays evaluated tracks
  void  FillGeant(Bool_t electrons, Bool_t non_vtx, Bool_t good, Bool_t geant_hits);           // fills geant histograms
  void  FillFound(Bool_t good_found, MIntArray *split, MIntArray *unclean, Bool_t found_hits); // fills found histograms
  void  OnOff(Bool_t on);                                                                      // prints 'On' or 'Off'

  ClassDef(StFtpcDisplay, 1)  // Ftpc display class
};

#endif
