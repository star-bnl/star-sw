// $Id: StFtpcDisplay.hh,v 1.3 2000/06/07 11:35:12 oldi Exp $
// $Log: StFtpcDisplay.hh,v $
// Revision 1.3  2000/06/07 11:35:12  oldi
// Major update due to naming changes.
// New data members added.
// Added Delete() function.
//
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
#include "TPolyMarker3D.h"
#include "TPolyLine3D.h"
#include "TCanvas.h"
#include "TNode.h"

#include "MIntArray.h"

class StFtpcDisplay : public TObject {

private:
            TObjArray  *mTrack;             // found tracks
            TObjArray  *mHit;               // found hits
            TObjArray  *mGeantTrack;        // geant tracks
            TObjArray  *mGeantHit;          // geanthits
  
                Int_t   mNumRowSegment;     // number of row segments
                Int_t   mNumPhiSegment;     // number of phi segments
                Int_t   mNumEtaSegment;     // number of eta segments
                Int_t   mBounds;            // row * phi * eta segments

               Bool_t   mIsGeant;           // indicator if input is geant

               TCanvas *mX_Y_Z;             // canvas for both Ftpcs
               TCanvas *mX_Y_Zplus;         // canvas for positive Ftpc
               TCanvas *mX_Y_Zminus;        // canvas for negative Ftpc

                 TNode *mNode0;             // node for both Ftpcs
                 TNode *mNode1;             // node for positive Ftpcs
                 TNode *mNode2;             // node for negative Ftpcs

           TPolyLine3D *line;               // tracks
           TPolyLine3D *current_line;       // pointer to track

         TPolyMarker3D *found_hit;          // cluster on track in both Ftpcs
         TPolyMarker3D *found_hit_plus;     // cluster on track in positive Ftpcs
         TPolyMarker3D *found_hit_minus;    // cluster on track in negative Ftpcs
         TPolyMarker3D *unused_hit;         // unused cluster in both Ftpcs
         TPolyMarker3D *unused_hit_plus;    // unused cluster in positive Ftpcs
         TPolyMarker3D *unused_hit_minus;   // unused cluster in negative Ftpcs
         TPolyMarker3D *wrong_hit;          // wrong cluster on track in both Ftpcs
         TPolyMarker3D *wrong_hit_plus;     // wrong cluster on track in positive Ftpcs
         TPolyMarker3D *wrong_hit_minus;    // wrong cluster on track in negative Ftpcs

               Float_t *found_value;        // found hit coordinates of both Ftpcs
               Float_t *found_value_plus;   // found hit coordinates of positive Ftpc
               Float_t *found_value_minus;  // found hit coordinates of negative Ftpc
               Float_t *unused_value;       // unused hit coordinates of both Ftpcs
               Float_t *unused_value_plus;  // unused hit coordinates of positive Ftpc
               Float_t *unused_value_minus; // unused hit coordinates of negative Ftpc
               Float_t *wrong_value;        // wrong found hit coordinates of both Ftpcs
               Float_t *wrong_value_plus;   // wrong found hit coordinates of positive Ftpc
               Float_t *wrong_value_minus;  // wrong found hit coordinates of negative Ftpc

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
  void  ShowEvalTracks(MIntArray *splitArr = NULL, MIntArray *uncleanArr = NULL, MIntArray *clusterArr = NULL);                    // displays evaluated tracks
  void  FillGeant(Bool_t electrons, Bool_t non_vtx, Bool_t good, Bool_t geant_hits, 
		  Float_t eta_low, Float_t eta_up, Float_t pt_low, Float_t pt_up);             // fills geant histograms
  void  FillFound(Bool_t good_found, MIntArray *split, MIntArray *unclean, MIntArray *found_hits, 
		  Float_t eta_low, Float_t eta_up, Float_t pt_low, Float_t pt_up);             // fills found histograms
  void  Delete();                                                                               // deletes objects
  void  OnOff(Bool_t on);                                                                      // prints 'On' or 'Off'

  ClassDef(StFtpcDisplay, 1)  // Ftpc display class
};

#endif
