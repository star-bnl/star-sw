// $Id: StFtpcDisplay.hh,v 1.11 2003/09/16 15:27:02 jcs Exp $
// $Log: StFtpcDisplay.hh,v $
// Revision 1.11  2003/09/16 15:27:02  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.10  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.9  2003/01/16 18:04:32  oldi
// Bugs eliminated. Now it compiles on Solaris again.
// Split residuals for global and primary fit.
//
// Revision 1.8  2002/04/05 16:50:28  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.7  2001/07/13 18:01:03  oldi
// New function WriteData() added. It writes TPolyMarkers and TPolyLine3Ds to
// a file which can be used to display results (pictures) offline.
//
// Revision 1.6  2000/11/10 18:37:24  oldi
// It is now possible to display tracks which should be longer (so called 'short tracks').
//
// Revision 1.5  2000/07/28 09:45:42  hummler
// change "Stiostream.h" to <Stiostream.h> for correct path
//
// Revision 1.4  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
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

#include <Stiostream.h>

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
             TObjArray *mTrack;             // found tracks
             TObjArray *mHit;               // found hits
             TObjArray *mGeantTrack;        // geant tracks
             TObjArray *mGeantHit;          // geanthits
  
                 Int_t  mNumRowSegment;     // number of row segments
                 Int_t  mNumPhiSegment;     // number of phi segments
                 Int_t  mNumEtaSegment;     // number of eta segments
                 Int_t  mBounds;            // row * phi * eta segments

               TCanvas *mX_Y_Z;             // canvas for both Ftpcs
               TCanvas *mX_Y_Zplus;         // canvas for positive Ftpc
               TCanvas *mX_Y_Zminus;        // canvas for negative Ftpc

                 TNode *mNode0;             // node for both Ftpcs
                 TNode *mNode1;             // node for positive Ftpcs
                 TNode *mNode2;             // node for negative Ftpcs

           TPolyLine3D *geant_line;         // geant tracks
           TPolyLine3D *found_line;         // found tracks
           TPolyLine3D *current_line;       // pointer to track

         TPolyMarker3D *found_hit;          // cluster on track in both Ftpcs
         TPolyMarker3D *found_hit_plus;     // cluster on track in positive Ftpcs
         TPolyMarker3D *found_hit_minus;    // cluster on track in negative Ftpcs
         TPolyMarker3D *geant_hit;          // cluster on track in both Ftpcs
         TPolyMarker3D *geant_hit_plus;     // cluster on track in positive Ftpcs
         TPolyMarker3D *geant_hit_minus;    // cluster on track in negative Ftpcs
         TPolyMarker3D *unused_hit;         // unused cluster in both Ftpcs
         TPolyMarker3D *unused_hit_plus;    // unused cluster in positive Ftpcs
         TPolyMarker3D *unused_hit_minus;   // unused cluster in negative Ftpcs
         TPolyMarker3D *wrong_hit;          // wrong cluster on track in both Ftpcs
         TPolyMarker3D *wrong_hit_plus;     // wrong cluster on track in positive Ftpcs
         TPolyMarker3D *wrong_hit_minus;    // wrong cluster on track in negative Ftpcs

               Float_t *found_value;        //! found hit coordinates of both Ftpcs
               Float_t *found_value_plus;   //! found hit coordinates of positive Ftpc
               Float_t *found_value_minus;  //! found hit coordinates of negative Ftpc
               Float_t *geant_value;        //! geant hit coordinates of both Ftpcs
               Float_t *geant_value_plus;   //! geant hit coordinates of positive Ftpc
               Float_t *geant_value_minus;  //! geant hit coordinates of negative Ftpc
               Float_t *unused_value;       //! unused hit coordinates of both Ftpcs
               Float_t *unused_value_plus;  //! unused hit coordinates of positive Ftpc
               Float_t *unused_value_minus; //! unused hit coordinates of negative Ftpc
               Float_t *wrong_value;        //! wrong found hit coordinates of both Ftpcs
               Float_t *wrong_value_plus;   //! wrong found hit coordinates of positive Ftpc
               Float_t *wrong_value_minus;  //! wrong found hit coordinates of negative Ftpc

public:
            StFtpcDisplay();                                    // default constructor
            StFtpcDisplay(TObjArray *hits, TObjArray *tracks);  // usual constructor 
            StFtpcDisplay(TObjArray *hits, TObjArray *tracks, TObjArray *geanthits, TObjArray *geanttracks);  // constructor for evaluator 
  virtual  ~StFtpcDisplay();                                    // destructor

  //void  Info();                                                                              // prints general information
  void  WriteData(Char_t *filename);                                                           // writes clusters and tracks to file
  void  TrackInfo();                                                                           // plots single tracks
  void  ShowClusters();                                                                        // displays clusters
  void  ShowTracks(Int_t trackanz = 0, Int_t trackarray[] = 0);                               // displays tracks
  void  ShowEvalTracks(MIntArray *splitArr = 0, MIntArray *uncleanArr = 0, MIntArray *clusterArr = 0);   // displays evaluated tracks
  void  FillGeant(Bool_t electrons, Bool_t non_vtx, Bool_t good, Bool_t geant_hits, 
		  Float_t eta_low, Float_t eta_up, Float_t pt_low, Float_t pt_up, Bool_t blue);// fills geant histograms
  void  FillFound(Bool_t good_found, Bool_t st, MIntArray *split, MIntArray *unclean, MIntArray *found_hits, 
		  Float_t eta_low, Float_t eta_up, Float_t pt_low, Float_t pt_up);             // fills found histograms
  void  DrawNodes();                                                                           // draw nodes
  void  DeleteAll();                                                                              // deletes objects of found and geant tracks
  void  DeleteFound();                                                                         // deletes objects of found tracks
  void  DeleteGeant();                                                                         // deletes objects of geant tracks
  void  OnOff(Bool_t on);                                                                      // prints 'On' or 'Off'

  ClassDef(StFtpcDisplay, 1)  // Ftpc display class
};

 

#endif
