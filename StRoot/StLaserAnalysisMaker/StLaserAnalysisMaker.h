#ifndef STAR_StLaserAnalysisMaker
#define STAR_StLaserAnalysisMaker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StLaserAnalysisMaker virtual base class for Maker                            //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
// Removed all CVS stuff
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TClonesArray.h"
class StDAQReader;
class StTPCReader;
class St_tpg_pad_plane;
class St_tpg_detector;
class TTree;
class LEvent;
class StLaserAnalysisMaker : public StMaker {
 private:
  Int_t             fNPrediction;   //!
  LEvent           *fevent;         //!
  TClonesArray     *fPredictions;   //!
  StDAQReader      *fDAQReader;     //!
  StTPCReader      *fTPCReader;     //!
  St_tpg_pad_plane *ftpg_pad_plane; //!
  St_tpg_detector  *ftpg_detector;  //!
  TTree            *fTree;          //!
 protected:
 public: 
                  StLaserAnalysisMaker(const char *name="LaserAnalysis");
   virtual       ~StLaserAnalysisMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual TClonesArray *GetPredictions() const { return fPredictions; }
   virtual Int_t OpenDAQ();
   ClassDef(StLaserAnalysisMaker, 1)   //StF Achain virtual base class for Makers
};

#endif
