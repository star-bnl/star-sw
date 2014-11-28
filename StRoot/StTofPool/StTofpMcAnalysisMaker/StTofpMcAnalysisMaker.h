/**********************************************
 *
 * $Id: StTofpMcAnalysisMaker.h,v 1.3 2014/08/06 11:43:47 jeromel Exp $
 * $Log: StTofpMcAnalysisMaker.h,v $
 * Revision 1.3  2014/08/06 11:43:47  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.2  2011/04/03 15:52:57  fisyak
 * Fix effect of constness in StAssociationMaker
 *
 * Revision 1.1  2004/03/16 04:58:54  geurts
 * *** empty log message ***
 *
 * Revision 1.7  2003/09/10 19:47:22  perev
 * ansi corrs
 *
 **********************************************/

#ifndef StTofpMcAnalysisMaker_HH
#define StTofpMcAnalysisMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

//  - if not using the methods of the class, then we can just put class TCanvas;
//   -  however, if we are using the methods of TCanvas, then #include "TCanvas.h"

class TH1F;
class TH2F;
class TH2D;
class TH1D;
class TFile;
class TNtuple;
class StTofGeometry;
class StTrackGeometry;
class StTrack;


class StTofpMcAnalysisMaker : public StMaker {

 public:
  StTofpMcAnalysisMaker(const char* name = "StTofpMcAnalysisMaker",
			const char* title = "event/StTofpMcAnalysisMaker");
  virtual ~StTofpMcAnalysisMaker();
  virtual void  Clear(const char* opt="");
  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual Int_t FinishRun(int);
  virtual Int_t Make();
  virtual Int_t Finish();

  // Embedding/Reconstruction/Matching histograms
  TH2F*     hMcPionPlus;         //! montecarlo pi+
  TH2F*     hMcPionMin;          //! montecarlo pi-
  TH2F*     hMcKaonPlus;         //! montecarlo K+
  TH2F*     hMcKaonMin;          //! montecarlo K-
  TH2F*     hMcProton;           //! montecarlo p+
  TH2F*     hMcAntiProton;       //! montecarlo pbar
  TH2F*     hMcElectron;         //! montecarlo e-  
  TH2F*     hRcPionPlus;   //! TPC reconstructed pi+  
  TH2F*     hRcPionMin;    //! TPC reconstructed pi-  
  TH2F*     hRcKaonPlus;   //! TPC reconstructed K+   
  TH2F*     hRcKaonMin;    //! TPC reconstructed K-   
  TH2F*     hRcProton;     //! TPC reconstructed p+   
  TH2F*     hRcAntiProton; //! TPC reconstructed pbar 
  TH2F*     hRcElectron;   //! TPC reconstructed e-   
  TH2F*     hMatchPionPlus;   //! TOF matched pi+  
  TH2F*     hMatchPionMin;    //! TOF matched pi-  
  TH2F*     hMatchKaonPlus;   //! TOF matched K+   
  TH2F*     hMatchKaonMin;    //! TOF matched K-   
  TH2F*     hMatchProton;     //! TOF matched p+   
  TH2F*     hMatchAntiProton; //! TOF matched pbar 
  TH2F*     hMatchElectron;   //! TOF matched e-   

  // Embedding QA histograms
  TH2F*     hMomResPtPion;    //! MC/RC momentum resolution
  TH1F*     hMultRef;         //! multiplicity reference

  // Match QA histograms
  TH2D *hTofpHitMap1;  //! 2D tray hit positions (B)
  TH2D *hTofpHitMap2;  //! 2D tray hit positions (pre-D)
  TH2D *hTofpHitMap3;  //! 2D tray hit positions (D)
  TH2D *hTofpHitMap4;  //! 2D tray hit positions (F)
  TH1D *hTofpSlatIdA0; //! events per slat
  TH1D *hTofpSlatIdA1; //! valid slat
  TH1D *hTofpSlatIdB1; //! #tracks match  valid slat
  TH1D *hTofpSlatIdD1; //! track match per valid slat
  TH1D *hTofpSlatIdD2; //! single track match per slat
  TH1D *hTofpSlatIdE1; //! one slat for one track match 
  TH1D *hTofpSlatIdE2; //! recovered from hitprof-weight
  TH1D *hTofpSlatIdE3; //! recovered from ss
  TH1D *hTofpSlatIdE4; //! recovered from closest hitplane
  TH1D *hTofpSlatIdE5; //! total recovered slat per track match
  TH1D *hTofpSlatIdF1; //! primary track match per slat
  TH1D *hTofpSlatHitVecSize;     //! slat mult per StTrack 

  StTofGeometry *mTofGeom; //!

private:
  bool             mOuterTrackGeometry; //!
  const StTrackGeometry* trackGeometry(const StTrack*);//!
  unsigned int     mMinHitsPerTrack;
  bool             validTrack(StTrack*);
  bool             validTofTrack(StTrack*);
    
  //! Histograms booking constants
  static const Int_t mPtBin;
  static const Int_t  mYBin;
  static const Float_t mPtMin;
  static const Float_t mPtMax;
  static const Float_t mYMin;
  static const Float_t mYMax;


  virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StTofpMcAnalysisMaker.h,v 1.3 2014/08/06 11:43:47 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}	
    
  ClassDef(StTofpMcAnalysisMaker,0)
};
#endif
