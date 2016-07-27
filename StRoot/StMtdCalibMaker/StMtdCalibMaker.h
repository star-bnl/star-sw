#ifndef STMTDCALIMAKER_H
#define STMTDCALIMAKER_H

/***************************************************************************
 *
 * $Id: StMtdCalibMaker.h,v 1.3 2016/07/27 15:17:19 marr Exp $ 
 * StMtdCalibMaker - class to in inplement Mtd related Calibration paraments
 * Author: Xinjie Huang
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/
#include "TMath.h"
#include "StMaker.h"

#include <string>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::string;
using std::vector;
#endif
class TH1F;
class TH1D;
class TH2F;
class TFile;
class StEvent;
class StMuDst;

class StMtdCalibMaker : public StMaker{
 public:
  StMtdCalibMaker(const char* name="mtdCalib");   /// Default constructor
  virtual ~StMtdCalibMaker();                     /// Destructor
    
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t);
  virtual Int_t Make();
  
  void setSlewingCorr(Bool_t val=kTRUE);          /// switch to turn on slewing correction or not - maybe only T0 in the first step.
  void setDebug(Bool_t val=kFALSE);               /// switch to run in debug mod
  void setUseTriggercut(Bool_t val=kTRUE);        ///switch to use trigger cut
  void setCreateHistoFlag(Bool_t histos=kTRUE);   /// enable QA histogram filling
  void setInitFromFile(Bool_t val=kTRUE);         /// set input source

  /// read calibration parameters from file
  void setCalibFileTot(const Char_t*);           
  void setCalibFileT0(const Char_t*);
  void setCalibFileDy(const Char_t*);
  void setCalibFileDz(const Char_t*);
  void setCalibFileTrigger(const Char_t*);

 protected:
  void processStEvent();
  void processMuDst();
  ///Caculate all the correction value for time of flight
  Double_t mtdAllCorr(const Double_t tot, const Int_t iBackleg, const Int_t iModule, const Int_t iCell);
  void bookHistograms();
      
 private:
  enum{
    mNBackleg = 30,    // 30 Backlegs
    mNModule = 5,      // 5 Modules for each Backlegs
    mNCell = 12,       // 12 cells per module
    mNBinMax = 20     // 20 bins for T-Tot correction and Triggercut
  };

  Double_t          mMtdT0Corr[mNBackleg][mNModule][mNCell];    // T0 offset 
  Double_t          mMtdTotCorr[mNBackleg][mNModule][mNBinMax]; // Slewing correction
  Double_t          mMtdTotEdge[mNBackleg][mNModule][mNBinMax]; // Slewing correction bin edges
  Double_t          mMtdDyCorr[mNBackleg][mNModule];    // dy correction for each module
  Double_t          mMtdDzCorr[mNBackleg][mNModule][mNCell];    // dz correction for each cell 
  Double_t          mTriggerHighEdge[mNBackleg][mNModule];      // Lower edge of trigger time cut
  Double_t          mTriggerLowEdge[mNBackleg][mNModule];       // Upper edge of trigger time cut

  StEvent*          mStEvent;
  StMuDst*          mMuDst;
  Bool_t            mDebug;            // switch to debug mod
  Bool_t            mHisto;            // switch to fill QA histograms
  Bool_t            mUseTriggercut;    // switch for use trigger cut
  Bool_t            mInitFromFile;     // switch for reading from files
  string            mCalibFileTot;     // filename for ToT calibration parameters    
  string            mCalibFileT0;      // filename for T0 calibration parameters
  string            mCalibFileDy;      // filename for dy position correction parameters
  string            mCalibFileDz;      // filename for dz position correction parameters
  string            mCalibFileTrigger; // filename for Triggercut calibration parameters

  TH1D              *hTimeOfFlightCorr;        // histo of TimeOfFlight before correction	
  TH1D              *hAllCorr;       	       // All correction distribution 		
  TH2F              *hTimeOfFlightModule;      // TimeOfFlight vs Module
  TH2F              *hTimeOfFlightCorrModule;  // TimeOfFlight correction vs Module 
  TH2F              *hTriggerTimeBL;           // TriggerTime vs backleg plot
  TH2F              *hVertexzVsTpcz;	       // Vertexz Vs Tpcz
  TH2F              *hTOFTimeOfFlightTray;     // TOF TimeOfFlight vs Tray
  TH2F              *hDyModule;      // Dy vs Module
  TH2F              *hDzModule;      // Dz vs Module

  virtual const Char_t *GetCVS() const 
  {
    static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
  }
    
  ClassDef(StMtdCalibMaker,0);
};
inline void StMtdCalibMaker::setUseTriggercut(const Bool_t val)            { mUseTriggercut = val;           }
inline void StMtdCalibMaker::setDebug(const Bool_t val)                    { mDebug = val;                   }
inline void StMtdCalibMaker::setCreateHistoFlag(Bool_t histos)             { mHisto = histos;                }
inline void StMtdCalibMaker::setInitFromFile(const Bool_t val)             { mInitFromFile = val;            }
inline void StMtdCalibMaker::setCalibFileTot(const Char_t* filename)       { mCalibFileTot = filename;       }
inline void StMtdCalibMaker::setCalibFileT0(const Char_t* filename)        { mCalibFileT0 = filename;        }
inline void StMtdCalibMaker::setCalibFileDy(const Char_t* filename)        { mCalibFileDy = filename;       }
inline void StMtdCalibMaker::setCalibFileDz(const Char_t* filename)        { mCalibFileDz = filename;        }
inline void StMtdCalibMaker::setCalibFileTrigger(const Char_t* filename)   { mCalibFileTrigger = filename;   }
#endif
