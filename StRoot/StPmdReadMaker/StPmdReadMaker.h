/***************************************************************************
 *$Id: StPmdReadMaker.h,v 1.6 2007/08/31 10:50:39 rashmi Exp $
 *
 *  StPmdReadMaker
 *
 * Author: Supriya Das and Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: Pmd Data Reader to store hits in StEvent
 ***************************************************************************
 * $Log: StPmdReadMaker.h,v $
 * Revision 1.6  2007/08/31 10:50:39  rashmi
 * Added routines to read badchains,HotCells,Cell_GNF,SMChain_GNF
 *
 * Revision 1.5  2004/07/12 14:45:16  subhasis
 * QA hist added
 *
 * Revision 1.4  2004/03/23 08:52:03  subhasis
 * several changes (Board Detail by hand etc) for first production
 *
 * Revision 1.3  2004/03/11 11:29:41  subhasis
 * Changes made for PMD run config
 *
 * Revision 1.2  2003/12/03 11:52:38  subhasis
 * Comment header changed by Supriya
 *
 ***************************************************************************/
#ifdef __ROOT__
#ifndef STAR_StPmdReadMaker
#define STAR_StPmdReadMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
// DAQ Libraries
//#include "StDaqLib/PMD/PMD_Reader.hh"
class StPhmdCollection;
class StPhmdHit;
class StPhmdDetector;
class StPmdCollection;
class StPmdHit;
class StPmdDetector;

#ifndef __CINT__
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
#endif

#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdDBUtil.h"
#include "tables/St_pmdBrdMipCalib_Table.h"
#include "tables/St_pmdCalSummary_Table.h"
#include "tables/St_pmdSMCalib_Table.h"
#include "tables/St_pmdSMChain_GNF_Table.h"

#include  <TH1.h>
#include  <TH2.h>

#include "tables/St_pmdHotCells_Table.h"
class StDAQReader;
class StPMDReader;
//SP
class EventReader;
class StPmdGeom;
class StPmdDBUtil;

class StPmdReadMaker : public StMaker {
  
  
 public: 
  StPmdReadMaker(const char *name="pmdReader");  // Constructor
  virtual       ~StPmdReadMaker();               //Destructor
  virtual Int_t  Init(); 	  //Initialization
  virtual Int_t  InitRun(Int_t runnr);          // Init for every run to read DB
  virtual Int_t  Make();			// Make
  Int_t  fillStEvent(StPmdDetector*, StPmdDetector*);  // Fills StEvent
  virtual Int_t  Finish();                         // Finish
  void SetPmdPrint(Bool_t);			   //Set print flag
  void SetCalibFlag(Bool_t);			   //Set calib flag
  void SetChainThreshold(Float_t);		   //Set ADC Threshold
  void bookHist();
  Int_t GetCalib(int,int,int,float&);  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StPmdReadMaker.h,v 1.6 2007/08/31 10:50:39 rashmi Exp $ built "__DATE__" "__TIME__ ;
    return cvs;
  }
  
 protected:
 TH1F * m_event_tothit_pmd;
 TH1F * m_event_tothit_cpv;
 TH2F* m_chain_channel;

 private:
  StDAQReader*           mTheDataReader;//!
  StPMDReader* mThePmdReader;//!
  St_DataSet*            mThePmdData;//!
  
  // These give the hot channels
  St_pmdHotCells* mHotCells;
  Bool_t IsHot(Int_t chain, Int_t channel);
  Bool_t Accept(Int_t chain, Int_t channel);

  Int_t mRunNumber;
  Int_t mVmeCond;
  //  
  StPhmdCollection *        mEvtPmdCollection;   
  StPhmdDetector* mPmdEvent;  
  StPhmdDetector* mCpvEvent; 
  StPmdCollection * mPmdCollection;  
  TDataSet          *mDb;
  Bool_t mPmdPrint;
  Bool_t mCalibFlag;
  Float_t mChainTh;
  StPmdGeom* mPmdGeom;
  StPmdDBUtil* mPmdDBUtil;
  Int_t mHotTracks;
  
  //calib arrays 
  pmdSMCalib_st* m_PmdCalibConst;
 
  Int_t ApplyMapping(int*);// Reads raw data from DAQReader and applies mapping
  Bool_t ReadCalibrationsConst(); // Reads calibration constant from DB
  void ReadBadChains(int);//Read Bad Chain information from StPmdCleanConstants
  
  ClassDef(StPmdReadMaker, 1)   
    };
inline void  StPmdReadMaker::SetPmdPrint(Bool_t var) {mPmdPrint = var;}
inline void  StPmdReadMaker::SetCalibFlag(Bool_t var) {mCalibFlag = var;}
inline void  StPmdReadMaker::SetChainThreshold(Float_t var) {mChainTh = var;}
#endif 
#endif 
