/***************************************************************************
 *$Id: StPmdReadMaker.h,v 1.2 2003/12/03 11:52:38 subhasis Exp $
 *
 *  StPmdReadMaker
 *
 * Author: Supriya Das and Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: Pmd Data Reader to store hits in Stevent
 ***************************************************************************
 * $Log: StPmdReadMaker.h,v $
 * Revision 1.2  2003/12/03 11:52:38  subhasis
 * Comment header changed by Supriya
 *
 */
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
  void SetPmdPrint(Bool_t);			//Set print flag
  virtual const char *GetCVS() const {
	    static const char cvs[]="Tag $Name:  $ $Id: StPmdReadMaker.h,v 1.2 2003/12/03 11:52:38 subhasis Exp $ built "__DATE__" "__TIME__ ;
	        return cvs;
		  }

 protected:

 private:
  StDAQReader*           mTheDataReader;//!
  StPMDReader* mThePmdReader;//!
  St_DataSet*            mThePmdData;//!
    
  StPhmdCollection *        mEvtPmdCollection;   
  StPhmdDetector* mPmdEvent;  
  StPhmdDetector* mCpvEvent; 
  StPmdCollection * mPmdCollection;  
  TDataSet          *mDb;
  Bool_t mPmdPrint;
  StPmdGeom* mPmdGeom;
  StPmdDBUtil* mPmdDBUtil;
  //calib arrays 
  pmdBrdMipCalib_st* m_PmdCalibConst;
//  Float_t m_MipPeak[PMD_CRAMS_CH_MAX][PMD_BOARD_CH_MAX];
  Int_t ApplyMapping(int*);// Reads raw data from DAQReader and applies mapping
  Bool_t ReadCalibrationsConst(); // Gets calibrations constant from DB

  ClassDef(StPmdReadMaker, 1)   
    };
    inline void  StPmdReadMaker::SetPmdPrint(Bool_t var) {mPmdPrint = var;}
#endif 
#endif 
