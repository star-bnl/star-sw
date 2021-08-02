//Author: Sergey Panitkin
//Date:   2001

/////////////////////////////////////////////////////////////////
//  HistoHandler for the STAR EVP GUI
/////////////////////////////////////////////////////////////////
//
//
//

#ifndef HIST0HANDLER_H
#define HIST0HANDLER_H


#include "TH1.h"
#include "TH2.h"
#include "TH2.h"

#include <TCanvas.h>
#include <TFile.h>
#include <TMapFile.h>
// includes for string parsing
#include <TString.h>
#include <TObjString.h>
#include <TObjArray.h>
#include <TRegexp.h>
#include <string>
#include <fstream>
#include <iostream>
#include <sstream>

#include "OTH.h"
#ifdef NEW_DAQ_READER
  class daqReader;
  typedef daqReader evpReader;
  typedef unsigned int UINT32;
#else
  class evpReader   ; // new  2007 DAQ file reader
#endif

#include "StReadLaserEvent.h"


// ***********************************************************************
class HistoHandler
{

  //friend void writeVertex(HistoHandler* mHistoHandler, const int histoNum);
  //friend class TestMainFrame;
  //friend class EvpServer;
 public:
  static char* mListOfHistograms;

 private:
  // MAXIMUM Number of histograms
  static const int MAX_NHIST_1 = 512;
  static const int MAX_NHIST_2 = 512;
  OTH* oth;
  TH1  *h1[MAX_NHIST_1];
  

  int  mDebugLevel;

  int NHIST_1; // number of booked 1d histogramms
  int NHIST_2; // number of booked 2d histogramms

  float mPhiAngleMap[24][45][182];

  TMapFile*&  mFile;

  int ParseString (const TString &tChain, TObjArray &Opt);
  
  // For laser events
  StReadLaserEvent *mLaser;

 public:
  HistoHandler(TMapFile*&);
  virtual ~HistoHandler();

  
  void SetDefaults(void);
  //void BookOld(void);
  void Book();
  void Reset(void);
  int Save(const char* filenname);
  void MakePS(void);
  void Print(void);
  void  SetPhiAngleMap();
  void SetHistoListFile(char *lHistoListFile);
  char *GetHistoListFile(void){return mListOfHistograms;}
  void SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
  int  GetDebugLevel(void){return mDebugLevel;}
  static char* l3Buffer() { return mL3Buffer; }
  int fillHistos( evpReader* evp, char* mem) {
    return fill(evp, mem, mPhiAngleMap);
  }
  int fill(evpReader* evp, char* mem, float mPhiAngleMap[24][45][182]);
  void writeVertex(const int histoNum);
  void CopyMapFile(void);

 private:
  double mZdcVertex;
  double mZdcTimeDiff;
  double mBbcVertex;
  double mBbcTimeDiff;
  static char* mL3Buffer;
};
//************************************************************************
#endif











/***************************************************************************
 *
 * $Id: HistoHandler.h,v 1.2 2009/02/17 19:24:56 dkettler Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: HistoHandler.h,v $
 * Revision 1.2  2009/02/17 19:24:56  dkettler
 * Fixed ZDC/BBC-l3 vertex plots
 *
 * Revision 1.1  2009/01/23 16:10:55  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.8  2009/01/21 01:24:17  genevb
 * EMC updates (from O. Grebenyuk)
 *
 * Revision 1.7  2009/01/08 19:39:28  fine
 * fix the bemcFillHisto function signature  HistoHandler.cxx
 *
 * Revision 1.6  2008/12/18 18:10:03  fine
 * adjust for the newest DAQ_READER
 *
 * Revision 1.5  2008/02/15 18:51:53  dkettler
 * Updates to laser and TOF reader
 *
 * Revision 1.4  2007/04/03 13:19:33  laue
 * Some minor modification on laser histograms by request from Blair
 *
 * Revision 1.3  2007/03/26 16:16:37  laue
 * ZDC vertex recalibrated
 *
 * Revision 1.2  2007/02/27 18:16:01  laue
 * *** empty log message ***
 *
 * Revision 1.1  2007/02/27 15:23:37  laue
 * Initial version
 *
 * Revision 1.1  2006/10/27 17:43:29  laue
 * Resources folder added
 * histogram controll class OTH added
 *
 * Revision 1.1  2006/10/04 20:31:34  laue
 * Initial Version
 *
 *
 ***************************************************************************/
