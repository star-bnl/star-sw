#ifndef EndOfRunAction_h
#define EndOfRunAction_h

#include <iostream>
#include <stdlib.h>
#include <stdio.h>

#include <Rtypes.h>
#include <TROOT.h>
#include <TApplication.h>
//#include <TVirtualX.h>
#include <TSystem.h>
#include <TStyle.h>


#include <TCanvas.h>
#include <TH1.h>
//#include <TROOT.h>
#include <TH2.h>
#include <TProfile.h>
#include <TRandom.h>

#include <TMapFile.h>

#include <TNamed.h>
#include <TPostScript.h>
#include <TPaveLabel.h>
#include <TDatime.h>
// includes for string parsing
#include <TString.h>
#include <TObjString.h>
#include <TObjArray.h>
#include <TRegexp.h>
#include <string>
#include <fstream>
#include <sstream>

#include "EvpUtil.h"
#include "RunStatus.h"

#include <rtsLog.h>

#include "GroupCollection.h"


using namespace std;


//************************************************************************
class EndOfRunAction  {

 private:

 GenericFile *mfile;
 RunStatus* mRS;
 TCanvas* mCanvas;
 GroupCollection mGroups;

 int mDebugLevel;
 const char* mDir;

 char mBaseName[1024];
 char mPsName[1024];
 char mPdfName[1024];

 bool mDb;
 bool mPs;
 bool mPdf;
 bool mRemove;

 bool mIsUploaded;

 public:

 EndOfRunAction(const char* file, const char* dir, TCanvas*);

 virtual ~EndOfRunAction();

 void  SetDb(bool b=true)     { mDb = b; }
 void  SetPs(bool b=true)     { mPs = b; }
 void  SetPdf(bool b=true)    { mPdf = b; }
 void  SetRemove(bool b=true) { mRemove = b; }
 void Cleanup();

 void Action();

 
 void MakeNames(void);
 void SetDebugLevel(int lDebugLevel){mDebugLevel = lDebugLevel;}
 void SetDefaults(void);
 int DisplayRunStatus();
 // void DisplayOneCanvas(const int, const int);
 //void WriteCurrentCanvasToPSFile(int tab, int subTab);
 int WriteFile(const char* ps);
 int MakePDFFromPSFile(const char* ps, const char* pdf);
 int  WriteFileToDB(const char*);
 private:
 string utoa(unsigned int t);

};
//************************************************************************

#endif







/***************************************************************************
 *
 * $Id: EndOfRunAction.h,v 1.1 2009/01/23 16:10:53 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EndOfRunAction.h,v $
 * Revision 1.1  2009/01/23 16:10:53  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.2  2007/03/21 17:11:45  laue
 * Moved individual HistogramGroups to their own folder
 * Modified Makefile and server to allow profiling with gprof (i.e. must not exot with exit(...) function)
 *
 * Revision 1.1  2007/02/27 15:23:37  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

