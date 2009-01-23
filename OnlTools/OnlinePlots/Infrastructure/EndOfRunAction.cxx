///  (TH1 *)mfile->Get(myNames[i][j][0].Data(), h);
///  (TH1 *)mfile->Get(myNames[tab][subTab][i].Data());

#include "EndOfRunAction.h"
#include "EvpUtil.h"
#include "RunStatus.h"


#include "DGHelp.h"
#include "PGMessage.h"
#include "TEnv.h" //test?
#include "TCanvas.h"
#include "TLatex.h"


//------------------------------------------------------------------------
EndOfRunAction::EndOfRunAction(const char* filename, const char* dir, TCanvas* gcc) 
  : mDb(true), mPs(true), mPdf(true), mRemove(true) {

  rtsLogLevel(NOTE);
  rtsLogAddDest("172.16.0.1",8002);
  rtsLogOutput(RTS_LOG_NET);
  LOG(INFO,"Online plots: Performing end of run action on file %s",filename);
  
  if ( strstr(filename,".map") ) {
    TMapFile* file = TMapFile::Create(filename);
    if ( file ) mfile = new GenericFile(file);
  } 
  if ( strstr(filename,".root") ) {
    TFile* file = new TFile(filename);
    mfile = new GenericFile(file);
  }

  if ( !mfile ) {
    cerr << " ### error ### can not open map file " << filename << endl;
    exit(-1);
  }

  mRS = (RunStatus*) mfile->Get("RunStatus");
  if ( !mRS ) {
    cerr << " ### error ### can not int RunStatus " << endl;
    //exit(-1);
  }

  mGroups.read( mfile );
  mGroups.list();

  cout << " performing end of run action " << endl;
  cout << " performing end of run action for file  " << filename << endl;
  cout << " performing end of run action for run   " << mRS->getRunNumber() << endl;


  mDir = EvpUtil::GetOutputPath();
  
  mDebugLevel=0;
  mCanvas = gcc;
  mIsUploaded = false;

  char title[1024];
  sprintf(title,"run%d",mRS->getRunNumber());
  mCanvas->SetTitle(title);
  
  EvpUtil::ReadCanvasDefinitions();
}

//----------------------------------------------------------------
EndOfRunAction::~EndOfRunAction() {
  delete mRS;
  //delete mCanvas;
}


//-------------------------------------------------------------------
void EndOfRunAction::MakeNames() {
  sprintf(mBaseName,"%d",            mRS->getRunNumber() );
  sprintf(mPsName,"%s/%d.ps",  mDir, mRS->getRunNumber() );
  sprintf(mPdfName,"%s/%d.pdf",mDir, mRS->getRunNumber() );
}
#include <bitset>
//-----------------------------------------------------------------------
int  EndOfRunAction::DisplayRunStatus() {
  //if ( !mCanvas ) return 1;
  //if ( !mRS ) return 1;
  char txt[1024];
  TLatex* tl; 
  // run number 
  sprintf(txt,"Run: #%d",mRS->getRunNumber() );
  tl = new TLatex(0.2,.90,txt); tl->SetTextSize(0.05); tl->Draw();
  // trigger bits
  sprintf(txt,"TriggerBits:  %s", utoa(mRS->getTriggerBitsRun()).c_str() );
  tl = new TLatex(0.1,.85,txt); tl->SetTextSize(0.03); tl->Draw();
  // detector bits
  sprintf(txt,"DetectorBits: %s", utoa(mRS->getDetectorBitsRun()).c_str() );
  tl = new TLatex(0.1,.80,txt); tl->SetTextSize(0.03); tl->Draw();
  cout << __PRETTY_FUNCTION__ << endl;
  return 0;
}
//-----------------------------------------------------------------------
string EndOfRunAction::utoa(unsigned int t) {
  ostringstream os;
  bitset<8> bit;
  unsigned char*  p = (unsigned char*)&t;
  for (int i=sizeof(t)-1; i>=0; i--) {
    bit = *(p+i); 
    os << bit << " ";
  } 
  return os.str();
}
//-----------------------------------------------------------------------
int  EndOfRunAction::WriteFile(const char* ps)
{

  if ( mDebugLevel ) {
    cout << __PRETTY_FUNCTION__ << endl;
  }
  //int type = 111; 
  //portrait ps
  //int type =112; //landscape ps
  //int type =113; //eps

  //char name[1024] = "Saving Histoes in PS File";
  //int canvasWidth  = EvpUtil::mCanvasWidth;
  //int canvasHeight = EvpUtil::mCanvasHeight;

  mCanvas->cd();

  char openPs[1024]; 
  char printPs[1024];
  char closePs[1024];
  sprintf(openPs,"%s[",ps);
  sprintf(printPs,"%s",ps);
  sprintf(closePs,"%s]",ps);

  mCanvas->Print(openPs);
  // plot RunStatus
  DisplayRunStatus();
  mCanvas->Print(printPs);
  // plots standard canvases
  for(int i =0; i<MAX_TABS ; i++) {
    for(int j =1; j<MAX_SUBTABS;j++) {
	if(EvpUtil::nHist[i][j]>0 && EvpUtil::nHist[i][j]<MAX_PADS) {
	  if ( (mRS->getDetectorBitsRun()&EvpUtil::canvasDetectorBits[i][j])==0 ) continue; 
	  if ( (mRS->getTriggerBitsRun()&EvpUtil::canvasTriggerBits[i][j])  ==0 ) continue;
	  EvpUtil::DisplayOneCanvas(mfile,mCanvas,i,j,true);
	  mCanvas->Print(printPs);
	}// well defined canvas
    }  // j - subtab
  }    // i - tab
  // now write groups
  mGroups.print(mCanvas,printPs);
  mCanvas->Print(closePs);
  if ( mDebugLevel ) {
    cout << __PRETTY_FUNCTION__ << endl;
  }
  return 0;
}
//--------------------------------------------------------------
int  EndOfRunAction::MakePDFFromPSFile(const char* ps, const char* pdf) {
  if ( mDebugLevel ) {
    cout << __PRETTY_FUNCTION__ << endl;
  }

  char cmd[1024];
  sprintf(cmd,"ps2pdf %s %s",ps, pdf);
  int iret = gSystem->Exec(cmd);
  if ( iret ) {
    LOG(ERR,"Could not create pdf file %s for %s ",pdf,ps);
  }
  return iret;
} 
//-----------------------------------------------------------------------
int  EndOfRunAction::WriteFileToDB(const char* basename ) {
  if ( mDebugLevel ) {
    cout << __PRETTY_FUNCTION__ << endl;
  }

  TString Message = "Write PDF to Data Base Error: ";

  char cmd[1024];
  sprintf(cmd,"/a/histos/storeOnlQAHistosToOnlDB -f %s -r %d -e pdf -d %s",basename,mRS->getRunNumber(),mDir);

  //Commented out just for the test. Uncomment when finished
  int iret = gSystem->Exec(cmd);

  cout << __PRETTY_FUNCTION__ << " return code " << iret << endl; 
  switch (iret) {
  case 0: LOG(OPER,"Online plots: saving to db successfull:  %s",mPdfName); break;
  case 256: LOG(CAUTION,"Online plots: saving to db failed : doublicate db entry :  %s",mPdfName); break;
  default: LOG(CAUTION,"Online plots: unknown return code:  %s",cmd); break;
  }

  //Codes used by Jeff as of 02/05/03
  //  0 => Success
  //  1 => DB Connection failed
  //  2 => Incomplete arguments
  //  3 => file specified not found or empty
  //  4 => Write to DB Failed

  return iret;
}


//---------------------------------------------------------------
void EndOfRunAction::Action() {
  MakeNames();

  WriteFile(mPsName);
  //WriteFile(mPdfName);

  if ( mPdf) {
    MakePDFFromPSFile(mPsName, mPdfName);
  }

  if ( mDb ) {
    WriteFileToDB(mBaseName);  // for nor ignore name
  }
  
  Cleanup();
}

void EndOfRunAction::Cleanup() {
  mCanvas->Close();
}





/***************************************************************************
 *
 * $Id: EndOfRunAction.cxx,v 1.1 2009/01/23 16:11:01 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EndOfRunAction.cxx,v $
 * Revision 1.1  2009/01/23 16:11:01  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.6  2009/01/21 01:24:16  genevb
 * EMC updates (from O. Grebenyuk)
 *
 * Revision 1.5  2007/04/25 17:52:29  laue
 * Minor updates
 *
 * Revision 1.4  2007/03/26 16:16:37  laue
 * ZDC vertex recalibrated
 *
 * Revision 1.3  2007/03/21 17:11:45  laue
 * Moved individual HistogramGroups to their own folder
 * Modified Makefile and server to allow profiling with gprof (i.e. must not exot with exit(...) function)
 *
 * Revision 1.2  2007/03/01 20:40:34  laue
 * Updates to print active HistogramGroups into ps/pdf files
 *
 * Revision 1.1  2007/02/27 15:23:36  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

