#ifndef ServerGui_h
#define ServerGui_h

#include <iostream>
#include <stdlib.h>
using namespace std;

#include <Rtypes.h>
#include <TROOT.h>
#include <TApplication.h>
//#include <TVirtualX.h>
#include <TSystem.h>

#include <TGFrame.h>
#include <TGButton.h>
#include <TGTextEntry.h>

#include <TGListBox.h>
#include <TGLabel.h>
#include <TGString.h>

#include <TGMenu.h>
#include <RQ_OBJECT.h>
#include <TGFileDialog.h>

#include <TGMsgBox.h>
#include <TGClient.h>


class HistoHandler;
class EvpServer;


enum ETestCommandIdentifiers {
  M_FILE_OPEN,
  M_FILE_SAVE,
  M_FILE_SAVEAS,
  M_FILE_PRINT,
  M_FILE_PRINTSETUP,
  M_FILE_CLOSE,
  M_FILE_EXIT,


  M_HELP_CONTENTS,
  M_HELP_SEARCH,
  M_HELP_ABOUT,

  M_CASCADE_1,
  M_CASCADE_2,
  M_CASCADE_3,

  VId1,
  HId1,
  VId2,
  HId2,

  VSId1,
  HSId1,
  VSId2,
  HSId2
};


//************************************************************************
class ServerGui : public TObject
{

    RQ_OBJECT()

private:
    
    TGMainFrame        *fMain;

    TGCompositeFrame   *fMain1;

    TGVerticalFrame    *fConnectionFrame;
    TGVerticalFrame    *fControlFrame;

    TGHorizontalFrame  *fRunFrame;
    TGHorizontalFrame  *fHistoFrame;
    TGHorizontalFrame  *fMessagesFrame;

    TGVerticalFrame    *fRunLoopFrame;
    TGVerticalFrame    *fRunInfoFrame;


    //  TGListBox          *fDir;


    TGCompositeFrame   *fRunIDFrame;
    TGLabel            *fRunLabel;
    TGTextEntry        *fRun;
    TGTextBuffer       *mRun;

    TGCompositeFrame   *fEventCountFrame;
    TGLabel            *fEventCountLabel;
    TGTextEntry        *fEventCount;
    TGTextBuffer       *mEventCount;

    TGCompositeFrame   *fEventNumberFrame;
    TGLabel            *fEventNumberLabel;
    TGTextEntry        *fEventNumber;
    TGTextBuffer       *mEventNumber;

    TGCompositeFrame   *fTokenNumberFrame;
    TGLabel            *fTokenNumberLabel;
    TGTextEntry        *fTokenNumber;
    TGTextBuffer       *mTokenNumber;


    TGCompositeFrame   *fSourceFileFrame;
    TGLabel            *fSourceFileLabel;
    TGTextEntry        *fSourceFile;
    TGTextBuffer       *mSourceFileName;

    TGCompositeFrame   *fRunStatusFrame;
    TGLabel            *fRunStatusLabel;
    TGTextEntry        *fRunStatus;
    TGTextBuffer       *mRunStatusName;


    TGHorizontalFrame  *fButtonFrame;
    TGButton           *fStartButton;
    TGButton           *fStopButton;
    TGButton           *fNextButton;
    TGButton           *fLiveButton;
    TGButton           *fFileButton;

    TGVerticalFrame    *fHistoControlFrame;
    TGVerticalFrame    *fHistoInfoFrame;

    TGGroupFrame       *fDummyConnectionFrame;
    TGGroupFrame       *fDummyRunLoopFrame;
    TGGroupFrame       *fDummyHistoFrame;
    TGGroupFrame       *fDummyRunInfoFrame;

    TGCompositeFrame   *fHistoFileFrame;
    TGLabel            *fHistoFileLabel;
    TGTextEntry        *fHistoFile;
    TGTextBuffer       *mHistoOutFileName;


    TGHorizontalFrame  *fHistoButtonFrame;
    TGButton           *fHistoResetButton;
    TGButton           *fHistoSaveButton;
    TGButton           *fHistoSavePSButton;
    TGButton           *fHistoPrintButton;
    TGButton           *fHistoViewButton;

    TGPictureButton    *fStarLogo;

    //    TGLabel            *fStatusLabel;
    //    TGLabel            *fHistoStatusLabel;


    TGMenuBar          *fMenuBar;
    TGPopupMenu        *fMenuFile,  *fMenuHelp;

    TGLayoutHints      *fL0;
    TGLayoutHints      *fL1, *fL101, *fL102;
    TGLayoutHints      *fL2, *fL3;
    TGLayoutHints      *fL200, *fL201, *fL203, *fL204;
    TGLayoutHints      *fLDir;
    TGLayoutHints      *fButtonFrameLayout;
    TGLayoutHints      *fL5, *fL6;


    TList              *fCleanUp;


    EvpServer          *mEvpServer;
    int                 mDebugLevel;
public:
    Pixel_t green;
    Pixel_t blue;
    Pixel_t grey;
    Pixel_t red;
    Pixel_t black;


    ServerGui(const TGWindow *p, UInt_t w, UInt_t h, EvpServer*);
    virtual ~ServerGui();
    void SetTarget(const char* name);
    void SetServer(EvpServer* server) { mEvpServer = server; }
        // slots
    void CloseWindow(void);
    void DoStartButton(void);
    void DoStopButton(void);
    void DoNextButton(void);
    void DoLiveButton(void);
    void DoFileButton(void);
    void DoHistoResetButton(void);
    void DoHistoSaveButton(void);
    void DoHistoSavePSButton(void);
    void DoHistoPrintButton(void);
    void DoHistoViewButton(void);

    void HandleMenu(Int_t id);
    //  void SetDefaults(void);
    void SetDebugLevel(int );
    int  GetDebugLevel(void);

    void UpdateRunInfo(int);
    void UpdateEventCountInfo(int);
    void UpdateEventNumberInfo(int);
    void UpdateTokenNumberInfo(int);
    void UpdateRootFileInfo(char *);

    void ShowNext(bool live = true);
    void ShowLiveRun(bool live = true);
    void ShowRunning(bool run = true);
    void ShowFileRun(bool run = true);
    void ShowButton(TGButton* b, Pixel_t color);

    void ShowStatus(char *);

    void SetEnabled(bool b = true);

    ClassDef(ServerGui,0 ) ;
};
//************************************************************************

#endif







/***************************************************************************
 *
 * $Id: ServerGui.h,v 1.1 2009/01/23 16:10:58 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: ServerGui.h,v $
 * Revision 1.1  2009/01/23 16:10:58  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2007/02/27 15:23:39  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

