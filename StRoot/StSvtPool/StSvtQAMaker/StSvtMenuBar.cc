/***************************************************************************
 *
 * $Id: StSvtMenuBar.cc,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT GUI Menu Bar
 *
 ***************************************************************************
 *
 * $Log: StSvtMenuBar.cc,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#include "StSvtMenuBar.hh"
#include "StSvtClassLibrary/StSvtEnumerations.hh"
#include "StSvtMonitor.hh"
#include "StSvtGuiMonitor.hh"
#include "StSvtView.hh"
#include "StSvtHist.hh"
#include "StSvtGraph.hh"
#include "StSvtHybridGraph.hh"
#include "StDAQMaker/StDAQMaker.h"
#include "StSvtDaqMaker/StSvtDaqMaker.h"
#include "StSvtCalibMaker/StSvtPedMaker.h"
#include "StSvtCalibMaker/StSvtBadAnodesMaker.h"
#include "StSvtSeqAdjMaker/StSvtSeqAdjMaker.h"
#include "StSvtClusterMaker/StSvtClusterMaker.h"
#include "StSvtClusterMaker/StSvtClusterAnalysisMaker.h"
#include "StEventMaker/StEventMaker.h"
#include "StSvtQAMaker.h"
#include "InputDialog.hh"
#include "InputDialog2.hh"
#include "tables/St_scs_spt_Table.h"

#include "StChain.h"
//bfc stuff
#include "StBFChain.h"
//#include "StEventDisplayMaker/StEventDisplayMaker.h"
#include "StTrackFilter.h"
#include "StIOMaker/StIOMaker.h"
 
#include <TGraph.h>
#include <TH2.h>
#include <TGFrame.h>
#include <TROOT.h>
#include <TApplication.h>
#include <TVirtualX.h>
#include <TGFileDialog.h>
#include <TCanvas.h>
#include <string.h>
 
//--- Utility Functions --------------------------------------------------------
 
char *GetStringDialog(const char *prompt, const char *defval)
{
   // Prompt for string. The typed in string is returned.
 
   static char answer[128];
 
   new InputDialog(prompt, defval, answer);
 
   return answer;
}
 
Int_t GetIntegerDialog(const char *prompt, Int_t defval)
{
   // Prompt for integer. The typed in integer is returned.
 
   static char answer[32];
 
   char defv[32];
   sprintf(defv, "%d", defval);
 
   new InputDialog(prompt, defv, answer);
 
   return atoi(answer);
}
 
Float_t GetFloatDialog(const char *prompt, Float_t defval)
{
   // Prompt for float. The typed in float is returned.
 
   static char answer[32];
 
   char defv[32];
   sprintf(defv, "%f", defval);
 
   new InputDialog(prompt, defv, answer);
 
   return atof(answer);
}

Int_t GetIntegerRange(char* prompt, char* prompt2, Int_t defval, Int_t defval2)
{
   // Prompt for integer. The typed in integer is returned.
 
   static char answer[32], answer2[32];
  
   char defv[32];
   sprintf(defv, "%d", defval);
   char defv2[32];
   sprintf(defv2, "%d", defval2);
 
   new InputDialog2(prompt, prompt2, defv, defv2, answer, answer2);
 
   int output = 10000*atoi(answer) + atoi(answer2);
   return output;
}
 
Float_t GetFloatRange(char* prompt, char* prompt2, Float_t defval, Float_t defval2)
{
   // Prompt for integer. The typed in integer is returned.
 
   static char answer[32], answer2[32];
  
   char defv[32];
   sprintf(defv, "%f", defval);
   char defv2[32];
   sprintf(defv2, "%f", defval2);
 
   new InputDialog2(prompt, prompt2, defv, defv2, answer, answer2);
 
   float output = 10000*atof(answer) + atof(answer2);
   return output;
}
 
//_____________________________________________________________________
void bfc(StChain *chain, int firstEvent, int currentEvent)
{ 
  StIOMaker *inpMk = (StIOMaker *)chain->GetMaker("inputStream");

  int event = 0;

  cout << "firstEvent = " << firstEvent << ", currentEvent = " << currentEvent << endl;
  
  if (inpMk) {
    event = inpMk->GetEventNumber() + 1;
    if (event == 0)
      event = firstEvent;
    
    cout << "firstEvent = " << firstEvent << ", currentEvent = " << currentEvent << ", event = " << event << endl;

    if (currentEvent > event) {
      cout << "Skip " << currentEvent-event << " events..." << endl;
      inpMk->Skip(currentEvent-event);
    }
  }

  Int_t iMake = 0;
  if (iMake != kStEOF && iMake != kStFatal) {
    chain->Clear();
    iMake = chain->Make();
  }

  printf ("QAInfo: Done with Event [no. %d/run %d/evt. %d/Date.Time %d.%d/sta %d]\n",
	  currentEvent,chain->GetRunNumber(),chain->GetEventNumber(),chain->GetDate(), chain->GetTime(),
	  iMake);
}

const char *filetypes[] = { "All files",     "*",
			    "ROOT files",    "*.root",
			    "ROOT macros",   "*.C",
			    0,               0 };

enum ETestCommandIdentifiers {
   M_CREATE,
   M_DELETE,
   M_DIVIDE,
   M_SAME,
   M_CANVAS_1,
   M_CANVAS_2,
   M_CANVAS_3,
   M_CANVAS_4,
   M_CANVAS_5,

   M_FILE_OPEN,
   M_SET_EVP,
   M_GET_EVT,
   M_GET_EVT_N,
   M_GET_PED,
   M_GET_N_EVTS,
   M_GET_N_PEDS,
   M_GET_BUFFER,
   M_GET_PED_BUFFER,
   M_ADD_EVT,
   M_REMOVE_EVT,
   M_CALC_PED,
   M_CALC_PED2NDORD,
   M_CALC_BAD_AN,
   M_CALC_BAD_AN_FROM_PED,
   M_BAD_AN_NULL,
   M_BAD_AN_OVER,
   M_BAD_AN_OCCUP,
   M_BAD_AN_SET_RMS,
   M_BAD_AN_SET_MEAN_PED,
   M_BAD_AN_SET_MEAN_RMS,
   M_BAD_AN_RMS,
   M_BAD_AN_FREQ_NULL,
   M_BAD_AN_FREQ_OVER,
   M_BAD_AN_FREQ_OCCUP,
   M_DAQ_PED,
   M_DAQ_RMS_PED,
   M_READ_PED,
   M_READ_PED_RMS,
   M_WRITE_PED,
   M_WRITE_PED_RMS,
   M_WRITE_ASCII,
   M_RUN_BFC,
   M_RUN_DST,
   M_RESET_EVT,
   M_RESET_BUFFER,
   M_RESET_PED,
   M_RESET_PEDRMS,
   M_QUIT,

   M_REBIN,
   M_REBIN_ALL_HYB,
   M_REBIN_ALL_HISTOS,
   M_SET_MAX,
   M_SET_MAX_ALL_HYB,
   M_RESET_MAX,
   M_RESET_MAX_ALL_HYB,
   M_SET_MIN,
   M_SET_MIN_ALL_HYB,
   M_RESET_MIN,
   M_RESET_MIN_ALL_HYB,
   M_SET_SEQADJ,
   M_SET_FLAGS,
   M_SET_GLOBAL_TRACKS,
   M_SET_GLOBAL_HITS,
   M_SET_GLOBAL_ALL,
   M_SET_GLOBAL_SVT,
   M_SET_PRIM_TRACKS,
   M_SET_PRIM_HITS,
   M_SET_PRIM_ALL,
   M_SET_PRIM_SVT,
   M_SET_TRACK_PRIM,
   M_SET_TRACK_GLOBAL,

   M_PROJ_X,
   M_PROJ_Y,
   M_PROJ_X_ALL,
   M_PROJ_Y_ALL,
   M_FOURIER,
   M_LADDER,
   M_BARREL,
   M_3D,
   M_CLU,
   M_TRACKS,

   M_RAW,
   M_RAW_2D,
   M_PED,
   M_PED_2D,
   M_CMN,
   M_CMN_2D,
   M_AVE_ANDS,
   M_AVE_TIME,
   M_N_PIXELS_PED,
   M_N_PIXELS_ADC,
   M_N_PIXELS_PED_HYB,
   M_N_PIXELS_ADC_HYB,
   M_N_PIXELS_PED_HALF,
   M_N_PIXELS_ADC_HALF,
   M_RAW_ALL,

   M_PEDESTAL,
   M_RMS_PEDESTAL,
   M_PED_HYB,
   M_RMS_PED_HYB,
   M_PED_ALL,
   M_RMS_PED_ALL,
   M_RMS_PED_LADDER,
   M_RMS_PED_BARREL,
   M_RMS_PED_HALF,

   M_MEAN,
   M_RMS,
   M_MEAN_PIXELS,
   M_RMS_PIXELS,
   M_MEAN_EVENT,
   M_RMS_EVENT,
   M_MEAN_VS_EVENT,
   M_RMS_VS_EVENT,
   M_RMS_ALL,
   M_RMS_LADDER,
   M_RMS_BARREL,
   M_RMS_HALF,

   M_MEAN_PED,
   M_RMS_PED,
   M_MEAN_PED_PIXELS,
   M_RMS_PED_PIXELS,
   M_MEAN_PED_EVENT,
   M_RMS_PED_EVENT,
   M_MEAN_PED_VS_EVENT,
   M_RMS_PED_VS_EVENT,

   M_MEAN_CMN,
   M_RMS_CMN,
   M_MEAN_CMN_PIXELS,
   M_RMS_CMN_PIXELS,
   M_MEAN_CMN_EVENT,
   M_RMS_CMN_EVENT,
   M_MEAN_CMN_VS_EVENT,
   M_RMS_CMN_VS_EVENT,

   M_N_PIXELS_STAT_PED,
   M_N_PIXELS_STAT_ADC,
   M_N_PIXELS_VS_EVENT,
   M_MEAN_ANODE,
   M_MEAN_TIME,

   M_FILE,
   M_POOL,
   M_DATA_RAW,
   M_DATA_ZS,
   M_DATA_SEQ,
   M_RAW_STAT,
   M_ZS_STAT,
   M_SUB_PED,
   M_SUB_CMN,
   M_EVT_HIS,
   M_PIXEL_STAT,
   M_GLOBAL_STAT,
   M_PED_TIME,
   M_PED_CAP,

   M_HELP_CONTENTS,
   M_HELP_SEARCH,
   M_HELP_ABOUT
  
};

int evtNumber=1;
int MAX_EVENTS=1000;
int range, previousEvt, previousEvtDst;
int firstBin, lastBin, nBinsX, nBinsY;
int lastEvent=0;

int maxPadCanvas[5] = {1,1,1,1,1};
int iPadCanvas[5] = {1,1,1,1,1};
int iCanvas;

const char* fileNameDst;
TString lastFileName;

TH1F* inFourierX;
TH1F* inFourierY;

Bool_t seqCalled;
StSvtHybridCollection *clustersColl;

// List of makers
StEventMaker *readerMaker=0;
StDAQMaker *DAQMaker=0;
StSvtDaqMaker *SvtDaqMaker=0;
StSvtPedMaker *PedMaker=0;
StSvtBadAnodesMaker *BadAnMaker=0;
StSvtQAMaker *QAMaker=0;
StSvtSeqAdjMaker *SeqAdjMaker=0;
StSvtClusterMaker *ClusterMaker=0;
StSvtClusterAnalysisMaker *ClusterAnalysisMaker=0;

ClassImp(StSvtMenuBar)

StSvtMenuBar::StSvtMenuBar(const TGWindow *p, UInt_t w, UInt_t h, UInt_t o)
      : TGMenuBar(p, w, h, o)
{
   fBFChain = 0;
   fDstChain = 0;
   fFirstEvent = 0;
   mDrawOption = " ";

   //fFlags = "off FieldOff tdaq tpc -PreVtx -Kink -Xi -V0 global dst display geant y1h";
   //fFlags = "P00h -Kink -Xi -V0 noevent display geant y1h";
   fFlags = "drawDst";

   // Create test main frame. A TGMainFrame is a top level window.
 
   // Create menubar and popup menus. The hint objects are used to place
   // and group the different menu widgets with respect to eachother.
   fMenuBarItemLayout = new TGLayoutHints(kLHintsTop | kLHintsLeft, 0, 4, 0, 0);
   fMenuBarHelpLayout = new TGLayoutHints(kLHintsTop | kLHintsRight);
 
   fMenuControl = new TGPopupMenu(fClient->GetRoot());
   fMenuControl->AddEntry("&Open Data File", M_FILE_OPEN);
   fMenuControl->AddEntry("&Set Event Pool", M_SET_EVP);
   fMenuControl->AddSeparator();
   fMenuControl->AddEntry("&Get Event", M_GET_EVT);
   fMenuControl->AddEntry("&Get Event Number <N>", M_GET_EVT_N);
   //   fMenuControl->AddEntry("G&et Pedestal Event", M_GET_PED);
   fMenuControl->AddEntry("Get <&N> Events", M_GET_N_EVTS);
   //   fMenuControl->AddEntry("Get <&N> Pedestal Events", M_GET_N_PEDS);
   //   fMenuControl->AddEntry("Get Events from &Buffer", M_GET_BUFFER);
   //   fMenuControl->AddEntry("Get Pedestal Events from B&uffer", M_GET_PED_BUFFER);
   //   fMenuControl->AddSeparator();
   //   fMenuControl->AddEntry("Re&move Event from Statistics", M_REMOVE_EVT);
   fMenuControl->AddSeparator();
   fMenuControl->AddEntry("&Get Pedestals from DAQ File", M_DAQ_PED);
   fMenuControl->AddEntry("&Get RMS Pedestals from DAQ File", M_DAQ_RMS_PED);
   fMenuControl->AddSeparator();
   fMenuControl->AddEntry("&Read Pedestals from File", M_READ_PED);
   fMenuControl->AddEntry("&Read Pedestals RMS from File", M_READ_PED_RMS);
   fMenuControl->AddSeparator();
   fMenuControl->AddEntry("&Calculate Pedestals and RMS from <N> Events", M_CALC_PED);
   fMenuControl->AddEntry("&Calculate 2nd Order Pedestals from <N> Events", M_CALC_PED2NDORD);
   fMenuControl->AddEntry("&Write Pedestals to File", M_WRITE_PED);
   fMenuControl->AddEntry("&Write Pedestals RMS to File", M_WRITE_PED_RMS);
   fMenuControl->AddSeparator();
   fMenuControl->AddEntry("&Calculate Bad Anodes from <N> Events", M_CALC_BAD_AN);
   fMenuControl->AddEntry("&Calculate Bad Anodes from Pedestals", M_CALC_BAD_AN_FROM_PED);

   fMenuControl->AddSeparator();
   fMenuControl->AddEntry("Write histo(2D) content to ASCII file",M_WRITE_ASCII);
   fMenuControl->AddSeparator();
   //fMenuControl->AddEntry("&Run BFC", M_RUN_BFC);
   //fMenuControl->AddSeparator();
   fMenuControl->AddEntry("Re&set Current Histos", M_RESET_EVT);
   fMenuControl->AddEntry("Reset Statistics", M_RESET_BUFFER);
   fMenuControl->AddEntry("Reset Pedestals", M_RESET_PED);
   fMenuControl->AddEntry("Reset Pedestals RMS", M_RESET_PEDRMS);
   fMenuControl->AddSeparator();
   fMenuControl->AddEntry("&Quit", M_QUIT);
  
   fMenuControl->DisableEntry(M_SET_EVP);

   fMenuSource = new TGPopupMenu(fClient->GetRoot());
   fMenuSource->AddEntry("Data from File", M_FILE);
   fMenuSource->AddEntry("Data from Event Pool", M_POOL);
   fMenuSource->AddSeparator();
   fMenuSource->AddEntry("NON-Zero Supressed Data", M_DATA_RAW);
   fMenuSource->AddEntry("Zero Supressed Data", M_DATA_ZS);
   fMenuSource->AddEntry("Sequence Adusted Data", M_DATA_SEQ);
   fMenuSource->AddSeparator();

   fMenuStat = new TGPopupMenu(fClient->GetRoot());
   fMenuStat->AddEntry("Raw Data", M_RAW_STAT);
   fMenuStat->AddEntry("Zero Supressed Data", M_ZS_STAT);
   fMenuStat->AddEntry("Pedestal Subtracted", M_SUB_PED);
   fMenuStat->AddEntry("C.M.N. Subtracted", M_SUB_CMN);
   fMenuStat->AddEntry("Keep Event History", M_EVT_HIS);
   fMenuStat->AddEntry("Pixel", M_PIXEL_STAT);
   //fMenuStat->AddEntry("Global", M_GLOBAL_STAT);
   fMenuSource->AddPopup("Set Statistics",fMenuStat);

   fMenuSource->AddSeparator();
   fMenuSource->AddEntry("Pedestal Relative to First Time Bin", M_PED_TIME);
   fMenuSource->AddEntry("Pedestal Relative to First Capacitor", M_PED_CAP);

   fMenuSource->CheckEntry(M_FILE);
   fMenuSource->CheckEntry(M_DATA_ZS);
   fMenuSource->CheckEntry(M_PED_TIME);

   fMenuCanvas = new TGPopupMenu(fClient->GetRoot());
   fMenuCanvas->AddEntry("Create Canvas", M_CREATE);
   fMenuCanvas->AddEntry("Delete Canvas", M_DELETE);
   fMenuCanvas->AddEntry("Divide Canvas", M_DIVIDE);
   fMenuCanvas->AddEntry("Same", M_SAME);
   fMenuCanvas->AddSeparator();
   fMenuCanvas->AddEntry("Canvas 1", M_CANVAS_1);
   fMenuCanvas->AddEntry("Canvas 2", M_CANVAS_2);
   fMenuCanvas->AddEntry("Canvas 3", M_CANVAS_3);
   fMenuCanvas->AddEntry("Canvas 4", M_CANVAS_4);
   fMenuCanvas->AddEntry("Canvas 5", M_CANVAS_5);

   fMenuEdit = new TGPopupMenu(fClient->GetRoot());
   fMenuEdit->AddEntry("Re-Bin Current 2D Pixel Histo",M_REBIN);
   fMenuEdit->AddEntry("Re-Bin Current 2D Pixel Histo for All Hybrids",M_REBIN_ALL_HYB);
   fMenuEdit->AddEntry("Re-Bin ALL 2D Pixel Histos",M_REBIN_ALL_HISTOS);
   fMenuEdit->AddSeparator();
   fMenuEdit->AddEntry("Set Maximum",M_SET_MAX);
   fMenuEdit->AddEntry("Set Maximum All Hybrids",M_SET_MAX_ALL_HYB);
   fMenuEdit->AddSeparator();
   fMenuEdit->AddEntry("Set Minimum",M_SET_MIN);
   fMenuEdit->AddEntry("Set Minimum All Hybrids",M_SET_MIN_ALL_HYB);
   fMenuEdit->AddSeparator();
   fMenuEdit->AddEntry("Reset Maximum",M_RESET_MAX);
   fMenuEdit->AddEntry("Reset Maximum All Hybrids",M_RESET_MAX_ALL_HYB);
   fMenuEdit->AddSeparator();
   fMenuEdit->AddEntry("Reset Minimum",M_RESET_MIN);
   fMenuEdit->AddEntry("Reset Minimum All Hybrids",M_RESET_MIN_ALL_HYB);

   fMenuEdit->AddSeparator();
   fMenuEdit->AddEntry("Set Parameters for Sequence Adjusting",M_SET_SEQADJ);

   fMenuTrack = new TGPopupMenu(fClient->GetRoot());
   fMenuTrack->AddEntry("Primary",M_SET_TRACK_PRIM);
   fMenuTrack->AddEntry("Global",M_SET_TRACK_GLOBAL);

   fMenuEdit->AddSeparator();
   fMenuEdit->AddPopup("Set Type of Track to Project",fMenuTrack);
   fMenuTrack->CheckEntry(M_SET_TRACK_PRIM);

   fMenuEdit->AddSeparator();
   fMenuEdit->AddEntry("Set Flags for BFC",M_SET_FLAGS);

   fMenuEdit->AddSeparator();
   fMenuBadAn = new TGPopupMenu(fClient->GetRoot());
   fMenuBadAn->AddEntry("Set Mean Pedestal thresholds", M_BAD_AN_SET_MEAN_PED);
   fMenuBadAn->AddEntry("Set Mean RMS thresholds", M_BAD_AN_SET_MEAN_RMS);
   fMenuBadAn->AddEntry("Set RMS threshold", M_BAD_AN_SET_RMS);
   fMenuBadAn->AddEntry("Null ADC threshold in time bins", M_BAD_AN_NULL);
   fMenuBadAn->AddEntry("Overloaded ADC threshold in time bins", M_BAD_AN_OVER);
   fMenuBadAn->AddEntry("Occupancy ADC threshold in time bins", M_BAD_AN_OCCUP);
   fMenuBadAn->AddEntry("RMS threshold in time bins", M_BAD_AN_RMS);
   fMenuBadAn->AddEntry("Events fraction for Null ADC", M_BAD_AN_FREQ_NULL);
   fMenuBadAn->AddEntry("Events fraction for Overloaded ADC", M_BAD_AN_FREQ_OVER);
   fMenuBadAn->AddEntry("Events fraction for Occupancy", M_BAD_AN_FREQ_OCCUP);
   fMenuEdit->AddPopup("Set Bad Anodes Parameters",fMenuBadAn);

   fMenuEdit->AddSeparator();
   fMenuFilter = new TGPopupMenu(fClient->GetRoot());

   fMenuGlobal = new TGPopupMenu(fClient->GetRoot());
   fMenuGlobal->AddEntry("See All Tracks",M_SET_GLOBAL_TRACKS);
   fMenuGlobal->AddEntry("See All Hits",M_SET_GLOBAL_HITS);
   fMenuGlobal->AddEntry("See All Tracks and Hits",M_SET_GLOBAL_ALL);
   fMenuGlobal->AddEntry("See Only Tracks and Hits that reach the SVT",M_SET_GLOBAL_SVT);
   fMenuFilter->AddPopup("Global Tracks",fMenuGlobal);

   fMenuPrim = new TGPopupMenu(fClient->GetRoot());
   fMenuPrim->AddEntry("See All Tracks",M_SET_PRIM_TRACKS);
   fMenuPrim->AddEntry("See All Hits",M_SET_PRIM_HITS);
   fMenuPrim->AddEntry("See All Tracks and Hits",M_SET_PRIM_ALL);
   fMenuPrim->AddEntry("See Only Tracks and Hits that reach the SVT",M_SET_PRIM_SVT);
   fMenuFilter->AddPopup("Primary Tracks",fMenuPrim);

   fMenuGlobal->CheckEntry(M_SET_GLOBAL_SVT);

   fMenuEdit->AddPopup("Set 3D Filter",fMenuFilter);

   fMenuView = new TGPopupMenu(fClient->GetRoot());
   fMenuView->AddEntry("Projection on X-axis",M_PROJ_X);
   fMenuView->AddEntry("Projection on Y-axis",M_PROJ_Y);
   fMenuView->AddEntry("Projection each Y bin on X-axis",M_PROJ_X_ALL);
   fMenuView->AddEntry("Projection each X bin on Y-axis",M_PROJ_Y_ALL);
   fMenuView->AddSeparator();
   fMenuView->AddEntry("Fourier Analysis",M_FOURIER);
   fMenuView->AddSeparator();
   fMenuView->AddEntry("Clusters - Anode vs Time",M_CLU);
   fMenuView->AddSeparator();
   fMenuView->AddEntry("Tracks Projected to Hybrids - Anode vs Time",M_TRACKS);
   fMenuView->AddSeparator();
   fMenuView->AddEntry("Ladder",M_LADDER);
   fMenuView->AddSeparator();
   fMenuView->AddEntry("Barrel",M_BARREL);
   fMenuView->AddSeparator();
   fMenuView->AddEntry("STAR in 3D",M_3D);

   //fMenuView->DisableEntry(M_CLU);
   fMenuView->DisableEntry(M_TRACKS);
   fMenuView->DisableEntry(M_3D);

   fMenuPedHist = new TGPopupMenu(fClient->GetRoot());
   fMenuPedHist->AddEntry("Pedestal - Anode vs Time",M_PEDESTAL);
   fMenuPedHist->AddEntry("RMS Pedestal - Anode vs Time",M_RMS_PEDESTAL);
   fMenuPedHist->AddSeparator();
   fMenuPedHist->AddEntry("Pedestal",M_PED_HYB);
   fMenuPedHist->AddEntry("RMS Pedestal",M_RMS_PED_HYB);
   fMenuPedHist->AddSeparator();
   fMenuPedHist->AddEntry("Pedestal for all hybrids",M_PED_ALL);
   fMenuPedHist->AddEntry("RMS Pedestal for all hybrids",M_RMS_PED_ALL);
   fMenuPedHist->AddEntry("Averaged RMS Pedestal per hybrid for each ladder",M_RMS_PED_LADDER);
   fMenuPedHist->AddEntry("Averaged RMS Pedestal per hybrid for each barrel",M_RMS_PED_BARREL);
   fMenuPedHist->AddEntry("Averaged RMS Pedestal per half ladder",M_RMS_PED_HALF);

   fMenuSingEvtHist = new TGPopupMenu(fClient->GetRoot());
   fMenuSingEvtHist->AddEntry("Raw ADC",M_RAW);
   fMenuSingEvtHist->AddEntry("Raw ADC - Anode vs Time",M_RAW_2D);
   fMenuSingEvtHist->AddEntry("Raw ADC for all hybrids",M_RAW_ALL);
   fMenuSingEvtHist->AddSeparator();
   fMenuSingEvtHist->AddEntry("Pedestal Subtracted ADC",M_PED);
   fMenuSingEvtHist->AddEntry("Pedestal Subtracted ADC - Anode vs Time",M_PED_2D);
   fMenuSingEvtHist->AddSeparator();
   fMenuSingEvtHist->AddEntry("Pedestal and CMN Subtracted ADC",M_CMN);
   fMenuSingEvtHist->AddEntry("Pedestal and CMN Subtracted ADC - Anode vs Time",M_CMN_2D);
   fMenuSingEvtHist->AddSeparator();
   fMenuSingEvtHist->AddEntry("Average Over Anodes (Pedestal Subtracted ADC) vs Time",M_AVE_ANDS);
   fMenuSingEvtHist->AddEntry("Average Over Time (Pedestal Subtracted ADC) vs Anode",M_AVE_TIME);
   fMenuSingEvtHist->AddSeparator();
   fMenuSingEvtHist->AddEntry("Number of pixels above pedestal per hybrid for each ladder",M_N_PIXELS_PED);
   fMenuSingEvtHist->AddEntry("Number of pixels above pedestal per hybrid for each barrel",M_N_PIXELS_PED_HYB);
   fMenuSingEvtHist->AddEntry("Number of pixels above pedestal per half ladder for each barrel",M_N_PIXELS_PED_HALF);
   fMenuSingEvtHist->AddSeparator();
   fMenuSingEvtHist->AddEntry("Number of pixels above threshold per hybrid for each ladder",M_N_PIXELS_ADC);
   fMenuSingEvtHist->AddEntry("Number of pixels above threshold per hybrid for each barrel",M_N_PIXELS_ADC_HYB);
   fMenuSingEvtHist->AddEntry("Number of pixels above threshold per half ladder for each barrel",M_N_PIXELS_ADC_HALF);
 
   fMenuSingEvtHist->CheckEntry(M_RAW);
   fMenuSingEvtHist->DisableEntry(M_PED);
   fMenuSingEvtHist->DisableEntry(M_PED_2D);
   fMenuSingEvtHist->DisableEntry(M_CMN);
   fMenuSingEvtHist->DisableEntry(M_CMN_2D);

   fMenuMultEvtHist1 = new TGPopupMenu(fClient->GetRoot());
   fMenuMultEvtHist1->AddEntry("Mean ADC - Anode vs Time",M_MEAN);
   fMenuMultEvtHist1->AddEntry("RMS ADC - Anode vs Time",M_RMS);
   fMenuMultEvtHist1->AddSeparator();
   fMenuMultEvtHist1->AddEntry("Mean ADC",M_MEAN_PIXELS);
   fMenuMultEvtHist1->AddEntry("RMS ADC",M_RMS_PIXELS);
   fMenuMultEvtHist1->AddSeparator();
   fMenuMultEvtHist1->AddEntry("RMS ADC for all hybrids",M_RMS_ALL);
   fMenuMultEvtHist1->AddEntry("Averaged RMS ADC per hybrid for each ladder",M_RMS_LADDER);
   fMenuMultEvtHist1->AddEntry("Averaged RMS ADC per hybrid for each barrel",M_RMS_BARREL);
   fMenuMultEvtHist1->AddEntry("Averaged RMS ADC per half ladder for each barrel",M_RMS_HALF);
   fMenuMultEvtHist1->AddSeparator();
   fMenuMultEvtHist1->AddEntry("Event Mean ADC",M_MEAN_EVENT);
   fMenuMultEvtHist1->AddEntry("Event RMS ADC",M_RMS_EVENT);
   fMenuMultEvtHist1->AddSeparator();
   fMenuMultEvtHist1->AddEntry("Mean ADC vs Event",M_MEAN_VS_EVENT);
   fMenuMultEvtHist1->AddEntry("RMS ADC vs Event",M_RMS_VS_EVENT);

   //fMenuMultEvtHist1->DisableEntry(M_MEAN_EVENT);
   //fMenuMultEvtHist1->DisableEntry(M_RMS_EVENT);
   //fMenuMultEvtHist1->DisableEntry(M_MEAN_VS_EVENT);
   //fMenuMultEvtHist1->DisableEntry(M_RMS_VS_EVENT);

   fMenuMultEvtHist2 = new TGPopupMenu(fClient->GetRoot());
   fMenuMultEvtHist2->AddEntry("Mean ADC (Pedestal Subtracted) - Anode vs Time",M_MEAN_PED);
   fMenuMultEvtHist2->AddEntry("RMS ADC (Pedestal Subtracted) - Anode vs Time",M_RMS_PED);
   fMenuMultEvtHist2->AddSeparator();
   fMenuMultEvtHist2->AddEntry("Mean ADC (Pedestal Subtracted) - All Pixels",M_MEAN_PED_PIXELS);
   fMenuMultEvtHist2->AddEntry("RMS ADC (Pedestal Subtracted) - All Pixels",M_RMS_PED_PIXELS);
   fMenuMultEvtHist2->AddSeparator();
   fMenuMultEvtHist2->AddEntry("Event Mean ADC (Pedestal Subtracted)",M_MEAN_PED_EVENT);
   fMenuMultEvtHist2->AddEntry("Event RMS ADC (Pedestal Subtracted)",M_RMS_PED_EVENT);
   fMenuMultEvtHist2->AddSeparator();
   fMenuMultEvtHist2->AddEntry("Mean ADC (Pedestal Subtracted) vs Event",M_MEAN_PED_VS_EVENT);
   fMenuMultEvtHist2->AddEntry("RMS ADC (Pedestal Subtracted) vs Event",M_RMS_PED_VS_EVENT);

   //fMenuMultEvtHist2->DisableEntry(M_MEAN_PED_EVENT);
   //fMenuMultEvtHist2->DisableEntry(M_RMS_PED_EVENT);
   //fMenuMultEvtHist2->DisableEntry(M_MEAN_PED_VS_EVENT);
   //fMenuMultEvtHist2->DisableEntry(M_RMS_PED_VS_EVENT);

   fMenuMultEvtHist3 = new TGPopupMenu(fClient->GetRoot());
   fMenuMultEvtHist3->AddEntry("Mean ADC (CMN Subtracted) - Anode vs Time",M_MEAN_CMN);
   fMenuMultEvtHist3->AddEntry("RMS ADC (CMN Subtracted) - Anode vs Time",M_RMS_CMN);
   fMenuMultEvtHist3->AddSeparator();
   fMenuMultEvtHist3->AddEntry("Mean ADC (CMN Subtracted) - All Pixels",M_MEAN_CMN_PIXELS);
   fMenuMultEvtHist3->AddEntry("RMS ADC (CMN Subtracted) - All Pixels",M_RMS_CMN_PIXELS);
   fMenuMultEvtHist3->AddSeparator();
   fMenuMultEvtHist3->AddEntry("Event Mean ADC (CMN Subtracted)",M_MEAN_CMN_EVENT);
   fMenuMultEvtHist3->AddEntry("Event RMS ADC (CMN Subtracted)",M_RMS_CMN_EVENT);
   fMenuMultEvtHist3->AddSeparator();
   fMenuMultEvtHist3->AddEntry("Mean ADC (CMN Subtracted) vs Event",M_MEAN_CMN_VS_EVENT);
   fMenuMultEvtHist3->AddEntry("RMS ADC (CMN Subtracted) vs Event",M_RMS_CMN_VS_EVENT);

   //fMenuMultEvtHist3->DisableEntry(M_MEAN_CMN_EVENT);
   //fMenuMultEvtHist3->DisableEntry(M_RMS_CMN_EVENT);
   //fMenuMultEvtHist3->DisableEntry(M_MEAN_CMN_VS_EVENT);
   //fMenuMultEvtHist3->DisableEntry(M_RMS_CMN_VS_EVENT);
   
   fMenuMultEvtHist4 = new TGPopupMenu(fClient->GetRoot());
   fMenuMultEvtHist4->AddEntry("Number of pixels above pedestal per hybrid",M_N_PIXELS_STAT_PED);
   fMenuMultEvtHist4->AddEntry("Number of pixels above threshold per hybrid",M_N_PIXELS_STAT_ADC);
   fMenuMultEvtHist4->AddSeparator();
   fMenuMultEvtHist4->AddEntry("Number of pixels vs Event #",M_N_PIXELS_VS_EVENT);
   fMenuMultEvtHist4->AddSeparator();
   fMenuMultEvtHist4->AddEntry("Mean anode per hybrid",M_MEAN_ANODE);
   fMenuMultEvtHist4->AddEntry("Mean time per hybrid",M_MEAN_TIME);

   fMenuMultEvtHist = new TGPopupMenu(fClient->GetRoot());
   fMenuMultEvtHist->AddPopup("Raw Data",fMenuMultEvtHist1);
   fMenuMultEvtHist->AddPopup("Pedestal Subtracted",fMenuMultEvtHist2);
   fMenuMultEvtHist->AddPopup("C.M.N. Subtracted",fMenuMultEvtHist3);
   fMenuMultEvtHist->AddPopup("Occupancy",fMenuMultEvtHist4);
   fMenuMultEvtHist->DisableEntry(1);

   fMenuHelp = new TGPopupMenu(fClient->GetRoot());
   fMenuHelp->AddEntry("&Contents", M_HELP_CONTENTS);
   fMenuHelp->AddEntry("&Search...", M_HELP_SEARCH);
   fMenuHelp->AddSeparator();
   fMenuHelp->AddEntry("&About", M_HELP_ABOUT);
 
   // Menu button messages are handled by the main frame (i.e. "p")
   // ProcessMessage() method.
   fMenuControl->Associate(p);
   fMenuSource->Associate(p);
   fMenuCanvas->Associate(p);
   fMenuEdit->Associate(p);
   fMenuFilter->Associate(p);
   fMenuView->Associate(p);
   fMenuPedHist->Associate(p);
   fMenuSingEvtHist->Associate(p);
   fMenuMultEvtHist->Associate(p);
   fMenuMultEvtHist1->Associate(p);
   fMenuMultEvtHist2->Associate(p);
   fMenuMultEvtHist3->Associate(p);
   fMenuHelp->Associate(p);

   AddPopup("&Control", fMenuControl, fMenuBarItemLayout);
   AddPopup("&Data", fMenuSource, fMenuBarItemLayout);
   AddPopup("Can&vas", fMenuCanvas, fMenuBarItemLayout);
   AddPopup("&Edit", fMenuEdit, fMenuBarItemLayout);
   AddPopup("&View", fMenuView, fMenuBarItemLayout);
   AddPopup("&Pedestal", fMenuPedHist, fMenuBarItemLayout);
   AddPopup("&Single Event", fMenuSingEvtHist, fMenuBarItemLayout);
   AddPopup("&Multi Events", fMenuMultEvtHist, fMenuBarItemLayout);
   AddPopup("&Help", fMenuHelp, fMenuBarHelpLayout);
} 
 
StSvtMenuBar::~StSvtMenuBar()
{
   // Delete all created widgets.
  
   delete fMenuBarItemLayout;
   delete fMenuBarHelpLayout;
 
   delete fMenuControl;
   delete fMenuCanvas;
   delete fMenuSingEvtHist;
   delete fMenuFilter;
   delete fMenuHelp;

   delete fChain;
   delete fMainChain;
}
  
void StSvtMenuBar::SetChain(StChain* aChain, StChain* aBFChain)
{
  fChain = aChain;
  fMainChain = aBFChain;
  SetMakers();
}

void StSvtMenuBar::CloseWindow()
{
   // Got close message for this MainFrame. Calls parent CloseWindow()
   // (which destroys the window) and terminate the application.
   // The close message is generated by the window manager when its close
   // window menu item is selected.
 
  //  TGMainFrame::CloseWindow();
  gApplication->Terminate(0);
}

int StSvtMenuBar::SetDefaultCanvas()
{
  if (fCanvas)
    fCanvas->cd();
  else {
    cout << "NO Canvas activated !!!" << endl;		    
    return 0;
  }

  return 1;
}

int StSvtMenuBar::UpdateCanvas()
{
  if (fCanvas)
    fCanvas->Update();
  else {
    cout << "NO Canvas activated !!!" << endl;		    
    return 0;
  }
  
  return 1;
}

void StSvtMenuBar::UnCheckAllEntries()
{
  int i;

  fMenuView->UnCheckEntry(M_PROJ_X);
  fMenuView->UnCheckEntry(M_PROJ_Y);
  fMenuView->UnCheckEntry(M_PROJ_X_ALL);
  fMenuView->UnCheckEntry(M_PROJ_Y_ALL);
  fMenuView->UnCheckEntry(M_FOURIER);
  fMenuView->UnCheckEntry(M_LADDER);
  fMenuView->UnCheckEntry(M_BARREL);

  for ( i=M_PEDESTAL;i<=M_RMS_PED_HALF;i++)
    if (fMenuPedHist->IsEntryChecked(i)) fMenuPedHist->UnCheckEntry(i);

  for ( i=M_RAW;i<=M_RAW_ALL;i++)
    if (fMenuSingEvtHist->IsEntryChecked(i)) fMenuSingEvtHist->UnCheckEntry(i);

  for ( i=M_MEAN;i<=M_RMS_HALF;i++)
    if (fMenuMultEvtHist1->IsEntryChecked(i)) fMenuMultEvtHist1->UnCheckEntry(i);

  for ( i=M_MEAN_PED;i<=M_RMS_PED_VS_EVENT;i++)
    if (fMenuMultEvtHist2->IsEntryChecked(i)) fMenuMultEvtHist2->UnCheckEntry(i);

  for ( i=M_MEAN_CMN;i<=M_RMS_CMN_VS_EVENT;i++)
    if (fMenuMultEvtHist3->IsEntryChecked(i)) fMenuMultEvtHist3->UnCheckEntry(i);

  for ( i=M_N_PIXELS_STAT_PED;i<=M_MEAN_TIME;i++)
    if (fMenuMultEvtHist4->IsEntryChecked(i)) fMenuMultEvtHist4->UnCheckEntry(i);
}

void StSvtMenuBar::SetMakers()
{
   // instantiate makers
   DAQMaker = (StDAQMaker*)fChain->GetMaker("DAQInput");
   SvtDaqMaker = (StSvtDaqMaker*)fChain->GetMaker("SvtDaq");
   PedMaker = (StSvtPedMaker*)fChain->GetMaker("SvtPed");
   BadAnMaker = (StSvtBadAnodesMaker*)fChain->GetMaker("SvtBadAn");
   QAMaker = (StSvtQAMaker*)fChain->GetMaker("SvtQA");
   SeqAdjMaker = (StSvtSeqAdjMaker*)fChain->GetMaker("SvtSeqAdj");
   ClusterMaker = (StSvtClusterMaker*)fChain->GetMaker("SvtClu");
   ClusterAnalysisMaker = (StSvtClusterAnalysisMaker*)fChain->GetMaker("SvtCluAna"); 
}
	    
void StSvtMenuBar::OpenFile(const char* file)
{
  // function that takes care of all makers to open the daq file
  DAQMaker->SetFile(file);
  DAQMaker->Open();
  DAQMaker->Init();
}

void StSvtMenuBar::InitMakers()
{
  SvtDaqMaker->Init();
  //  SvtDaqMaker->SetSvtData();
  PedMaker->SetSvtData();
  BadAnMaker->Init();
  SeqAdjMaker->Init();
  //QAMaker->SetSvtData();
  QAMaker->Init();
  ClusterMaker->Init();
  ClusterAnalysisMaker->Init(); 
}

void StSvtMenuBar::CloseFile(char* option)
{
  // function that takes care of all makers to close the daq file

  DAQMaker->Close();
  //SvtDaqMaker->Reset();
  SvtDaqMaker->Clear(0);
  SeqAdjMaker->Reset();
  ClusterMaker->Reset();
  //ClusterAnalysisMaker->Reset();
  DAQMaker->Clear();
  //  fChain->Clear();
}

void StSvtMenuBar::DrawHist(char* option)
{
  //  TCanvas *canvas;
  TIter* iter;
  Bool_t found;
  int hist;

  cout << "mHist = " << mHist << endl;

  if (TString(option) == "BUTTON") {
    if (mHist == 1005) {
      mHist = fSvtMonitor->getHistID();
      fMenuView->UnCheckEntry(M_LADDER);
    }
    if (mHist == 1006) {
      mHist = fSvtMonitor->getHistID();
      fMenuView->UnCheckEntry(M_BARREL);
    }
    if (fFilter) {
      fFilter->SetId_globtrk(0,0);
      fFilter->SetHybridID(fSvtMonitor->getWaferID(),fSvtMonitor->getHybridID());
    }
  }

  if (TString(option) == "LADDERBUTTON") {
    if (mHist == 1006) {
      mHist = fSvtMonitor->getHistID();
      fMenuView->UnCheckEntry(M_BARREL);
    }
  }

  if (iPadCanvas[iCanvas-1] > maxPadCanvas[iCanvas-1]) iPadCanvas[iCanvas-1] = 1;
  if (maxPadCanvas[iCanvas-1] == 1) iPadCanvas[iCanvas-1] = 0;
  fCanvas->cd(iPadCanvas[iCanvas-1]++);

  switch (mHist) {
  case 10:
    fSvtGuiMonitor->drawHist("Psame");
    break;
  case 1001:
    if (TString(mDrawOption) != "same")
      gPad->Clear();
    if (!fSvtMonitor->getHist()->GetEntries())
      if (fSvtMonitor->getHistID() < 300)
	fSvtMonitor->fillHist();
      else
	fSvtMonitor->fillHistPed();
    inFourierX = fSvtGuiMonitor->projectX(firstBin,lastBin,mDrawOption);
    if (fMenuView->IsEntryChecked(M_FOURIER)) {
      if (inFourierX) {
	fSvtMonitor->fillFourier(inFourierX,"X");
	hist = fSvtMonitor->getHistID();
	fSvtMonitor->setHistID(58);
	fSvtGuiMonitor->drawHist(mDrawOption);
	fSvtMonitor->setHistID(hist);
      }
    }
    break;
  case 1002:
    if (TString(mDrawOption) != "same")
      gPad->Clear();
    if (!fSvtMonitor->getHist()->GetEntries())
      if (fSvtMonitor->getHistID() < 300)
	fSvtMonitor->fillHist();
      else
	fSvtMonitor->fillHistPed();
    inFourierY = fSvtGuiMonitor->projectY(firstBin,lastBin,mDrawOption);
    if (fMenuView->IsEntryChecked(M_FOURIER)) {
      if (inFourierY) {
	fSvtMonitor->fillFourier(inFourierY,"Y");
	hist = fSvtMonitor->getHistID();
	fSvtMonitor->setHistID(58);
	fSvtGuiMonitor->drawHist(mDrawOption);
	fSvtMonitor->setHistID(hist);
      }
    }
    break;
  case 1003:
    if (TString(mDrawOption) != "same")
      gPad->Clear();
    if (!fSvtMonitor->getHist()->GetEntries())
      if (fSvtMonitor->getHistID() < 300)
	fSvtMonitor->fillHist();
      else
	fSvtMonitor->fillHistPed();
    fSvtGuiMonitor->projectX(0,0,mDrawOption);
    break;
  case 1004:
    if (TString(mDrawOption) != "same")
      gPad->Clear();
    if (!fSvtMonitor->getHist()->GetEntries())
      if (fSvtMonitor->getHistID() < 300)
	fSvtMonitor->fillHist();
      else
	fSvtMonitor->fillHistPed();
    fSvtGuiMonitor->projectY(0,0,mDrawOption);
    break;
  case 1005:
    found = kFALSE;
    iter = new TIter(gROOT->GetListOfCanvases());
    for (int i = 0;i<gROOT->GetListOfCanvases()->GetSize();i++) {
      if(TString(((TCanvas*)gROOT->GetListOfCanvases()->At(i))->GetName()) == "CanvasLadder") {
	fCanvasLadder = (TCanvas*)gROOT->GetListOfCanvases()->At(i);
	found = kTRUE;
	break;
      }
    }
    
    if (!found)
      fCanvasLadder = new TCanvas("CanvasLadder","Canvas Ladder",1000,600);

    fSvtGuiMonitor->drawHistLadder(mDrawOption,fCanvasLadder,firstBin,lastBin);
    SetDefaultCanvas();
    return;
  case 1006:
    found = kFALSE;
    iter = new TIter(gROOT->GetListOfCanvases());
    for (int i = 0;i<gROOT->GetListOfCanvases()->GetSize();i++) {
      if(TString(((TCanvas*)gROOT->GetListOfCanvases()->At(i))->GetName()) == "CanvasBarrel") {
	fCanvasBarrel = (TCanvas*)gROOT->GetListOfCanvases()->At(i);
	found = kTRUE;
	break;
      }
    }
    
    if (!found)
      fCanvasBarrel = new TCanvas("CanvasBarrel","Canvas Barrel",1000,600);

    fSvtGuiMonitor->drawHistBarrel(mDrawOption,fCanvasBarrel);
    SetDefaultCanvas();
    return;
  default:
    if (TString(mDrawOption) != "same")
      gPad->Clear();
    fSvtGuiMonitor->drawHist(mDrawOption);
    break;
  }

  gPad->Paint();
  gPad->Update();
}


Bool_t StSvtMenuBar::ProcessMessage(const TGWindow *p, Long_t msg, Long_t parm1, Long_t parm2,const void* buffer)
{
   // Handle messages send to the MainWindow object. E.g. all menu button
   // messages.

  int nEvts, evt, i, status, tempHist, imin, imax;
  int thresh, thresh_lo, n_seq_lo, thresh_hi, n_seq_hi, pedOffset;
  float threshf, max, min;

  TGFileInfo fi;
  const char* fileName;
  TString config;

  // St_DataSet *dataSet;

  switch (GET_MSG(msg)) {
 
  case kC_COMMAND:
    switch (GET_SUBMSG(msg)) {
      
    case kCM_BUTTON:
      //printf("Button was pressed, id = %ld\n", parm1);
      break;
      
    case kCM_MENUSELECT:
      //printf("Pointer over menu entry, id=%ld\n", parm1);
      break;
      
    case kCM_MENU:
      switch (parm1) {

//******************************************************	
//    Control Menu 
//******************************************************

      case M_SET_EVP:
	fi.fFileTypes = (const char **)filetypes;
	new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	if (!fi.fFilename) break;
	fileName = fi.fFilename;
	fSvtGuiMonitor->GetSvtView()->SetInfoFile(fileName);
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(fSvtMonitor->getEventNumber(),fSvtMonitor->getEventsInBuffer());
	if (SvtDaqMaker->InitEvp(fileName) == kStErr)
	  cout << "No Event Pool for this run! Try another run or a file..." << endl;
	break;

      case M_FILE_OPEN:
	{//this skips manual input
          if (buffer){
            fileName =(char*)buffer;
            lastEvent = 0;
          }
	  else {
	    if (!parm2) {
	      fi.fFileTypes = (const char **)filetypes;
	      new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	      if (!fi.fFilename) break;
	      fileName = fi.fFilename;
	      lastEvent = 0;
	    }
	    else {
	      fileName = lastFileName.Data();
	    }
	  }

	  CloseFile();
	  OpenFile(fileName);
	  InitMakers();
	  // the following line works but create a memory leakage.
	  // The right line should be
	  //if (fBFChain) delete fBFChain; 
	  // However, StBFChain object has problems during delete process
	  if (fBFChain) fBFChain = 0; 
	  if (fDstChain) fDstChain = 0; 
	  fFirstEvent = 0;
	  fileNameDst = NULL;
	  fSvtGuiMonitor->GetSvtView()->SetInfoFile(fileName);
	  fSvtGuiMonitor->GetSvtView()->SetInfoEvent(fSvtMonitor->getEventNumber(),fSvtMonitor->getEventsInBuffer());

	  if (parm2)
	    break;

	  //break;
	}
	//Don't break here so that you also get the first event when you open the file
	
      case M_GET_EVT:
	if (lastEvent>0) {
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_GET_EVT_N,lastEvent);
	  lastEvent = 0;
	  break;
	}

	//if (TString(mDrawOption) != "same")
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_EVT,0);
	//fChain->Clear();

	if (fMenuSource->IsEntryChecked(M_POOL)) {
	  SvtDaqMaker->SetSvtData();
	  if(SvtDaqMaker->GetSvtEvpData()==kStErr ) return 0;
	}
	else {
	  status = DAQMaker->Make();
	  if (status==kStErr) { 
	    cout << "Error reading event..." << endl;
	    return 0;
	  }
	  else if (status == kStEOF) {
	    cout << "End of file! Get another one..." << endl;
	    return 0;
	  }

	  //if(SvtDaqMaker->GetSvtData() == 3) return 0;
	  if(SvtDaqMaker->Make() == 3) return 0;
	  SvtDaqMaker->PrintEventInfo();
	}

	if (seqCalled)
	  SeqAdjMaker->Make();

	QAMaker->Make();
	//QAMaker->SetData();
	//fSvtMonitor->setStatistics();
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(fSvtMonitor->getEventNumber(),fSvtMonitor->getEventsInBuffer());

	if (!fFirstEvent)
	  fFirstEvent = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	//seqCalled = kFALSE;
	clustersColl = 0;

	if (fMenuView->IsEntryChecked(M_3D))
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_3D,1);

	if (!parm2)
	  if (fMenuSource->IsEntryChecked(M_DATA_SEQ))
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_DATA_SEQ,0);
	  else
	    DrawHist("EVENT");

	break;
	
      case M_GET_N_EVTS:
	nEvts = GetIntegerDialog("Enter number of events:",0);
	for (i=0;i<nEvts;i++)
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_GET_EVT,1);
	break;
	
      case M_GET_EVT_N:
	if (!parm2)
	  evt = GetIntegerDialog("Enter event number:",0);
	else
	  evt = parm2+1;
	if (evt) {
	  if ((fFirstEvent) && (evt > fFirstEvent))
	    cout << "Skip " << evt - fFirstEvent << " events..." << endl;	   
	    
	  for (i = 0;i < MAX_EVENTS;i++) {
	    status = ((StDAQMaker*)fChain->GetMaker("DAQInput"))->Make();

	    if (!fFirstEvent) {
	      fFirstEvent = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	      cout << "Skip " << evt - fFirstEvent << " events..." << endl;	   
	    }

	    if( evt < fFirstEvent){
	      cout << "Cannot get this event. File starts at event " << fFirstEvent << "!!" << endl;
	      break;
	    }
	    else if (evt == ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber()) {
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_EVT,0);	
	      // Check that SVT is there if not dont go on
	      //if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetSvtData() ==3) return 0;
	      if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Make() ==3) return 0;
	      ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->PrintEventInfo();
	      if (seqCalled)
		SeqAdjMaker->Make();
	      ((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetSvtData();
	      ((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetData();
	      fSvtMonitor->setStatistics();
	      if (!parm2)
		DrawHist("EVENT");
	      break;
	    }
	    else if (evt > ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber())
	      continue;
	    else if (evt < ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber()) {
	      fileName = ((StDAQMaker*)fChain->GetMaker("DAQInput"))->GetFile();
	      CloseFile();
	      OpenFile(fileName);
	      InitMakers();
	      /*
	      ((StDAQMaker*)fChain->GetMaker("DAQInput"))->Close();
	      ((StDAQMaker*)fChain->GetMaker("DAQInput"))->SetFile(fileName);
	      ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Reset();
	      ((StDAQMaker*)fChain->GetMaker("DAQInput"))->Clear();
	      ((StDAQMaker*)fChain->GetMaker("DAQInput"))->Init();
	      ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Init();
	      ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->SetSvtData();
	      ((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->SetSvtData();
	      ((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetSvtData();
	      */
	      fFirstEvent = 0;
	    }
	  }
	}
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(fSvtMonitor->getEventNumber(),fSvtMonitor->getEventsInBuffer());
	//seqCalled = kFALSE;
	clustersColl = 0;
	if (fMenuView->IsEntryChecked(M_3D))
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_3D,1);
	break;
	
      case M_DAQ_PED:
	lastFileName  = TString(DAQMaker->GetFile());
	lastEvent = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();

	fi.fFileTypes = (const char **)filetypes;
	new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	if (!fi.fFilename) break;
	CloseFile();
	OpenFile(fi.fFilename);
	//DAQMaker->ProcessToken0(kTRUE);
	SvtDaqMaker->Init();
	SvtDaqMaker->SetSvtPed();
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_PED,0);
	fSvtMonitor->setEvent(0);
	//fChain->Clear();
	status = DAQMaker->Make();
	if (status==kStErr) { 
	  cout << "Error reading event..." << endl;
	  return 0;
	}
	else if (status == kStEOF) {
	  cout << "End of file! Get another one..." << endl;
	  return 0;
	}
	SvtDaqMaker->GetDaqReader();
	if(SvtDaqMaker->GetSvtPed() ==3) return 0;
	SvtDaqMaker->PrintEventInfo();
	QAMaker->SetPedestal();
	//DAQMaker->ProcessToken0(kFALSE);
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile(fi.fFilename);

	if (fMenuSource->IsEntryChecked(M_POOL)) {
	  CloseFile();
	  InitMakers();
	}
	else
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_FILE_OPEN,1);

	break;

      case M_DAQ_RMS_PED:
	lastFileName  = TString(DAQMaker->GetFile());
	lastEvent = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();

	fi.fFileTypes = (const char **)filetypes;
	new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	if (!fi.fFilename) break;
	CloseFile();
	OpenFile(fi.fFilename);
	//DAQMaker->ProcessToken0(kTRUE);
	SvtDaqMaker->Init();
	SvtDaqMaker->SetSvtRMSPed();

	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_PEDRMS,0);
	fSvtMonitor->setEvent(0);
	//fChain->Clear();
	status = DAQMaker->Make();
	if (status==kStErr) { 
	  cout << "Error reading event..." << endl;
	  return 0;
	}
	else if (status == kStEOF) {
	  cout << "End of file! Get another one..." << endl;
	  return 0;
	}
	SvtDaqMaker->GetDaqReader();
	if(SvtDaqMaker->GetSvtRMSPed() == 3) return 0;
	SvtDaqMaker->PrintEventInfo();
	QAMaker->SetRMSPedestal();
	fSvtMonitor->setRMSFactor(16);
	BadAnMaker->setRMSFactor(16);

	//DAQMaker->ProcessToken0(kFALSE);
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile(fi.fFilename);

	if (fMenuSource->IsEntryChecked(M_POOL)) {
	  CloseFile();
	  InitMakers();
	}
	else
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_FILE_OPEN,1);

	break;

      case M_CALC_PED:
	nEvts = GetIntegerDialog("Enter number of events:",0);
	for (i=0;i<nEvts;i++) {
	  if(((StDAQMaker*)fChain->GetMaker("DAQInput"))->Make()){
	    cout << "Reached end of file" << endl;
	    break;
	  }
	  if (!fFirstEvent)
	    fFirstEvent = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	  // Check that SVT is there if not dont go on
	  //if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetSvtData() ==3) return 0;
	  if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Make() ==3) return 0;
	  ((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetData();
	  ((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->AddStat();
	  cout <<"Adding data from event " << i << " to pedestal statistics." << endl;
	}
	cout << "Writing pedestals to memory..." << endl;
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->CalcPed();
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetPedestal();
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetRMSPedestal();
	fSvtMonitor->setRMSFactor(1);
	BadAnMaker->setRMSFactor(1);
	break;

      case M_CALC_PED2NDORD:
	nEvts = GetIntegerDialog("Enter number of events:",0);

	if (!fSvtMonitor->getkPed2ndOrd())
	  fSvtMonitor->setkPed2ndOrd(kTRUE);

	for (i=0;i<nEvts;i++) {
	  if(((StDAQMaker*)fChain->GetMaker("DAQInput"))->Make()){
	    cout << "Reached end of file" << endl;
	    break;
	  }
	  if (!fFirstEvent)
	    fFirstEvent = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	  // Check that SVT is there if not dont go on
	  //if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetSvtData() ==3) return 0;
	  if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Make() ==3) return 0;
	  ((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetData();
	  ((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->AddStat2ndOrd();
	  cout <<"Adding data from event " << i << " to pedestal statistics." << endl;
	}

	cout << "Writting pedestals to memory..." << endl;
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->CalcPed2ndOrd();
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetPedestal2ndOrd();

	break;

      case M_WRITE_PED:
	fi.fFileTypes = (const char **)filetypes;
	new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	if (!fi.fFilename) break;
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->WriteToFile(fi.fFilename);
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile(fi.fFilename);
	cout << "Pedestals written to file " << fi.fFilename << endl;
	break;

      case M_WRITE_PED_RMS:
	fi.fFileTypes = (const char **)filetypes;
	new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	if (!fi.fFilename) break;
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->WriteRMSToFile(fi.fFilename);
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile(fi.fFilename);
	cout << "Pedestals RMS written to file " << fi.fFilename << endl;
	break;

      case M_READ_PED:
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_PED,0);
	fi.fFileTypes = (const char **)filetypes;
	new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	if (!fi.fFilename) break;
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->ReadFromFile(fi.fFilename);
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetPedestal();
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile(fi.fFilename);
	cout << "Pedestals read from file " << fi.fFilename << endl;
	break;

      case M_READ_PED_RMS:
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_PEDRMS,0);
	fi.fFileTypes = (const char **)filetypes;
	new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	if (!fi.fFilename) break;
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->ReadRMSFromFile(fi.fFilename);
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetRMSPedestal();
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile(fi.fFilename);
	cout << "Pedestals RMS read from file " << fi.fFilename << endl;
	fSvtMonitor->setRMSFactor(1);
	BadAnMaker->setRMSFactor(1);
	break;

      case M_CALC_BAD_AN:
	nEvts = GetIntegerDialog("Enter number of events:",0);
	BadAnMaker->Reset();
	for (i=0;i<nEvts;i++) {
	  if(DAQMaker->Make()){
	    cout << "Reached end of file" << endl;
	    break;
	  }
	  if (!fFirstEvent)
	    fFirstEvent = SvtDaqMaker->GetEventNumber();
	  // Check that SVT is there if not dont go on
	  //if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetSvtData() ==3) return 0;
	  //if(SvtDaqMaker->Make() ==3) return 0;
	  if(SvtDaqMaker->Make() ==3) continue;
	  QAMaker->SetData();
	  BadAnMaker->Make();
	  cout <<"Adding data from event " << i << " to bad anodes statistics." << endl;
	}
	cout << "Writing bad anodes to file..." << endl;
	BadAnMaker->Finish();
	break;

      case M_CALC_BAD_AN_FROM_PED:
	BadAnMaker->Reset();
	BadAnMaker->Make();
	cout << "Writing bad anodes to file..." << endl;
	BadAnMaker->Finish();
	break;

      case M_BAD_AN_SET_MEAN_PED:
	thresh = GetIntegerRange("Enter Minimum Mean Pedestal threshold:","Enter Maximum Mean Pedestal threshold:",0,255);
	if (thresh) {
	  imin = int(thresh/10000);
	  imax = thresh - imin*10000;
	  BadAnMaker->setBadMeanPedMin(imin);
	  BadAnMaker->setBadMeanPedMax(imax);
	}
	break;
      case M_BAD_AN_SET_MEAN_RMS:
	threshf = GetFloatRange("Enter Minimum Mean RMS threshold:","Enter Maximum Mean RMS threshold:",0,10);
	if (threshf) {
	  imin = int(threshf/100.);
	  min = float(imin)/100.;
	  max = threshf - imin*100;
	  BadAnMaker->setBadMeanRMSMin(min);
	  BadAnMaker->setBadMeanRMSMax(max);
	  cout << threshf << "   " << min << "   " << max << endl;
	}
	break;
      case M_BAD_AN_SET_RMS:
	thresh = GetFloatDialog("Enter RMS threshold:",10);
	BadAnMaker->setBadRMS(thresh);
	break;
      case M_BAD_AN_NULL:
	thresh = GetIntegerDialog("Enter Null ADC threshold in time bins:",126);
	cout << thresh << endl;
	BadAnMaker->setThresholdNull(thresh);
	break;
      case M_BAD_AN_OVER:
	thresh = GetIntegerDialog("Enter Overloaded ADC threshold in time bins:",60);
	BadAnMaker->setThresholdOver(thresh);
	break;
      case M_BAD_AN_OCCUP:
 	thresh = GetIntegerDialog("Enter Occupancy threshold in time bins:",120);
	BadAnMaker->setThresholdOccup(thresh);
	break;
      case M_BAD_AN_RMS:
	thresh = GetIntegerDialog("Enter RMS threshold in time bins:",60);
	BadAnMaker->setThresholdRMS(thresh);
	break;
      case M_BAD_AN_FREQ_NULL:
	threshf = GetFloatDialog("Enter Null ADC events fraction:",0.);
	BadAnMaker->setFrequencyNull(threshf);
	break;
      case M_BAD_AN_FREQ_OVER:
	threshf = GetFloatDialog("Enter Overloaded ADC events fraction:",0.);
	BadAnMaker->setFrequencyOver(threshf);
	break;
      case M_BAD_AN_FREQ_OCCUP:
	threshf = GetFloatDialog("Enter Occupancy events fraction:",0.);
	BadAnMaker->setFrequencyOccup(threshf);
	break;

	/*
      case M_RUN_BFC:
	//fileName = ((StDAQMaker*)fChain->GetMaker("DAQInput"))->GetFile();
	//fileNameDst = "/star/data07/reco/P00he/2000/07/st_physics_1183020_raw_0001.dst.root";
	evt = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	if (evt < 0 || !fFirstEvent)
	  cout << "NO event to run BFC! Get one..." << endl;
	else if (evt == previousEvt && fBFChain)
	  break;
	else {
	  if (!fileNameDst) {
	    fi.fFileTypes = (char **)filetypes;
	    new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	    if (!fi.fFilename) break;
	    fileNameDst = fi.fFilename;
	  }

	  if (!fBFChain) {
	    fMainChain->cd();
	    fBFChain = new StBFChain("bfc");

	    fBFChain->SetFlags(fFlags);
	    fBFChain->Set_IO_Files(fileNameDst);
	    fBFChain->cd();
	    fBFChain->Load();
	    fBFChain->Instantiate();

	    fDisplay = (StEventDisplayMaker *) fBFChain->GetMaker("EventDisplay");
	    if (!fFilter) {
	      fFilter = new StTrackFilter();
	      fDisplay->SetFilter((StVirtualEventFilter *)fFilter,StEventDisplayMaker::kTptTrack); 
	      fDisplay->SetFilter((StVirtualEventFilter *)fFilter,StEventDisplayMaker::kTable);    
	      fFilter->SetHybridID(fSvtMonitor->getWaferID(),fSvtMonitor->getHybridID());
	    }
	    fDisplay->AddName("dst/primtrk"); 
	    fDisplay->AddName("dst/globtrk"); 
	    fDisplay->AddName("dst/point(id_track,position[0]:position[1]:charge)");

	    // Set Filter and tables
	    if (fMenuGlobal->IsEntryChecked(M_SET_GLOBAL_TRACKS))  
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_GLOBAL_TRACKS,1);
	    else if (fMenuGlobal->IsEntryChecked(M_SET_GLOBAL_HITS)) 
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_GLOBAL_HITS,1);
	    else if (fMenuGlobal->IsEntryChecked(M_SET_GLOBAL_ALL)) 
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_GLOBAL_ALL,1);
	    else if (fMenuGlobal->IsEntryChecked(M_SET_GLOBAL_SVT)) 
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_GLOBAL_SVT,1);
	    if (fMenuPrim->IsEntryChecked(M_SET_PRIM_TRACKS)) 
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_PRIM_TRACKS,1);
	    else if (fMenuPrim->IsEntryChecked(M_SET_PRIM_HITS)) 
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_PRIM_HITS,1);
	    else if (fMenuPrim->IsEntryChecked(M_SET_PRIM_ALL)) 
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_PRIM_ALL,1);
	    else if (fMenuPrim->IsEntryChecked(M_SET_PRIM_SVT)) 
	      ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_PRIM_SVT,1);

	    fBFChain->Init();

	  }
	  bfc(fBFChain, fFirstEvent, evt);
	  previousEvt = evt;
	  if (fFilter)
	    fFilter->SetId_globtrk(0,0);
	}
	break;
	*/

      case M_RUN_DST:
	evt = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	if (evt < 0 || !fFirstEvent)
	  cout << "NO event to read DST! Get one..." << endl;
	else if (evt == previousEvtDst && fDstChain)
	  break;
	else {
	  if (!fDstChain) {
	    if (!fileNameDst) {
	      fi.fFileTypes = (const char **)filetypes;
	      new TGFileDialog(fClient->GetRoot(), p, kFDOpen,&fi);
	      if (!fi.fFilename) break;
	      fileNameDst = fi.fFilename;
	    }

	    fMainChain->cd();
	    fDstChain = new StChain("dstTrack");
	    fDstChain->cd();
	    StIOMaker *IOMk = new StIOMaker("inputStream","r",fileNameDst);
	    IOMk->SetBranch("dstBranch",0,"r");
 	    IOMk->SetDebug();
	    readerMaker =  new StEventMaker("events","title");
	    readerMaker->SetDebug();
	    fDstChain->Init();
	  }

	  bfc(fDstChain, fFirstEvent, evt);
	  fSvtMonitor->setStEvent((StEvent *)readerMaker->GetInputDS("StEvent"));
	  previousEvtDst = evt;
	}
	break;

      case M_RESET_EVT:
	fSvtMonitor->resetHist();
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(0);
	//	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Clear();
	break;
	
      case M_RESET_BUFFER:
	fSvtMonitor->resetBuffers();
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(-1,0);
	//	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Clear();
	break;
	
      case M_RESET_PED:
	fSvtMonitor->resetPed();
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile("NULL");
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->ResetStat();
	break;
	
      case M_RESET_PEDRMS:
	fSvtMonitor->resetPedRMS();
	fSvtGuiMonitor->GetSvtView()->SetInfoPedFile("NULL");
	((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->ResetStat();
	break;
	
      case M_QUIT:
	//fChain->Finish();
	CloseWindow();   // p also terminates theApp
	break;
	
//******************************************************	
//    Data Menu 
//******************************************************

      case M_FILE:
	fMenuSource->CheckEntry(M_FILE);
	fMenuControl->EnableEntry(M_FILE_OPEN);
	fMenuControl->EnableEntry(M_GET_EVT_N);
	fMenuControl->DisableEntry(M_SET_EVP);
	fMenuSource->UnCheckEntry(M_POOL); 
	break;

      case M_POOL:
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_SET_EVP,0);
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_EVT,0);
	fSvtMonitor->setEvent(0);
	fMenuSource->CheckEntry(M_POOL); 
	fMenuSource->UnCheckEntry(M_FILE);
	fMenuControl->DisableEntry(M_FILE_OPEN);
	fMenuControl->DisableEntry(M_GET_EVT_N);
	fMenuControl->EnableEntry(M_SET_EVP);
	break;

      case M_DATA_RAW:
	fSvtMonitor->resetHist();
	fMenuSource->CheckEntry(M_DATA_RAW); 
	fMenuSource->UnCheckEntry(M_DATA_ZS);
	fMenuSource->UnCheckEntry(M_DATA_SEQ);
	fMenuSingEvtHist->EnableEntry(M_PED);
	fMenuSingEvtHist->EnableEntry(M_PED_2D);
	fMenuSingEvtHist->EnableEntry(M_CMN);
	fMenuSingEvtHist->EnableEntry(M_CMN_2D);
	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->SetDataType("RAW");
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetDataType("RAW");
	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->UpdateReader();
	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Reset();
	fSvtMonitor->resetBuffers();
	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->SetSvtData();
	PedMaker->SetSvtData();
	SeqAdjMaker->SetSvtData();
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetSvtData();
	if (fFirstEvent) {
	  //if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetSvtData()==3) return 0;
	  if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Make()==3) return 0;
	  ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->PrintEventInfo();
	  ((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetData();
	  fSvtMonitor->setStatistics();
	}
	DrawHist();
	break;

      case M_DATA_ZS:
	fSvtMonitor->resetHist();
	fMenuSource->UnCheckEntry(M_DATA_RAW); 
	fMenuSource->CheckEntry(M_DATA_ZS);
	fMenuSource->UnCheckEntry(M_DATA_SEQ);
	fMenuSingEvtHist->DisableEntry(M_PED);
	fMenuSingEvtHist->DisableEntry(M_PED_2D);
	fMenuSingEvtHist->DisableEntry(M_CMN);
	fMenuSingEvtHist->DisableEntry(M_CMN_2D);
	fMenuMultEvtHist->DisableEntry(2);
	fMenuMultEvtHist->DisableEntry(3);
	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->SetDataType("ZS");
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetDataType("ZS");
	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->UpdateReader();
	//((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Reset();
	//fSvtMonitor->resetBuffers();
	((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->SetSvtData();
	PedMaker->SetSvtData();
	SeqAdjMaker->SetSvtData();
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetSvtData();
	if (fFirstEvent) {
	  //if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetSvtData()==3) return 0;
	  if(((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->Make()==3) return 0;
	  ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->PrintEventInfo();
	  ((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetData();
	  fSvtMonitor->setStatistics();
	}
	DrawHist();
	break;

      case M_DATA_SEQ:
	fSvtMonitor->resetHist();
	fMenuSource->UnCheckEntry(M_DATA_RAW); 
	fMenuSource->UnCheckEntry(M_DATA_ZS);
	fMenuSource->CheckEntry(M_DATA_SEQ);
	fMenuSingEvtHist->DisableEntry(M_PED);
	fMenuSingEvtHist->DisableEntry(M_PED_2D);
	fMenuSingEvtHist->DisableEntry(M_CMN);
	fMenuSingEvtHist->DisableEntry(M_CMN_2D);
	//fSvtMonitor->resetBuffers();
	if(!seqCalled && fFirstEvent) {
	  ((StSvtSeqAdjMaker*)fChain->GetMaker("SvtSeqAdj"))->Make();
	  seqCalled = kTRUE;
	  fSvtMonitor->setStatistics();
	}
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetDataType("ADJ");
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetSvtData();
	((StSvtQAMaker*)fChain->GetMaker("SvtQA"))->SetData();
	DrawHist();
	break;

      case M_RAW_STAT:
	if (fMenuStat->IsEntryChecked(M_RAW_STAT)) {
	  fMenuStat->UnCheckEntry(M_RAW_STAT); 
	  fSvtMonitor->setkRaw(kFALSE);
	}
	else {
	  fMenuStat->CheckEntry(M_RAW_STAT); 
	  fSvtMonitor->setkRaw(kTRUE);
	}
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(-1,0);
	break;

      case M_ZS_STAT:
	if (fMenuStat->IsEntryChecked(M_ZS_STAT)) {
	  fMenuStat->UnCheckEntry(M_ZS_STAT); 
	  fSvtMonitor->setkZS(kFALSE);
	}
	else {
	  fMenuStat->CheckEntry(M_ZS_STAT); 
	  fSvtMonitor->setkZS(kTRUE);
	}
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(-1,0);
	break;

      case M_PIXEL_STAT:
	if (fMenuStat->IsEntryChecked(M_PIXEL_STAT)) {
	  fMenuStat->UnCheckEntry(M_PIXEL_STAT); 
	  fSvtMonitor->setkPixelStat(kFALSE);
	}
	else {
	  fMenuStat->CheckEntry(M_PIXEL_STAT); 
	  fSvtMonitor->setkPixelStat(kTRUE);
	}
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(-1,0);
	break;

      case M_GLOBAL_STAT:
	if (fMenuStat->IsEntryChecked(M_GLOBAL_STAT)) {
	  fMenuStat->UnCheckEntry(M_GLOBAL_STAT); 
	  fSvtMonitor->setkGlobalStat(kFALSE);
	}
	else {
	  fMenuStat->CheckEntry(M_GLOBAL_STAT); 
	  fSvtMonitor->setkGlobalStat(kTRUE);
	}
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(-1,0);
	break;

      case M_SUB_PED:
	if (fMenuStat->IsEntryChecked(M_SUB_PED)) {
	  fMenuStat->UnCheckEntry(M_SUB_PED); 
	  fSvtMonitor->setkPedSub(kFALSE);
	}
	else {
	  fMenuStat->CheckEntry(M_SUB_PED); 
	  fSvtMonitor->setkPedSub(kTRUE);
	}
	break;

      case M_SUB_CMN:
	if (fMenuStat->IsEntryChecked(M_SUB_CMN)) {
	  fMenuStat->UnCheckEntry(M_SUB_CMN); 
	  fSvtMonitor->setkCMNSub(kFALSE);
	}
	else {
	  fMenuStat->CheckEntry(M_SUB_CMN); 
	  fSvtMonitor->setkCMNSub(kTRUE);
	}
	break;

      case M_EVT_HIS:
	if (fMenuStat->IsEntryChecked(M_EVT_HIS)) {
	  fMenuStat->UnCheckEntry(M_EVT_HIS); 
	  fSvtMonitor->setkEventStat(kFALSE);
	  fMenuMultEvtHist1->DisableEntry(M_MEAN_EVENT);
	  fMenuMultEvtHist1->DisableEntry(M_RMS_EVENT);
	  fMenuMultEvtHist1->DisableEntry(M_MEAN_VS_EVENT);
	  fMenuMultEvtHist1->DisableEntry(M_RMS_VS_EVENT);
	  fMenuMultEvtHist2->DisableEntry(M_MEAN_PED_EVENT);
	  fMenuMultEvtHist2->DisableEntry(M_RMS_PED_EVENT);
	  fMenuMultEvtHist2->DisableEntry(M_MEAN_PED_VS_EVENT);
	  fMenuMultEvtHist2->DisableEntry(M_RMS_PED_VS_EVENT);
	  fMenuMultEvtHist3->DisableEntry(M_MEAN_CMN_EVENT);
	  fMenuMultEvtHist3->DisableEntry(M_RMS_CMN_EVENT);
	  fMenuMultEvtHist3->DisableEntry(M_MEAN_CMN_VS_EVENT);
	  fMenuMultEvtHist3->DisableEntry(M_RMS_CMN_VS_EVENT);
	}
	else {
	  fMenuStat->CheckEntry(M_EVT_HIS); 
	  fSvtMonitor->setkEventStat(kTRUE);
	  fMenuMultEvtHist1->EnableEntry(M_MEAN_EVENT);
	  fMenuMultEvtHist1->EnableEntry(M_RMS_EVENT);
	  fMenuMultEvtHist1->EnableEntry(M_MEAN_VS_EVENT);
	  fMenuMultEvtHist1->EnableEntry(M_RMS_VS_EVENT);
	  fMenuMultEvtHist2->EnableEntry(M_MEAN_PED_EVENT);
	  fMenuMultEvtHist2->EnableEntry(M_RMS_PED_EVENT);
	  fMenuMultEvtHist2->EnableEntry(M_MEAN_PED_VS_EVENT);
	  fMenuMultEvtHist2->EnableEntry(M_RMS_PED_VS_EVENT);
	  fMenuMultEvtHist3->EnableEntry(M_MEAN_CMN_EVENT);
	  fMenuMultEvtHist3->EnableEntry(M_RMS_CMN_EVENT);
	  fMenuMultEvtHist3->EnableEntry(M_MEAN_CMN_VS_EVENT);
	  fMenuMultEvtHist3->EnableEntry(M_RMS_CMN_VS_EVENT);
	}
	fSvtGuiMonitor->GetSvtView()->SetInfoEvent(-1,0);
	break;

      case M_PED_TIME:
	if (fMenuSource->IsEntryChecked(M_PED_TIME)) {
	  fMenuSource->UnCheckEntry(M_PED_TIME); 
	  fSvtMonitor->setkPedTime(kFALSE);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_PED_CAP,0);
	}
	else {
	  fMenuSource->CheckEntry(M_PED_TIME); 
	  if (fMenuSource->IsEntryChecked(M_PED_CAP))
	    fMenuSource->UnCheckEntry(M_PED_CAP); 
	  ((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->SetType(kTime);
	  fSvtMonitor->setkPedTime(kTRUE);
	}
	break;

      case M_PED_CAP:
	if (fMenuSource->IsEntryChecked(M_PED_CAP)) {
	  fMenuSource->UnCheckEntry(M_PED_CAP); 
	  fSvtMonitor->setkPedCap(kFALSE);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_PED_TIME,0);
	}
	else {
	  fMenuSource->CheckEntry(M_PED_CAP); 
	  if (fMenuSource->IsEntryChecked(M_PED_TIME))
	    fMenuSource->UnCheckEntry(M_PED_TIME); 
	  ((StSvtPedMaker*)fChain->GetMaker("SvtPed"))->SetType(kCapacitor);
	  fSvtMonitor->setkPedCap(kTRUE);
	}
	break;

      case M_WRITE_ASCII:
	fileName = GetStringDialog("File Name","histoContent.txt");
	if (TString(fileName) != "-999")
	  fSvtMonitor->fillHist(fileName);
	break;

//******************************************************	
//    Canvas Menu 
//******************************************************

      case M_CREATE:
	if(!fCanvas1) {
	  fCanvas1 = new TCanvas("Canvas1","Canvas 1",1);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_1,0);
	}
	else if (!fCanvas2) {
	  fCanvas2 = new TCanvas("Canvas2","Canvas 2",1);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_2,0);
	}
	else if (!fCanvas3) {
	  fCanvas3 = new TCanvas("Canvas3","Canvas 3",1);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_3,0);
	}
	else if (!fCanvas4) {
	  fCanvas4 = new TCanvas("Canvas4","Canvas 4",1);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_4,0);
	}
	else if (!fCanvas5) {
	  fCanvas5 = new TCanvas("Canvas5","Canvas 5",1);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_5,0);
	}
	else
	  cout << "All 5 Canvas have already been created! That's enough..." << endl;
	break;
	
      case M_DELETE:
	if (fCanvas == fCanvas1) { 
	  delete fCanvas1;
	  fCanvas1 = 0;
	  if (fCanvas2)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_2,0);
	  else if (fCanvas3)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_3,0);
	  else if (fCanvas4)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_4,0);
	  else if (fCanvas5)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_5,0);
	  else {
	    fCanvas = 0;
	    fMenuCanvas->UnCheckEntry(M_CANVAS_1); 
	  }
	} 
	else if (fCanvas == fCanvas2) { 
	  delete fCanvas2;
	  fCanvas2 = 0;
	  if (fCanvas1)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_1,0);
	  else if (fCanvas3)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_3,0);
	  else if (fCanvas4)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_4,0);
	  else if (fCanvas5)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_5,0);
	  else {
	    fCanvas = 0;
	    fMenuCanvas->UnCheckEntry(M_CANVAS_2); 
	  }
	} 
	else if (fCanvas == fCanvas3) { 
	  delete fCanvas3;
	  fCanvas3 = 0;
	  if (fCanvas2)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_2,0);
	  else if (fCanvas1)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_1,0);
	  else if (fCanvas4)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_4,0);
	  else if (fCanvas5)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_5,0);
	  else {
	    fCanvas = 0;
	    fMenuCanvas->UnCheckEntry(M_CANVAS_3); 
	  }
	} 
	else if (fCanvas == fCanvas4) { 
	  delete fCanvas4;
	  fCanvas4 = 0;
	  if (fCanvas3)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_3,0);
	  else if (fCanvas2)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_2,0);
	  else if (fCanvas1)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_1,0);
	  else if (fCanvas5)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_5,0);
	  else {
	    fCanvas = 0;
	    fMenuCanvas->UnCheckEntry(M_CANVAS_4); 
	  }
	} 
	else if (fCanvas == fCanvas5) { 
	  delete fCanvas5;
	  fCanvas5 = 0;
	  if (fCanvas4)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_4,0);
	  else if (fCanvas3)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_3,0);
	  else if (fCanvas2)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_2,0);
	  else if (fCanvas1)
	    ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_CANVAS_1,0);
	  else {
	    fCanvas = 0;
	    fMenuCanvas->UnCheckEntry(M_CANVAS_5); 
	  }
	} 
	else
	  cout << "NO Canvas activated !!!" << endl;
	break;

      case M_DIVIDE:
	range = GetIntegerRange("Number of X divisions:"," Number of Y divisions:",1,1);
	if (range) {
	  nBinsX = int(range/10000);
	  nBinsY = range - nBinsX*10000;

	  gPad->Clear();
	  fCanvas->Divide(nBinsX,nBinsY);
	  maxPadCanvas[iCanvas-1] = nBinsX*nBinsY;
	  iPadCanvas[iCanvas-1] = 1;	  
	}

	break;
	
      case M_SAME:
	if (fMenuCanvas->IsEntryChecked(M_SAME)) {
	  if (TString(mDrawOption) == "Xsame")
	    mDrawOption = "X";
	  else if (TString(mDrawOption) == "Ysame")
	    mDrawOption = "Y";
	  else
	  mDrawOption = "";
	  fMenuCanvas->UnCheckEntry(M_SAME);
	} 
	else {
	  if (TString(mDrawOption) == "X")
	    mDrawOption = "Xsame";
	  else if (TString(mDrawOption) == "Y")
	    mDrawOption = "Ysame";
	  else
	  mDrawOption = "same";
	  fMenuCanvas->CheckEntry(M_SAME);
	}
	break;

      case M_CANVAS_1:
	if (fCanvas1) {
	  fCanvas = fCanvas1;
	  fCanvas->cd();
	  iCanvas = 1;
	  
	  fMenuCanvas->CheckEntry(M_CANVAS_1); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_2); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_3); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_4); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_5); 
	}
	break;
	
      case M_CANVAS_2:
	if (fCanvas2) {
	  fCanvas = fCanvas2;
	  fCanvas->cd();
	  iCanvas = 2;
	  
	  fMenuCanvas->UnCheckEntry(M_CANVAS_1); 
	  fMenuCanvas->CheckEntry(M_CANVAS_2); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_3); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_4); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_5); 
	}
	break;
	
      case M_CANVAS_3:
	if (fCanvas3) {
	  fCanvas = fCanvas3;
	  fCanvas->cd();
	  iCanvas = 3;
	  
	  fMenuCanvas->UnCheckEntry(M_CANVAS_1); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_2); 
	  fMenuCanvas->CheckEntry(M_CANVAS_3); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_4); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_5); 
	}
	break;
	
      case M_CANVAS_4:
	if (fCanvas4) {
	  fCanvas = fCanvas4;
	  fCanvas->cd();
	  iCanvas = 4;
	  
	  fMenuCanvas->UnCheckEntry(M_CANVAS_1); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_2); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_3); 
	  fMenuCanvas->CheckEntry(M_CANVAS_4); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_5); 
	}
	break;
	
      case M_CANVAS_5:
	if (fCanvas5) {
	  fCanvas = fCanvas5;
	  fCanvas->cd();
	  iCanvas = 5;
	  
	  fMenuCanvas->UnCheckEntry(M_CANVAS_1); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_2); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_3); 
	  fMenuCanvas->UnCheckEntry(M_CANVAS_4);
	  fMenuCanvas->CheckEntry(M_CANVAS_5); 
	} 
	break;
	
//******************************************************	
//    Edit Menu 
//******************************************************

      case M_REBIN:	
	range = GetIntegerRange("Number of X bins:"," Number of Y bins:",fSvtMonitor->getHist()->GetNbinsX(),fSvtMonitor->getHist()->GetNbinsY());
	if (range) {
	  nBinsX = int(range/10000);
	  nBinsY = range - nBinsX*10000;
	  fSvtMonitor->reBin(nBinsX,nBinsY);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_MAX,0);
	}
	break;

      case M_REBIN_ALL_HYB:
	range = GetIntegerRange("Number of X bins:"," Number of Y bins:",240,128);
	if (range) {
	  nBinsX = int(range/10000);
	  nBinsY = range - nBinsX*10000;
	  fSvtMonitor->reBinAllHybrids(nBinsX,nBinsY);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_MAX_ALL_HYB,0);
	}
	break;

      case M_REBIN_ALL_HISTOS:
	range = GetIntegerRange("Number of X bins:"," Number of Y bins:",240,128);
	if (range) {
	  nBinsX = int(range/10000);
	  nBinsY = range - nBinsX*10000;
	  fSvtMonitor->reBinAllHistos(nBinsX,nBinsY);
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RESET_MAX_ALL_HYB,0);
	}
	break;

      case M_SET_MAX:
	if (fSvtMonitor->getHist()) {
	  max = fSvtMonitor->getHist()->GetMaximum();
	  max = GetFloatDialog("Set Maximum:",max);
	  if (max != -999) {
	    fSvtMonitor->getHist()->SetMaximum(max);
	    DrawHist();
	  }
	}
	else if (fSvtMonitor->getGraph()) {
	  max = fSvtMonitor->getGraph()->GetHistogram()->GetMaximum();
	  max = GetFloatDialog("Set Maximum:",max);
	  if (max != -999) {
	    fSvtMonitor->getHybridGraph()->setMaximum(max);
	    DrawHist();
	  }
	}
	break;

      case M_SET_MAX_ALL_HYB:
	if (fSvtMonitor->getSvtHist()) {
	  max = fSvtMonitor->getSvtHist()->getMaximum();
	  max = GetFloatDialog("Set Maximum for All Hybrids:",max);
	  if (max != -999) {
	    fSvtMonitor->getSvtHist()->setMaximum(max);
	    DrawHist();
	  }
	}
	else if (fSvtMonitor->getSvtGraph()) {
	  max = fSvtMonitor->getSvtGraph()->getMaximum();
	  max = GetFloatDialog("Set Maximum:",max);
	  if (max != -999) {
	    fSvtMonitor->getSvtGraph()->setMaximum(max);
	    DrawHist();
	  }
	}
	break;

      case M_RESET_MAX:
	if (fSvtMonitor->getHist()) {
	  fSvtMonitor->getHist()->SetMaximum();
	  DrawHist();
	}
	else if (fSvtMonitor->getGraph()) {
	  fSvtMonitor->getHybridGraph()->setMaximum();
	  DrawHist();
	}
	break;

      case M_RESET_MAX_ALL_HYB:
	if (fSvtMonitor->getSvtHist()) {
	  fSvtMonitor->getSvtHist()->setMaximum();
	  DrawHist();
	}
	else if (fSvtMonitor->getSvtGraph()) {
	  fSvtMonitor->getSvtGraph()->setMaximum();
	  DrawHist();
	}
	break;

      case M_SET_MIN:
	if (fSvtMonitor->getHist()) {
	  min = fSvtMonitor->getHist()->GetMinimum();
	  min = GetFloatDialog("Set Minimum:",min);
	  if (min != -999) {
	    fSvtMonitor->getHist()->SetMinimum(min);
	    DrawHist();
	  }
	}
	else if (fSvtMonitor->getGraph()) {
	  min = fSvtMonitor->getGraph()->GetHistogram()->GetMinimum();
	  min = GetFloatDialog("Set Minimum:",min);
	  if (min != -999) {
	    fSvtMonitor->getHybridGraph()->setMinimum(min);
	    DrawHist();
	  }
	}
	break;

      case M_SET_MIN_ALL_HYB:
	if (fSvtMonitor->getSvtHist()) {
	  min = fSvtMonitor->getSvtHist()->getMinimum();
	  min = GetFloatDialog("Set Minimum for All Hybrids:",min);
	  if (min != -999) {
	    fSvtMonitor->getSvtHist()->setMinimum(min);
	    DrawHist();
	  }
	}
	else if (fSvtMonitor->getSvtGraph()) {
	  min = fSvtMonitor->getSvtGraph()->getMinimum();
	  min = GetFloatDialog("Set Minimum:",min);
	  if (min != -999) {
	    fSvtMonitor->getSvtGraph()->setMinimum(min);
	    DrawHist();
	  }
	}
	break;

      case M_RESET_MIN:
	if (fSvtMonitor->getHist()) {
	  fSvtMonitor->getHist()->SetMinimum();
	  DrawHist();
	}
	else if (fSvtMonitor->getGraph()) {
	  fSvtMonitor->getHybridGraph()->setMinimum();
	  DrawHist();
	}
	break;

      case M_RESET_MIN_ALL_HYB:
	if (fSvtMonitor->getSvtHist()) {
	  fSvtMonitor->getSvtHist()->setMinimum();
	  DrawHist();
	}
	else if (fSvtMonitor->getSvtGraph()) {
	  fSvtMonitor->getSvtGraph()->setMinimum();
	  DrawHist();
	}
	break;

      case M_SET_TRACK_PRIM:
	fSvtMonitor->setkPrimary(kTRUE);
	fSvtMonitor->getSvtHist(10)->reset();
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_TRACKS,0);
	fMenuTrack->CheckEntry(M_SET_TRACK_PRIM); 
	fMenuTrack->UnCheckEntry(M_SET_TRACK_GLOBAL); 
	break;

      case M_SET_TRACK_GLOBAL:
	fSvtMonitor->setkGlobal(kTRUE);
	fSvtMonitor->getSvtHist(10)->reset();
	ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_TRACKS,0);
	fMenuTrack->CheckEntry(M_SET_TRACK_GLOBAL); 
	fMenuTrack->UnCheckEntry(M_SET_TRACK_PRIM); 
	break;

      case M_SET_SEQADJ:
	range = GetIntegerRange("Lower ADC threshold:"," Number of Time Bins above Lower Threshold:",0,0);
	thresh_lo = int(range/10000);
	n_seq_lo = range - thresh_lo*10000;

	range = GetIntegerRange("Higher ADC threshold:"," Number of Time Bins above Higher Threshold:",0,0);
	thresh_hi = int(range/10000);
	n_seq_hi = range - thresh_hi*10000;

	pedOffset = GetIntegerDialog("Enter pedestal Offset:",0);

	SeqAdjMaker->SetMinAdcLevels(thresh_lo,n_seq_lo,thresh_hi,n_seq_hi,pedOffset);
	if (fMenuSource->IsEntryChecked(M_DATA_SEQ) && fFirstEvent) {
	  fSvtMonitor->resetHist();
	  SeqAdjMaker->Make();
	  seqCalled = kTRUE;
	  fSvtMonitor->setStatistics();
	  DrawHist();
	}
	else
	  seqCalled = kFALSE;

	cout << thresh_lo << "  " << n_seq_lo << "  " << thresh_hi << "  " << n_seq_hi << "  " << pedOffset << endl;

	break;

      case M_SET_FLAGS:
	fFlags = GetStringDialog("Set BFC flags",fFlags);
	cout << fFlags << endl;
	break;

      case M_SET_GLOBAL_HITS:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kTRUE);
	  fFilter->SetPrimary(kFALSE);
	  fFilter->SetAllTracks(kTRUE);
	  fFilter->SetTracksHitSvt(kFALSE);
	  fFilter->SetDrawHits(kTRUE);
	  fFilter->SetDrawTracks(kFALSE);
	}
	fMenuGlobal->CheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_SVT); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_SVT); 
	break;

      case M_SET_GLOBAL_TRACKS:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kTRUE);
	  fFilter->SetPrimary(kFALSE);
	  fFilter->SetAllTracks(kTRUE);
	  fFilter->SetTracksHitSvt(kFALSE);
	  fFilter->SetDrawHits(kFALSE);
	  fFilter->SetDrawTracks(kTRUE);
	}
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->CheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_SVT); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_SVT); 
	break;

      case M_SET_GLOBAL_ALL:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kTRUE);
	  fFilter->SetPrimary(kFALSE);
	  fFilter->SetAllTracks(kTRUE);
	  fFilter->SetTracksHitSvt(kFALSE);
	  fFilter->SetDrawHits(kTRUE);
	  fFilter->SetDrawTracks(kTRUE);
	}
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->CheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_SVT); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_SVT); 
	break;

      case M_SET_GLOBAL_SVT:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kTRUE);
	  fFilter->SetPrimary(kFALSE);
	  fFilter->SetAllTracks(kFALSE);
	  fFilter->SetTracksHitSvt(kTRUE);
	  fFilter->SetDrawHits(kTRUE);
	  fFilter->SetDrawTracks(kTRUE);
	}
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->CheckEntry(M_SET_GLOBAL_SVT); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_SVT); 
	break;

      case M_SET_PRIM_HITS:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kFALSE);
	  fFilter->SetPrimary(kTRUE);
	  fFilter->SetAllTracks(kTRUE);
	  fFilter->SetTracksHitSvt(kFALSE);
	  fFilter->SetDrawHits(kTRUE);
	  fFilter->SetDrawTracks(kFALSE);
	}
	fMenuPrim->CheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_SVT); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_SVT); 
	break;

      case M_SET_PRIM_TRACKS:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kFALSE);
	  fFilter->SetPrimary(kTRUE);
	  fFilter->SetAllTracks(kTRUE);
	  fFilter->SetTracksHitSvt(kFALSE);
	  fFilter->SetDrawHits(kFALSE);
	  fFilter->SetDrawTracks(kTRUE);
	}
	fMenuPrim->UnCheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->CheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_SVT); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_SVT); 
	break;

      case M_SET_PRIM_ALL:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kFALSE);
	  fFilter->SetPrimary(kTRUE);
	  fFilter->SetAllTracks(kTRUE);
	  fFilter->SetTracksHitSvt(kFALSE);
	  fFilter->SetDrawHits(kTRUE);
	  fFilter->SetDrawTracks(kTRUE);
	}
	fMenuPrim->UnCheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->CheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_SVT); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_SVT); 
	break;

      case M_SET_PRIM_SVT:
	if (fFilter) {
	  fFilter->SetId_globtrk(0,0);
	  fFilter->SetGlobal(kFALSE);
	  fFilter->SetPrimary(kTRUE);
	  fFilter->SetAllTracks(kFALSE);
	  fFilter->SetTracksHitSvt(kTRUE);
	  fFilter->SetDrawHits(kTRUE);
	  fFilter->SetDrawTracks(kTRUE);
	}
	fMenuPrim->UnCheckEntry(M_SET_PRIM_HITS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_TRACKS); 
	fMenuPrim->UnCheckEntry(M_SET_PRIM_ALL); 
	fMenuPrim->CheckEntry(M_SET_PRIM_SVT); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_HITS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_TRACKS); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_ALL); 
	fMenuGlobal->UnCheckEntry(M_SET_GLOBAL_SVT); 
	break;

//******************************************************	
//    View Menu 
//******************************************************

      case M_PROJ_X:
	range = GetIntegerRange("First Y bin:"," Last Y bin:",1,128);
	if (range) {
	  mHist = 1001;
	  firstBin = int(range/10000)+1;
	  lastBin = range - (firstBin-1)*10000+1;
	  DrawHist();
	  fMenuView->UnCheckEntry(M_PROJ_Y);
	  fMenuView->UnCheckEntry(M_PROJ_X_ALL);
	  fMenuView->UnCheckEntry(M_PROJ_Y_ALL);
	  fMenuView->CheckEntry(M_PROJ_X);
	}
	break;

      case M_PROJ_Y:
	range = GetIntegerRange("First X bin:"," Last X bin:",1,240);
	if (range) {
	  mHist = 1002;
	  firstBin = int(range/10000);
	  lastBin = range - firstBin*10000;
	  DrawHist();
	  fMenuView->UnCheckEntry(M_PROJ_X);
	  fMenuView->UnCheckEntry(M_PROJ_X_ALL);
	  fMenuView->UnCheckEntry(M_PROJ_Y_ALL);
	  fMenuView->CheckEntry(M_PROJ_Y);
	}
	break;

      case M_PROJ_X_ALL:
	mHist = 1003;
	DrawHist();
	fMenuView->UnCheckEntry(M_PROJ_X);
	fMenuView->UnCheckEntry(M_PROJ_Y);
	fMenuView->UnCheckEntry(M_PROJ_Y_ALL);
	fMenuView->CheckEntry(M_PROJ_X_ALL);
	break;

      case M_PROJ_Y_ALL:
	mHist = 1004;
	DrawHist();
	fMenuView->UnCheckEntry(M_PROJ_X);
	fMenuView->UnCheckEntry(M_PROJ_Y);
	fMenuView->UnCheckEntry(M_PROJ_X_ALL);
	fMenuView->CheckEntry(M_PROJ_Y_ALL);
	break;

      case M_FOURIER:
	if (fMenuView->IsEntryChecked(M_FOURIER))
	  fMenuView->UnCheckEntry(M_FOURIER);
	else
	  fMenuView->CheckEntry(M_FOURIER);
	DrawHist();
	break;

      case M_LADDER:
	if (fSvtMonitor->getSvtHist())
	  config = TString(fSvtMonitor->getSvtHist()->getConfiguration());
	//else if (fSvtMonitor->getSvtGraph())
	//  config = TString(fSvtMonitor->getSvtGraph()->getConfiguration());
	else
	  config = "NULL";

	cout << config << endl;

	if ((config != "BARREL") && (config != "SVT") && (config != "NULL")) {
	  if (mHist == 1001)
	    if (TString(mDrawOption) == "same")
	      mDrawOption = "Xsame";
	    else
	      mDrawOption = "X";
	  if (mHist == 1002) 
	    if (TString(mDrawOption) == "same")
	      mDrawOption = "Ysame";
	    else
	      mDrawOption = "Y";
	  mHist = 1005;	
	  DrawHist();
	  //UnCheckAllEntries();
	  fMenuView->CheckEntry(M_LADDER);
	}
	break;

      case M_BARREL:
	if (fSvtMonitor->getSvtHist())
	  config = TString(fSvtMonitor->getSvtHist()->getConfiguration());
	//else if (fSvtMonitor->getSvtGraph())
	//  config = TString(fSvtMonitor->getSvtGraph()->getConfiguration());
	else
	  config = "NULL";
	
	if (config == "BARREL") {
	  mHist = 1006;	
	  DrawHist();
	  //UnCheckAllEntries();
	  fMenuView->CheckEntry(M_BARREL);
	}
	break;

	/*
      case M_3D:
	if ((fMenuView->IsEntryChecked(M_3D)) && (!parm2)) {
	  fMenuView->UnCheckEntry(M_3D);
	  if (fBFChain) {
	    delete fFilter;
	    fFilter = NULL;
	    delete fDisplay;
	    fDisplay = NULL;
	    //delete fBFChain;
	    fBFChain = NULL;
	    delete f3DCanvas;
	  }
	}
	else {
	  fMenuView->CheckEntry(M_3D);
	  //evt = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	  //if ((evt == previousEvt) && fBFChain) {
	  //  fDisplay->ReDraw();
	  //}
	  //else
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RUN_BFC,0);
	  f3DCanvas = (TCanvas*)gPad;
	  fCanvas->cd();
	}
	break;
	*/

      case M_CLU:
	if (fCanvas) {
	  if (f3DCanvas)
	    fCanvas->cd();
	  if (fMenuSource->IsEntryChecked(M_DATA_SEQ)) {
	    if (mHist != 2) {
	      mHist = 2;
	      fSvtMonitor->setHistID(mHist);
	      DrawHist();
	      UnCheckAllEntries();
	      fMenuSingEvtHist->CheckEntry(M_RAW_2D);
	    }
	  }
	  else {
	    if (mHist != 4) {
	      mHist = 4;
	      fSvtMonitor->setHistID(mHist);
	      DrawHist();
	      UnCheckAllEntries();
	      fMenuSingEvtHist->CheckEntry(M_PED_2D);
	    }
	  }
	  if (!fFirstEvent){
	    cout << "No event in buffer get one.." << endl;
	    break;
	  }

	  if( !clustersColl){

	    if (!seqCalled) {
	      ((StSvtSeqAdjMaker*)fChain->GetMaker("SvtSeqAdj"))->Make();
	      seqCalled = kTRUE;
	    }
	    ((StSvtClusterMaker*)fChain->GetMaker("SvtClu"))->Make();
	    ((StSvtClusterAnalysisMaker*)fChain->GetMaker("SvtCluAna"))->Make();
	    clustersColl = (StSvtHybridCollection *) (((StSvtClusterAnalysisMaker*)fChain->GetMaker("SvtCluAna"))->
						      GetDataSet("StSvtAnalResults"))->GetObject();    
	  }  
  
	  fSvtGuiMonitor->drawClusters(clustersColl);
	  fCanvas->Update();
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;

      case M_TRACKS:
	evt = ((StSvtDaqMaker*)fChain->GetMaker("SvtDaq"))->GetEventNumber();
	if ((evt != previousEvtDst) || (!fDstChain))
	  ProcessMessage(this,MK_MSG(kC_COMMAND, kCM_MENU),M_RUN_DST,0);

	if ((mHist != 2) || (mHist != 4)) {
	  if (fMenuSource->IsEntryChecked(M_DATA_SEQ))
	    mHist = 2;
	  else
	    mHist = 4;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  if (fMenuSource->IsEntryChecked(M_DATA_SEQ))
	    fMenuSingEvtHist->CheckEntry(M_RAW_2D);
	  else
	    fMenuSingEvtHist->CheckEntry(M_PED_2D);
	}
	tempHist = mHist;
	mHist = 10;
	fSvtMonitor->setHistID(mHist);
	DrawHist();
	mHist = tempHist;
	fSvtMonitor->setHistID(mHist);

	break;
	
//******************************************************	
//    Single Event Histos Menu 
//******************************************************

      case M_RAW:
	if (fCanvas) {
	  UnCheckAllEntries();
	  if (mHist == 1005) {
	    fSvtMonitor->setHistID(1);
	    fMenuView->CheckEntry(M_LADDER);
	  }
	  else {
	    mHist = 1;
	    fSvtMonitor->setHistID(mHist);
	  }
	  DrawHist();
	  fMenuSingEvtHist->CheckEntry(M_RAW);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RAW_2D:
	if (fCanvas) {
	  mHist = 2;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_RAW_2D);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_PED:
	if (fCanvas) {
	  mHist = 3;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_PED);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_PED_2D:
	if (fCanvas) {
	  mHist = 4;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_PED_2D);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_CMN:
	if (fCanvas) {
	  mHist = 5;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_CMN);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_CMN_2D:
	if (fCanvas) {
	  mHist = 6;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_CMN_2D);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;

      case M_AVE_ANDS:
	if (fCanvas) {
	  mHist = 7;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_AVE_ANDS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_AVE_TIME:
	if (fCanvas) {
	  mHist = 8;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_AVE_TIME);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_PEDESTAL:
	if (fCanvas) {	  
	  mHist = 301;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_PEDESTAL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PEDESTAL:
	if (fCanvas) {	  
	  mHist = 302;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_RMS_PEDESTAL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_PED_HYB:
	if (fCanvas) {	  
	  mHist = 303;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_PED_HYB);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_HYB:
	if (fCanvas) {	  
	  mHist = 304;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_RMS_PED_HYB);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_PED_ALL:
	if (fCanvas) {
	  mHist = 355;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_PED_ALL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_ALL:
	if (fCanvas) {
	  mHist = 351;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_RMS_PED_ALL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_LADDER:
	if (fCanvas) {	  
	  mHist = 352;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_RMS_PED_LADDER);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_BARREL:
	if (fCanvas) {	  
	  mHist = 354;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_RMS_PED_BARREL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_HALF:
	if (fCanvas) {	  
	  mHist = 353;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuPedHist->CheckEntry(M_RMS_PED_HALF);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;

      case M_N_PIXELS_PED:
	if (fCanvas) {
	  mHist = 51;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_N_PIXELS_PED);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_N_PIXELS_ADC:
	if (fCanvas) {
	  mHist = 52;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_N_PIXELS_ADC);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_N_PIXELS_PED_HYB:
	if (fCanvas) {
	  mHist = 54;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_N_PIXELS_PED_HYB);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_N_PIXELS_ADC_HYB:
	if (fCanvas) {
	  mHist = 55;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_N_PIXELS_ADC_HYB);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_N_PIXELS_PED_HALF:
	if (fCanvas) {
	  mHist = 56;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_N_PIXELS_PED_HALF);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_N_PIXELS_ADC_HALF:
	if (fCanvas) {
	  mHist = 57;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_N_PIXELS_ADC_HALF);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RAW_ALL:
	if (fCanvas) {
	  mHist = 53;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuSingEvtHist->CheckEntry(M_RAW_ALL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
//******************************************************	
//    Multi Events Histos Menu 
//******************************************************

      case M_MEAN:
	if (fCanvas) {
	  mHist = 101;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_MEAN);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS:
	if (fCanvas) {
	  mHist = 102;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_PIXELS:
	if (fCanvas) {
	  mHist = 103;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_MEAN_PIXELS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PIXELS:
	if (fCanvas) {
	  mHist = 104;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS_PIXELS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_EVENT:
	if (fCanvas) {
	  mHist = 201;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_MEAN_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_EVENT:
	if (fCanvas) {
	  mHist = 202;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_VS_EVENT:
	if (fCanvas) {
	  mHist = 107;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_MEAN_VS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_VS_EVENT:
	if (fCanvas) {
	  mHist = 108;
	  fSvtMonitor->setHistID(mHist);
	  
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS_VS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_ALL:
	if (fCanvas) {
	  mHist = 151;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS_ALL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_LADDER:
	if (fCanvas) {
	  mHist = 152;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS_LADDER);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_HALF:
	if (fCanvas) {
	  mHist = 153;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS_HALF);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_BARREL:
	if (fCanvas) {
	  mHist = 154;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist1->CheckEntry(M_RMS_BARREL);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_PED:
	if (fCanvas) {
	  mHist = 111;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_MEAN_PED);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED:
	if (fCanvas) {
	  mHist = 112;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_RMS_PED);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_PED_PIXELS:
	if (fCanvas) {
	  mHist = 113;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_MEAN_PED_PIXELS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_PIXELS:
	if (fCanvas) {
	  mHist = 114;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_RMS_PED_PIXELS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_PED_EVENT:
	if (fCanvas) {
	  mHist = 115;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_MEAN_PED_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_EVENT:
	if (fCanvas) {
	  mHist = 211;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_RMS_PED_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_PED_VS_EVENT:
	if (fCanvas) {
	  mHist = 212;
	  fSvtMonitor->setHistID(mHist);
	  
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_MEAN_PED_VS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_PED_VS_EVENT:
	if (fCanvas) {
	  mHist = 118;
	  fSvtMonitor->setHistID(mHist);
	  
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist2->CheckEntry(M_RMS_PED_VS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_CMN:
	if (fCanvas) {
	  mHist = 121;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_MEAN_CMN);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_CMN:
	if (fCanvas) {
	  mHist = 122;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_RMS_CMN);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_CMN_PIXELS:
	if (fCanvas) {
	  mHist = 123;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_MEAN_CMN_PIXELS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_CMN_PIXELS:
	if (fCanvas) {
	  mHist = 124;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_RMS_CMN_PIXELS);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_CMN_EVENT:
	if (fCanvas) {
	  mHist = 221;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_MEAN_CMN_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_CMN_EVENT:
	if (fCanvas) {
	  mHist = 222;
	  fSvtMonitor->setHistID(mHist);
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_RMS_CMN_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_MEAN_CMN_VS_EVENT:
	if (fCanvas) {
	  mHist = 127;
	  fSvtMonitor->setHistID(mHist);
	  
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_MEAN_CMN_VS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_RMS_CMN_VS_EVENT:
	if (fCanvas) {
	  mHist = 128;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist3->CheckEntry(M_RMS_CMN_VS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;

      case M_N_PIXELS_STAT_PED:
	if (fCanvas) {
	  mHist = 251;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist4->CheckEntry(M_N_PIXELS_STAT_PED);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;

      case M_N_PIXELS_STAT_ADC:
	if (fCanvas) {
	  mHist = 252;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist4->CheckEntry(M_N_PIXELS_STAT_ADC);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;

      case M_MEAN_ANODE:
	if (fCanvas) {
	  mHist = 253;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist4->CheckEntry(M_MEAN_ANODE);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;

      case M_MEAN_TIME:
	if (fCanvas) {
	  mHist = 254;
	  fSvtMonitor->setHistID(mHist);

	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist4->CheckEntry(M_MEAN_TIME);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      case M_N_PIXELS_VS_EVENT:
	if (fCanvas) {
	  mHist = 1000;
	  fSvtMonitor->setHistID(mHist);
	  
	  DrawHist();
	  UnCheckAllEntries();
	  fMenuMultEvtHist4->CheckEntry(M_N_PIXELS_VS_EVENT);
	}
	else
	  cout << "NO Canvas activated !!!" << endl;		    
	break;
	
      default:
	break;
      }
    default:
      break;
    }
  default:
    break;
  }
  return kTRUE;
}


