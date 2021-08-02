#!/bin/csh
#
# Check out the new OnlinePlots to build tree
# 
        
set macroFile = EvpPlotServer.C

echo "--- Compiling PPlots libraries"
cons

echo "--- Cleaning up"
/usr/bin/test -e $macroFile && /bin/rm -f $macroFile
/usr/bin/test -e .rootrc && /bin/rm -f .rootrc

set filesDir = "/star/u/starqa/OnlineQA/files"
@ on_evp = `/bin/uname -n | /bin/grep -c evp.starp`
if ($on_evp > 0) then
  set filesDir = "/a/pplot/files"
endif

# prepare .rootrc
echo "--- Generating a .rootrc file"
/bin/cat > .rootrc <<__ROOTRC__
# Author: Valeri Fine 10/10/2003
# Online parameters

Online.GuiRefreshRate:  50

Online.tofConfigPath:   ${filesDir}/tof/
Online.bemcStatus:   ${filesDir}/bemc/bemcStatus.txt
Online.eemcMask:     ${filesDir}/eemc/defaultPanitkinSetup/eemcTwMask.dat
Online.eemcDbDump:   ${filesDir}/eemc/defaultPanitkinSetup/eemcDbDump.dat
Online.eemcPathIn:   ${filesDir}/eemc/defaultPanitkinSetup/
Online.eemcPathOut:  /onlineweb/www/eemc2005pplot
Online.InputPath:    /a
Online.OutputPath:   /a/pplot/histos/
Online.ProjectPath:  OnlTools/OnlinePlots/
Online.endOfRun:     /home/evpops/EndOfRunScript
Online.plotsDir:     `pwd`/OnlTools/OnlinePlots/
Online.Reference:    /a/pplot/histos/run10029077.root
Online.testProc:     /a/pplot/bin/test_proc.csh

Plugin.TVirtualPadEditor: Ged TQtGedEditor    QtGed          "TQtGedEditor(TCanvas*)"

#Plugin.TPaletteEditor:      *  TQtPaletteEditor  QtImage  "TQtPaletteEditor(TAttImage*,UInt_t,UInt_t)"
#Plugin.TImage:              *  TQtImage          QtImage  "TQtImage()"
# GUI specific settings
# Gui.Style:                  sgi
Gui.Backend:                qt
Gui.Factory:                qtgui
#
Plugin.TVirtualX:        qt     TGQt             GQt       "TGQt(const char*,const char*)"
Plugin.TGuiFactory:      qtgui  TQtGUIFactory    QtRootGui "TQtGUIFactory()"
Plugin.TVirtualViewer3D: ogl    TQtRootViewer3D  RQTGL    "TQtRootViewer3D(TVirtualPad*)"
+Plugin.TVirtualViewer3D: oiv   TQtRootCoinViewer3D  RQIVTGL    "TQtRootCoinViewer3D(TVirtualPad*)"

#Local codes on evp:
+Unix.*.Root.MacroPath:	:/home/evpops/LOCAL_CODES/macros
__ROOTRC__



echo "--- Creating the ROOT macro $macroFile "
echo "---  (macro must be edited to use as server or presenter)"
echo "---  (macro must be run using root4starN)"
/bin/cat > $macroFile <<__ROOTMACRO__

{
  gROOT->Macro("Load.C");
  gSystem->Load("RTS");
  if (gROOT->IsBatch()) {
     gSystem->Load("libQtRootGui.so");
  }
  gSystem->Load("StDaqLib");
  gSystem->Load("OnlinePlots");
  gSystem->Load("StDbLib");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StEEmcPoolmuEztPanitkin");
  gSystem->Load("StBEMCPlots");

  char* argsS[] = {
                 "-path",
                 //"st_physics_adc_10022038_raw_1330001.daq",
		 "/a/9057008/",
                 "-map",
                 "./test.map",
                 "-nocheck",
                 "-nogui",
                 "-start" 
                 };


  char* argsP[] = {
                 "-path",
                 "./test.map"
                 };

  printf("Server=%d / Presenter=%d arguments\n",sizeof(argsS)/sizeof(char*),sizeof(argsP)/sizeof(char*));
  printf("  Start the server     [] evpServerMain::main(%d, argsS);\n",sizeof(argsS)/sizeof(char*));
  printf("  Start the presenter  [] evpMainPresenter::main(%d,argsP);\n",sizeof(argsP)/sizeof(char*));

//  evpServerMain::main(sizeof(argsS)/sizeof(char*), argsS);    // starts server
//  evpMainPresenter::main(sizeof(argsP)/sizeof(char*),argsP);  // start presenter
}
__ROOTMACRO__


#/bin/cat  EvpPlotServer.C
