#!/bin/csh
#
# Check out the new OnlinePlots  to offline build tree
# 
        
echo "--- Compiling PPlots libraries"
cons

echo "--- Cleaning up"
test -e EvpPlotServer.C && /bin/rm -f EvpPlotServer.C
test -e .rootrc && /bin/rm -f .rootrc

# prepare .rootrc
echo "--- Generating a .rootrc file"
cat >.rootrc <<__ROOTRC__
# Author: Valeri Fine 10/10/2003
# Online parameters

Online.GuiRefreshRate:  50

Online.tofConfigPath:   /a/pplot/files/tof/
Online.eemcMask:     /a/pplot/files/eemc/defaultPanitkinSetup/eemcTwMask.dat
Online.eemcDbDump:   /a/pplot/files/eemc/defaultPanitkinSetup/eemcDbDump.dat
Online.eemcPathIn:   /a/pplot/files/eemc/defaultPanitkinSetup/
Online.eemcPathOut:  /onlineweb/www/eemc2005pplot
Online.bemcStatus:   /a/pplot/files/bemc/bemcStatus.txt
Online.InputPath:    /a
Online.OutputPath:   /a/pplot/histos/
Online.ProjectPath:  OnlTools/OnlinePlots/
Online.endOfRun:     /home/operator/EndOfRunScript
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
+Unix.*.Root.MacroPath:	:/home/operator/LOCAL_CODES/macros
__ROOTRC__



echo "--- Creating the ROOT macro"
cat >EvpPlotServer.C<<__ROOTMACRO__

{
  gROOT->Macro("Load.C");
  gSystem->Load("RTS");
  if (gROOT->IsBatch()) {
     // too foolish the loaded
     gSystem->Load("libqt-mt.so");
     gSystem->Load("libGui.so");
     gSystem->Load("libGQt.so");
  }
  gSystem->Load("StDaqLib"); 
  gSystem->Load("OnlinePlots");
  gSystem->Load("StDbLib");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StEEmcPoolmuEztPanitkin");
  gSystem->Load("StBEMCPlots");
  gSystem->Load("StEvent");
  gSystem->Load("StEEmcUtil");

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


cat  EvpPlotServer.C
