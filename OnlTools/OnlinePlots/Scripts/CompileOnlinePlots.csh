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

Online.eemcMask:  /home_local/eemc/defaultPanitkinSetup/eemcTwMask.dat
Online.eemcDbDump:  /home_local/eemc/defaultPanitkinSetup/eemcDbDump.dat
Online.eemcPathIn:  /home_local/eemc/defaultPanitkinSetup
Online.eemcPathOut:  /onlineweb/www/eemc2005pplot
Online.bemcStatus:  /home_local/bemc/bemcStatus.txt
Online.InputPath:    /a
Online.OutputPath:   /a/pplot/histos/
Online.ProjectPath:  .

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
__ROOTRC__



echo "--- Creating the ROOT macro"
cat >EvpPlotServer.C<<__ROOTMACRO__
{
  gROOT->Macro("Load.C");
  gSystem->Load("RTS");
  gSystem->Load("StDaqLib");
  gSystem->Load("OnlinePlots");
  gSystem->Load("StDbLib");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
  gSystem->Load("StEEmcPoolmuEztPanitkin");
  gSystem->Load("StBEMCPlots");
  gSystem->Load("StEvent");
  const char *homedir = gSystem->Getenv("ONLINEPLOTSDIR");
  if (!homedir) printf(" Attention: The environment variable \"ONLINEPLOTSDIR\" has not been set yet !!!\n");
  char* args[] = {
                "-path"
                ,"/a/9057008/"
                , "-map"
                , "test.map"
                , "-nocheck"
                , "-nogui"
                , "-start" 
                };
//  evpServerMain::main(sizeof(args)/sizeof(char*), args);
//  evpServerMain::main(int argc, const char* argv[]);
//  evpMainPresenter::main();
}
__ROOTMACRO__


cat  EvpPlotServer.C

echo ""
echo "Do not forget to check/set the ONLINEPLOTSDIR environment variable properly"
echo "For example:"
echo "setenv ONLINEPLOTSDIR `pwd`/OnlTools/OnlinePlots/"
echo "-----------------------"
echo "Use: rootn.exe EvpPlotServer.C"
echo "-------------------------------"

setenv ONLINEPLOTSDIR `pwd`/OnlTools/OnlinePlots/
