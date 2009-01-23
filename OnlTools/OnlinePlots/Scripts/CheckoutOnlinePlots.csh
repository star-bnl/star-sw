# Check out the new OnlinePlots  to offline build tree
#  cd $SCRATCH # use the SCRATCH to test this script only.

mkdir -p StOnline/OnlinePlots
cd StOnline
# 
echo -- Checkout  OnlinePlots/Infrastructure
cvs co -d OnlinePlots/Infrastructure   online/RTS/src/OnlinePlots/Infrastructure
echo -- Checkout  OnlinePlots/QEvpClient
cvs co -d OnlinePlots/QEvpClient     online/RTS/src/OnlinePlots/QEvpClient
echo --  Clean up the QEvpClient
cd OnlinePlots/QEvpClient
      rm -rf EventInfoUi.ui.h
      rm -rf QEvpClient.pro
      rm -rf threadTest
      mv  hello.cxx hello.C
      mv  evpPresenter.cxx  evpPresenter.C
      mv  threadTest.cxx threadTest.C
cd ../..

echo -- Checkout  OnlinePlots/HistogramGroups
cvs co -d OnlinePlots/HistogramGroups  online/RTS/src/OnlinePlots/HistogramGroups
cd ..

echo -- Checkout  OnlinePlots/Resources
cvs co -d Resources    online/RTS/src/OnlinePlots/Resources

echo -- Checkout  OnlinePlots/Scripts
cvs co -d Scripts      online/RTS/src/OnlinePlots/Scripts


echo Use CompileOnlinePlots.csh script to build the PPlot system
echo -----------------------------------------------------------
cat >.rootrc <<ROOTRC
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
ROOTRC
