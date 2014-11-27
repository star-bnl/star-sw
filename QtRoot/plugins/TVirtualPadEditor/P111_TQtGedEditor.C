void P111_TQtGedEditor()
{
   TString qtgui = gEnv->GetValue("Gui.Factory","native");
   if (qtgui == "qtgui") {
      gPluginMgr->AddHandler("TVirtualPadEditor", "QtGed", "TQtGedEditor",
         "QtGed", "TQtGedEditor(TCanvas*)");
   }
}
