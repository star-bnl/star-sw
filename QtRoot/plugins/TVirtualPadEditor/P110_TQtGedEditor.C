void P110_TQtGedEditor()
{
   TString qtgui = gEnv->GetValue("Gui.Factory","native");
   if (qtgui == "qtgui") {
      gPluginMgr->AddHandler("TVirtualPadEditor", "Ged", "TQtGedEditor",
         "QtGed", "TQtGedEditor(TCanvas*)");
   }
}
