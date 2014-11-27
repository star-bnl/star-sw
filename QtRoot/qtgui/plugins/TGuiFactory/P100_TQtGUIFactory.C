void P100_TQtGUIFactory()
{
   gPluginMgr->AddHandler("TGuiFactory", "qtgui", "TQtGUIFactory",
      "QtRootGui", "TQtGUIFactory()");
}
