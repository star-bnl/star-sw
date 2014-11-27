void P110_TQtRootViewer3D()
{
   gPluginMgr->AddHandler("TVirtualViewer3D", "qgl", "TQtRootViewer3D",
      "RQTGL", "TQtRootViewer3D(TVirtualPad*)");
}
