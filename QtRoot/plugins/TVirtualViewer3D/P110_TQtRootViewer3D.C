void P110_TQtRootViewer3D()
{
   gPluginMgr->AddHandler("TVirtualViewer3D", "ogl", "TQtRootViewer3D",
      "RQTGL", "TQtRootViewer3D(TVirtualPad*)");
}
