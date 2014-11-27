#ifndef ROOT_TQtCanvas2Html
#define ROOT_TQtCanvas2Html

//
// class TQtCanvas2Html  converts the TCanvas object into the clickable
// Html map
// It creates TWO files:
// HTML wrapper and pixmap image.
//
#include "TQtPad2Html.h"
//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// class TQtCanvas2Html  converts the TCanvas object into the clickable     //
//   Html map                                                               //
//                                                                          //
// If TCanvas contains <N> TPads then this class constructor is to produce  //
// 2*(N+1) files                                                            //
//                                                                          //
// TCanvas is to be repsented by clickable HTML map and each TPad           //
// is to be represent by the html wrapper and the pixel file in PNG format  //
// Each pad is magnified by the TQtZoomPadWidget provided.                  //
//                                                                          //
// To create the Web pages from the current TPad / TCanvas invoke           //
// from your code  / ROOT macro:                                            //
// TQtCanvas2Html(gPad);                                                    //
//                                                                          //
// The regular user may not create this object at all.                      //
//                                                                          //
// He/she can save his /her TCanvas as the clickable html map and           //
// set of the magnified images via TCanvas "Save As Web page" file menu     //
// begin_html  <a href="png/ntuple1"><img src="png/SaveAsWeb.png"> </a> end_html  //
// (one needs the Qt-base TCanvas implementation from qtgui package)         //
//                                                                          //
// For example, with one click the for the TPad::Divide(6,16)               //
// this class creates the begin_html <a href="png/SaveAsWeb"> Web site</a> end_html  //
// One can select the TCanvas "zoomer" to adjust the  zoom factor of  TPad pages //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

class TQtZoomPadWidget;
class TCanvas;
class TQtWidget;

class TQtCanvas2Html : public  TQtPad2Html {
protected:
     QString          *fTargetWindow;
     TQtZoomPadWidget *fZoomer;
     TCanvas          *fZoomCanvas;
     bool              fOwnZoom;// do we own fZoomer;
     QString          *fUUID; // Unique ID to compare the Web pages


     virtual  int  CreateMapPage(TVirtualPad *pad);
     virtual  void CreateRectArea(const QString &name,const QString &title,int uleft_x, int uleft_y, int lright_x, int lright_y);
     virtual  void MakePage(float zoom=1.8,unsigned int width=0, unsigned int height=0);
     virtual  void MapTag(const char *name);

public:
     TQtCanvas2Html(TVirtualPad *pad=0,  float zoom=1.8, const char *folder=0, TQtZoomPadWidget *zoomer=0);
     TQtCanvas2Html(TQtWidget *canvas, float zoom=1.8, const char *folder=0, TQtZoomPadWidget *zoomer=0);
     TQtCanvas2Html(TVirtualPad *pad,  unsigned int width, unsigned int height, const char *folder=0, TQtZoomPadWidget *zoomer=0);
     TQtCanvas2Html(TQtWidget *canvas, unsigned int width, unsigned int height, const char *folder=0, TQtZoomPadWidget *zoomer=0);
     virtual ~TQtCanvas2Html();
};
#endif
