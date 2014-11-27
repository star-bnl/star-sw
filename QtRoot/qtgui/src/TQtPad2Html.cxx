// @(#)root/gui:$Name:  $:$Id: TQtPad2Html.cxx,v 1.6 2013/08/30 16:00:24 perev Exp $
// Author: Valeri Fine 07/06/2006
/****************************************************************************
**
** Copyright (C) 2006 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

#include "TQtPad2Html.h"
#include <QTextStream>
#include <QFileInfo>
#include <QFile>
#include <QPixmap>
#include <QDir>
#include <QIODevice>
#include <QDebug>
#include <QString>


#include "TVirtualPad.h"
#include "TQtWidget.h"
#include "TGQt.h"
#include "TSystem.h"
#include "TIterator.h"
#include "TList.h"
#include "TObjArray.h"
#include "THStack.h"
#include "TH1.h"
// fgPrefix = "Root_Qt_Canvas";

//__________________________________________________________________________________
TQtPad2Html::TQtPad2Html(TVirtualPad *pad,bool, const QString &folder) : 
             fAuthorName(0),fAuthorEmail(0),fFullHtmlName(0),fFullImageName(0),
             fHtmlFolder(0), fPad(pad ? pad : gPad),fHtml(0)
{ 
    // dummy ctor to be called by TQtCanvas2Html 
   if (!folder.isEmpty()) SetFolder(folder); 
}

//__________________________________________________________________________________
TQtPad2Html::TQtPad2Html(TVirtualPad *pad, const char *folder) : 
             fAuthorName(0),fAuthorEmail(0),fFullHtmlName(0),fFullImageName(0),
             fHtmlFolder(0),fPad(pad ? pad : gPad),fHtml(0)
{
   if (folder && folder[0]) SetFolder(QString(folder)); 
   WriteHtmlPad(fPad); 
}

//__________________________________________________________________________________
TQtPad2Html::~TQtPad2Html()
{
   Close();
   delete fHtml;         fHtml         = 0;
   delete fAuthorName;   fAuthorName   = 0;
   delete fAuthorEmail;  fAuthorEmail  = 0;
   delete fFullHtmlName; fFullHtmlName = 0;
   delete fFullImageName;fFullImageName= 0;
   delete fHtmlFolder;   fHtmlFolder   = 0;
}

//__________________________________________________________________________________
void TQtPad2Html::Close() {
   if (fFile) {
      fFile->close();
      delete fFile; fFile = 0;
   }
}

//__________________________________________________________________________________
QTextStream &TQtPad2Html::Html() {
   if (!fHtml) {
     fFile = new QFile(PadHtmlFile(fPad));
#if QT_VERSION >= 0x40000     
     fFile->open(QIODevice::WriteOnly);
#else     
     fFile->open(IO_WriteOnly);
#endif
     fHtml = new QTextStream(fFile);
   } 
   return *fHtml;
}
//__________________________________________________________________________________
QTextStream &TQtPad2Html::HtmlTag(const char* tag, const char *tagParameters, bool closed)
{
  QString tagName = tag;
  QString par;
  if (tagParameters && tagParameters[0]) par = tagParameters;
  Html() <<  "<" << tagName;
          if (!par.isEmpty())   
             Html() <<  " " << par;
          if (closed) 
            Html() <<  "\\";
          Html() << ">";
   return Html();
}

//__________________________________________________________________________________
QTextStream &TQtPad2Html::EndTag(const QString &tagName)
{
    Html() << "</" << tagName << ">"; 
    return Eol();
}

//__________________________________________________________________________________
QTextStream &TQtPad2Html::Quote()
{
   Html() <<  "\"";
   return Html();
}
//__________________________________________________________________________________
TVirtualPad *TQtPad2Html::Pad() const
{
    return fPad;
}
//__________________________________________________________________________________
QTextStream &TQtPad2Html::Eol()
{
   Html() <<  "\n";
   return Html();
}

//__________________________________________________________________________________
QTextStream & TQtPad2Html::OpenHeader(TVirtualPad *pad)
{
  // Create header
//  fMapName = fgPrefix;
//  fMapName +=pad->GetName();
  HtmlTag("HTML"); Eol();
     HtmlTag("HEAD");  Eol();
        HtmlTag("TITLE");
          Html() << ImageTitle(pad) ;
        EndTag("TITLE"); Eol();
        HtmlTag("meta", "name=\"Author\" content=\"Valeri Fine\"");  Eol();
        HtmlTag("meta", "http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\"");  Eol();
        HtmlTag("meta", "name=\"GENERATOR\" content=\"http://root.bnl.gov\"");  Eol();
        HtmlTag("meta", "target=\"exposition\"");  Eol();
     EndTag("HEAD"); Eol();
     Eol();
  HtmlTag("BODY"); Eol();
  HtmlTag("DIV", "ALIGN=CENTER"); Eol();
  return Html();

}

//__________________________________________________________________________________
void TQtPad2Html::WriteHtmlPad(TVirtualPad *pad, const char *name)
{
   if (name) {}
   OpenHeader(pad); 
   WritePad(pad);
   MapCanvas(pad,"hist");
   MapHistograms(pad);
   ClosePage();
}

//__________________________________________________________________________________
const QString &TQtPad2Html::PadHtmlFile(TVirtualPad *pad, const char *name)
{
   // Set the Html file for the pad at once per class instance
   if (name) {}
   if (!fFullHtmlName) {
      fFullHtmlName = new QString;
      if (pad) {
         QString folder = HtmlFolder().isEmpty() ? "./" : HtmlFolder();
         QString fileName = name ? name : pad->GetName();
         fileName +=  ".html";
         *fFullHtmlName = folder + fileName;
      }
      fprintf(stderr," Create Web page: %s\n", fFullHtmlName->toLatin1().data());
   }
   return *fFullHtmlName;
}

//__________________________________________________________________________________
const QString &TQtPad2Html::PadImageFile(TVirtualPad *pad, const char *name)
{
   // Set the Image file name at once per instance
   // returns the Image file name
   if (name) {}
   if (!fFullImageName) {
      fFullImageName = new QString;
      if (pad) {
         QString folder = HtmlFolder().isEmpty() ? "./" : HtmlFolder();
         QString fileName = name ? name : pad->GetName();
         fileName +=  ".png";
         *fFullImageName = folder + fileName;
      }
   }
   return *fFullImageName;
}

//__________________________________________________________________________________
const QString &TQtPad2Html::HtmlFolder()
{
   if (!fHtmlFolder) fHtmlFolder = new QString;
   if ( !(fHtmlFolder->isEmpty() || fHtmlFolder->endsWith("/")) )
      *fHtmlFolder += "/";
   return *fHtmlFolder;
}

//__________________________________________________________________________________
void  TQtPad2Html::SetFolder(const QString &folder)
{
   // assign and create the folder if any
   if ( !folder.isEmpty() ) {
     QFileInfo info(folder);
     if (!info.exists()) {
        QDir d("."); d.mkdir(folder);
     }
   }
   if (!fHtmlFolder) fHtmlFolder = new QString;
   *fHtmlFolder = folder;
}

//__________________________________________________________________________________
void TQtPad2Html::WritePad(TVirtualPad *pad, const char *name)
{
   if (name) {}
   if (pad) {
      const QString &fileName = PadImageFile(pad,name);
      //      gVirtualX->WritePixmap(pad->GetPixmapID(),fCanvasW,fCanvasH,(const char*)fileName);
//      gVirtualX->WritePixmap(pad->GetPixmapID(),-1,-1,(char*)(const char *)fileName);
      QPixmap *pix = 0;
      if (pad->GetCanvas() ==  pad) {
         TQtWidget *fCanvasID = (TQtWidget *)TGQt::iwid(((TCanvas *)pad)->GetCanvasID());
         pix = fCanvasID->GetOffScreenBuffer();
      } else {
         pix = (QPixmap *)TGQt::iwid(pad->GetPixmapID());
      }
      pix->save(fileName,"PNG");
   }
}

//__________________________________________________________________________________
QString TQtPad2Html::ImageTitle(TVirtualPad *pad)
{
   if (!pad) pad = fPad;
   if (!fPad) pad = gPad;
   QString title;
   if (pad) {
     // look for the histogram object
     TObject *hist = 0;
     TIter next(pad->GetListOfPrimitives());
     while ( (hist = next())) {
        if (hist->InheritsFrom("TH1")) {
            title = hist->GetTitle();
            break;
        }
     }
     if (title.isEmpty()) {
         title = pad->GetTitle();
     }
   }
   return title;
}
//__________________________________________________________________________________
void   TQtPad2Html:: MakeHistArea(const TH1 *hist,TVirtualPad *pad)
{
   //  Create the POLY HTML area
   if (!pad) pad = fPad;
   if (!fPad) pad = gPad;
   // Create the POLY html AREA
   if (hist && hist->GetDimension() == 1) {
      Html() << "<AREA ";
      Html() << " ALT=";   Quote() << hist->GetTitle();     Quote();
      Html() << " TITLE="; Quote() << hist->GetTitle();     Quote();
      Html() << " SHAPE=POLY COORDS=";           Quote();
      Eol();
      Double_t xmin; Double_t ymin; Double_t xmax; Double_t ymax;
      pad->GetRangeAxis(xmin, ymin, xmax,  ymax);
      int nBins  = hist->GetNbinsX();
      Float_t w  = hist->GetBinWidth(1);
      Float_t y0 = hist->GetBinContent(1);
      Float_t x0 = hist->GetBinCenter(1)-w/2;
      Html() <<  QString("%1,%2").arg(pad->XtoPixel(x0))
                                 .arg(pad->YtoPixel(ymin));
      Float_t x = x0;
      if (ymin != y0) {
         Html() <<  QString(",%1,%2").arg(pad->XtoPixel(x0))
                                     .arg(pad->YtoPixel(y0));
      }
      Eol();
      for (int i=1; i < nBins; i++) {
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,23,00)         
         if ( hist->IsBinOverflow(i) || hist->IsBinUnderflow(i)) continue;
#endif
         Float_t y = hist->GetBinContent(i);
         if (y != y0 ) {
            // Horizontal (can be long) line
            Html() <<  QString(",%1,%2").arg(pad->XtoPixel(x))
                                    .arg(pad->YtoPixel(y0)); 
            // Vertical line
            Html() <<  QString(",%1,%2").arg(pad->XtoPixel(x))
                                        .arg(pad->YtoPixel(y));
            Eol();
            y0 = y;
            x0 = x;
         }
         x += hist->GetBinWidth(i);         
      }
      
      Html() <<  QString(",%1,%2").arg(pad->XtoPixel(x))
                                  .arg(pad->YtoPixel(y0)); 
      Eol();
      Html() <<  QString(",%1,%2").arg(pad->XtoPixel(x))
                                  .arg(pad->YtoPixel(ymin));
      Eol();

      Quote() << " href=";  
      Quote() <<"javascript:window.alert(\'";
              Html() << hist->GetTitle() << "\')";
      Quote() << " onMouseOver=";  
      Quote() << "self.status=\'"; Html() << hist->GetTitle();
                 Html()  << "\';return true";
      Quote() << ">"; 
      Eol();
   }
}
//__________________________________________________________________________________
void   TQtPad2Html::MakeHistArea(TIter &next,TVirtualPad *pad)
{ 
   // Create the HTML map from the nested list of the histograms if any
   TObject *hist = 0;
   while ( (hist = next())) {
      if (hist->InheritsFrom("TSeqCollection")) {
         TIter nextCollection((TSeqCollection *)hist,kIterBackward);
         MakeHistArea(nextCollection,pad);
      } else if (hist->InheritsFrom("THStack")) {
         TIter nextStack(((THStack*)hist)->GetStack());
         MakeHistArea(nextStack,pad);
      } else if (hist->InheritsFrom("TH1")) {
         MakeHistArea((const TH1*)hist,pad);
      }
   }
}

//__________________________________________________________________________________
QTextStream &TQtPad2Html::MapHistograms(TVirtualPad *pad)
{
    // create an extra map for the histograms
   if (!pad) pad = fPad;
   if (!fPad) pad = gPad;
   if (pad) {
      // update first (just in case)
      pad->Update();
      // look up pad
      TIter nextPrimities(pad->GetListOfPrimitives(),kIterBackward);
      MakeHistArea(nextPrimities,pad);
   }
   return Html();
}

//__________________________________________________________________________________
QTextStream &TQtPad2Html::MapCanvas(TVirtualPad *pad, const char *mapName,bool adjust) 
{
    QFileInfo info(PadImageFile(pad));
    QString fileName = info.fileName(); 
    // Create the map canvas
    Html() <<  "<IMG SRC=" ; Quote() <<  fileName;  Quote();
           Html() << " ALT=";  Quote() <<  ImageTitle(pad);  Quote();
           Html() << " BORDER=0 "; 
           if (adjust) { // disable this option for the time being
              if (pad->GetWw() / pad->GetWh() > 1) 
                  Html() << " WIDTH=100% ";
              else
                  Html() << " HEIGHT=100%";
           }
//            title.Form(" BORDER=0 WIDTH=%d HEIGHT=%d", fCanvasW,fCanvasH);
//            html += title;
          if(mapName && mapName[0]) {  // Add map reference
             Html() << " USEMAP="; Quote() <<  "#" << mapName;  Quote();
          }
         Html() << "><BR>" ; Eol();
         return Html();
}

//__________________________________________________________________________________
const QString &TQtPad2Html::AuthorName() {
   if (!fAuthorName) 
      fAuthorName = new QString(gSystem->Getenv("USER"));
   return *fAuthorName;
}

//__________________________________________________________________________________
const QString &TQtPad2Html::AuthorEMail() 
{
  if (!fAuthorEmail) {
     fAuthorEmail = new QString(AuthorName());
     *fAuthorEmail += "@bnl.gov";
  }
  return *fAuthorEmail;
}

//__________________________________________________________________________________
void TQtPad2Html::ClosePage()
{
   HtmlTag("hr");
   HtmlTag("font","size=-4");
   HtmlTag("table", "class=\"frame\" cellSpacing=\"0\" cellPadding=\"1\" width=\"100%\" border=\"0\"");
   HtmlTag("code");
   HtmlTag("tr", "class=\"light\"");
   HtmlTag("td");
   Html() << "Author:<a href=\"mailto:";
   Html() << AuthorEMail();  Quote(); 
   Html() << ">" << AuthorName(); EndTag("a");
   HtmlTag("td","align=center");
   Html() << "<a href=\"http://root.bnl.gov\">QtRoot inside"; EndTag("a");
   HtmlTag("td","align=right");
   Html() << "<a href=\"http://www.star.bnl.gov/STAR/comp/vis/StarEvent.html\">";
   Html() << "<img src=\"http://www.star.bnl.gov/include/graphics/starLogoMenu.gif\" BORDER=0 >"; EndTag("a");
   EndTag("tr");EndTag("code");EndTag("table");EndTag("font");
   EndTag("DIV");
   EndTag("BODY");
   EndTag("HTML");
   Html() << flush;
   Close();
}
