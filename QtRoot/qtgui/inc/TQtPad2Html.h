#ifndef ROOT_TQtPad2Html
#define ROOT_TQtPad2Html

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

////////////////////////////////////////////////////////////////////////////
//                                                                        //
// class QtPad2Html  converts the TPad object into the Html format        //
//                                                                        //
// It creates TWO files:                                                  //
// HTML wrapper with "html" extenstion and pixmap image in "png" format   //
//                                                                        //
// To create  a Web page from the current TPad / TCanvas do               //
//                                                                        //
// TQtPad2Html(gPad);  It should be sufficient                            //
//                                                                        //
// This class is used internally by TQtCanvas2Html object                 //
// to convert multi-pad TCanvas into the HTML image map                   //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

class TVirtualPad;
class QTextStream;
class TObject;
class QFile;
class QString;
class TH1;
class TIter;

class TQtPad2Html {
private:
     QString *fAuthorName;
     QString *fAuthorEmail;
     QString *fFullHtmlName;
     QString *fFullImageName;
     QString *fHtmlFolder;
     TVirtualPad *fPad;
     QTextStream *fHtml;
     QFile       *fFile;

protected:
   TQtPad2Html(TVirtualPad *pad,bool,const QString &folder);
   virtual QTextStream &EndTag(const QString &tagName);
   virtual QTextStream &Eol();
   virtual QTextStream &Html();
   virtual QTextStream &HtmlTag(const char* tagName, const char *tagParameters=0, bool closed=false);
   virtual QTextStream &Quote();
   virtual QTextStream &MapHistograms(TVirtualPad *pad);
   virtual void Close();

  public:
     TQtPad2Html(TVirtualPad *pad=0,const char *folder = 0);
     virtual ~TQtPad2Html();
     const QString &AuthorName();
     const QString &AuthorEMail();
     virtual void  ClosePage();
     virtual QString ImageTitle(TVirtualPad *pad=0);
     virtual QTextStream   &MapCanvas(TVirtualPad *pad, const char *mapName=0,bool adjust=true);
     virtual const QString &HtmlFolder();
     virtual QTextStream &OpenHeader(TVirtualPad *pad=0);
     virtual TVirtualPad *Pad() const;
     virtual const QString &PadHtmlFile(TVirtualPad *pad=0, const char *name =0);
     virtual const    QString &PadImageFile(TVirtualPad *pad=0, const char *name =0);
     virtual void     SetFolder(const QString &folder);
     virtual void     WriteHtmlPad(TVirtualPad *pad=0, const char *name =0);
     virtual void     WritePad(TVirtualPad *pad=0, const char *name =0);
     virtual void     MakeHistArea(const TH1 *hist,TVirtualPad *pad);
     virtual void     MakeHistArea(TIter &next,TVirtualPad *pad);
};
#endif
