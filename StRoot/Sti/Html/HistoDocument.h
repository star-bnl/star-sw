#ifndef HistoDocument_H
#define HistoDocument_H
#include "Sti/Html/HtmlDocument.h"
#include "Sti/Base/HistogramGroup.h"

class HistoDocument : public HtmlDocument
{
public:
  HistoDocument(const string & targetDirectory,
		const string & documentFileName,
		const string & documentTitle,
		TCanvas * canvas);
  virtual ~HistoDocument();
  virtual void generateWebPage(HistogramGroup * histogramGroup);
  virtual void generateWebPage(HistogramGroup * histogramGroup1, 
			       HistogramGroup * histogramGroup2, 
			       int mode=0);
  virtual void generateWebPage(HistogramGroup * histogramGroup1, 
			       HistogramGroup * histogramGroup2,
			       HistogramGroup * histogramGroup3,
			       int mode=0);
  virtual void generateHistoEntry(const string & dir, TH1*histo);
  virtual void generateHistoEntry(const string & dir, TH1*histo1, TH1*histo2, int mode);
  virtual void generateHistoEntry(const string & dir, TH1*histo1, TH1*histo2, TH1*histo3, int mode);
  virtual const string saveHistoAsEpsFile(const string & dir, const string &histoName);
  virtual const string saveHistoAsGifFile(const string & dir, const string &histoName);
  virtual const string saveHistoAsTableFile(const string & dir, TH1*histo);


protected:
  HistogramGroup * _group;
  string   _directory;
  TCanvas  * _canvas;
};


#endif
