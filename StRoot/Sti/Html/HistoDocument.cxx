#include "TH1.h"
#include "TCanvas.h"
#include "HtmlDocument.h"
#include "HtmlHeading.h"
#include "HtmlList.h"
#include "HtmlListItem.h"
#include "HtmlAnchor.h"
#include "Sti/Html/HistoDocument.h"

HistoDocument::HistoDocument(const string & targetDirectory,
			     const string & documentFileName,
			     const string & documentTitle,
			     TCanvas * canvas)
  : HtmlDocument(targetDirectory+"/"+documentFileName,documentTitle,documentTitle),
    _directory(targetDirectory),
    _canvas(canvas)
{}

HistoDocument::~HistoDocument()
{}

void HistoDocument::generateWebPage(HistogramGroup * histogramGroup)
{
  vector<TH1*>::iterator iter;
  for (iter=histogramGroup->begin();
       iter!=histogramGroup->end();
       ++iter)
    {
      generateHistoEntry(_directory,(*iter));
    } 
  save();
}

void HistoDocument::generateWebPage(HistogramGroup * histogramGroup1,
				    HistogramGroup * histogramGroup2,
				    int mode)
{
  vector<TH1*>::iterator iter1;
  vector<TH1*>::iterator iter2;
  for (iter1=histogramGroup1->begin(), iter2=histogramGroup2->begin();
       iter1!=histogramGroup1->end();
       ++iter1,++iter2)
    {
      generateHistoEntry(_directory,(*iter1),(*iter2),mode);
    } 
  save();
}

void HistoDocument::generateHistoEntry(const string & dir, TH1*histo)
{
  histo->Draw();
  addHeading("2",histo->GetTitle());
  string histoName = histo->GetName(); 
  string fileNameEps   = saveHistoAsEpsFile(dir,histoName);
  string fileNameGif   = saveHistoAsGifFile(dir,histoName); 
  string fileNameTable = saveHistoAsTableFile(dir,histo);
  HtmlList * list = new HtmlList();
  add(list);
  HtmlListItem * item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameGif,"Histo GIF Format"));
  list->add(item); 
  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameEps,"Histo EPS Format"));
  list->add(item); 
  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameTable,"Histo Table Format"));
  list->add(item);
  add(new HtmlImage(fileNameGif));
}

void HistoDocument::generateHistoEntry(const string & dir, TH1*histo1, TH1*histo2, int mode)
{
  /*  if (mode==0)
    {
      histo1->Draw(); 
      histo2->Draw("SAME");
      string heading = histo1->GetTitle();
      heading += " vs ";
      heading += histo2->GetTitle();
      addHeading("2",heading);
      string histoName = histo1->GetName(); 
      histoName += " vs ";
      histoName += histo2->GetName(); 
      string fileNameEps = saveHistoAsEpsFile(dir,histoName);
      string fileNameGif = saveHistoAsGifFile(dir,histoName);
      string fileNameTable = saveHistoAsTableFile(dir,histo);
      HtmlList * list = new HtmlList();
      add(list);
      HtmlListItem * item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameGif,"Histo GIF Format"));
      list->add(item); 
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameEps,"Histo EPS Format"));
      list->add(item); 
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameTable,"Histo Table Format"));
      list->add(item);
      add(new HtmlImage(fileNameGif));
    }
  else
    {
      string heading = histo1->GetTitle();
      heading += " vs ";
      heading += histo2->GetTitle();
      addHeading("2",heading);
      histo1->Draw(); 
      string histoName1 = histo1->GetName(); 
      string fileNameEps1 = saveHistoAsEpsFile(dir,histoName1);
      string fileNameGif1 = saveHistoAsGifFile(dir,histoName1);
      string fileNameTable2 = saveHistoAsTableFile(dir,histo1);
      string histoName2 = histo2->GetName(); 
      string fileNameEps2 = saveHistoAsEpsFile(dir,histoName2);
      string fileNameGif2 = saveHistoAsGifFile(dir,histoName2);
      string fileNameTable2 = saveHistoAsTableFile(dir,histo2);
      HtmlList * list = new HtmlList();
      add(list);
      HtmlListItem * item;
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameGif1,"Histo GIF Format"));  list->add(item); 
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameEps1,"Histo EPS Format"));  list->add(item); 
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameTable1,"Histo Table Format")); list->add(item);
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameGif2,"Histo GIF Format"));  list->add(item); 
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameEps2,"Histo EPS Format"));  list->add(item); 
      item = new HtmlListItem();
      item->add(new HtmlAnchor(fileNameTable2,"Histo Table Format")); list->add(item);
      add(new HtmlImage(fileNameGif1));
      add(new HtmlImage(fileNameGif1));
      }*/
}

const string HistoDocument::saveHistoAsEpsFile(const string & dir, const string& histoName)
{
  string fileName = dir;
  fileName +="/";
  fileName +=histoName;
  fileName += ".eps"; 
  _canvas->Print(fileName.c_str());
  return histoName+".eps";
}

const string HistoDocument::saveHistoAsGifFile(const string & dir, const string& histoName)
{
  string fileName = dir;
  fileName +="/";
  fileName +=histoName;
  fileName += ".gif"; 
  _canvas->Print(fileName.c_str());
  return histoName+".gif";
}

const string HistoDocument::saveHistoAsTableFile(const string & dir, TH1*histo)
{
  string fileName = dir;
  string histoName = histo->GetName();
  fileName +="/";
  fileName +=histoName;
  fileName += "_table"; 
  
  HtmlDocument * doc = new HtmlDocument(fileName,histoName,histoName);
  doc->addTable();
  char indexS[20];
  char valueS[128];
  char errorS[128];
  for (int i=0;i<histo->GetNbinsX();++i)
    {
      doc->addTableRow();
      sprintf(indexS,"%d",i);
      sprintf(valueS,"%f +- %f",histo->GetBinContent(i), histo->GetBinError(i));
      HtmlTableCell * cell = doc->addTableCell();
      cell->add(indexS);
      cell = doc->addTableCell();
      cell->add(valueS);
    }
  doc->save();
  return histoName+"_table.htm";
}

