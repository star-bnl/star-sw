#include "TH1.h"
#include "TCanvas.h"
#include "HtmlDocument.h"
#include "HtmlHeading.h"
#include "HtmlList.h"
#include "HtmlListItem.h"
#include "HtmlAnchor.h"
#include "HtmlString.h"
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
  for (iter1=histogramGroup1->begin(), 
	 iter2=histogramGroup2->begin();
       iter1!=histogramGroup1->end();
       ++iter1,++iter2)
    {
      //cout << "HistoDocument::generateWebPage() -I- Generate enrty for:"<<(*iter1)->GetName()<<endl;
      generateHistoEntry(_directory,(*iter1),(*iter2),mode);
    } 
  save();
}

void HistoDocument::generateWebPage(HistogramGroup * histogramGroup1,
				    HistogramGroup * histogramGroup2,
				    HistogramGroup * histogramGroup3,
				    int mode)
{
  vector<TH1*>::iterator iter1;
  vector<TH1*>::iterator iter2;
  vector<TH1*>::iterator iter3;
  for (iter1=histogramGroup1->begin(), 
	 iter2=histogramGroup2->begin(), 
	 iter3=histogramGroup3->begin();
       iter1!=histogramGroup1->end();
       ++iter1,++iter2,++iter3)
    {
      generateHistoEntry(_directory,(*iter1),(*iter2),(*iter3),mode);
    } 
  save();
}

void HistoDocument::generateHistoEntry(const string & dir, TH1*histo)
{
  addHeading("2",histo->GetTitle());
  histo->SetStats(00000);
  histo->SetTitle("");
  TH2D* h2d = dynamic_cast<TH2D*>(histo);
  if (h2d)
    h2d->Draw("COLOR");
  else
    histo->Draw();
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
  string heading = histo1->GetTitle();
  addHeading("2",heading);
  histo1->SetTitle("");
  histo2->SetTitle("");
  histo1->SetStats(000000);
  histo2->SetStats(000000);
  histo1->SetLineColor(2); //red
  histo2->SetLineColor(4); //blue
  histo1->SetMarkerColor(2); //red
  histo2->SetMarkerColor(4); //blue

  TH2D* h2d = dynamic_cast<TH2D*>(histo1);
  if (h2d)
    {
      histo1->Draw(); 
      histo2->Draw("SAME");
    }
  else
    {
      histo1->SetMarkerStyle(20);
      histo2->SetMarkerStyle(25);
      histo1->Draw();
      histo2->Draw("SAME");
    }
  string histoName1 = histo1->GetName(); 
  string histoName2 = histo2->GetName(); 
  string fileNameEps = saveHistoAsEpsFile(dir,histoName1);
  string fileNameGif = saveHistoAsGifFile(dir,histoName1);
  string fileNameTable1 = saveHistoAsTableFile(dir,histo1);
  string fileNameTable2 = saveHistoAsTableFile(dir,histo2);
  HtmlList * list = new HtmlList();
  add(list);
  HtmlListItem * item;

  item = new HtmlListItem();
  item->add(new HtmlString(histoName1+": shown in RED"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlString(histoName2+": shown in BLUE"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameGif,"Image in GIF Format"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameEps,"Image in EPS Format"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameTable1,histoName1+" : Data in Table Format"));
  list->add(item);

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameTable2,histoName2+" : Data in Table Format"));
  list->add(item);

  add(new HtmlImage(fileNameGif));
}


void HistoDocument::generateHistoEntry(const string & dir, TH1*histo1, TH1*histo2,TH1*histo3, int mode)
{
  string heading = histo1->GetTitle();
  histo1->SetTitle("");
  histo2->SetTitle("");
  histo3->SetTitle("");
  histo1->SetStats(00000);
  histo2->SetStats(00000);
  histo3->SetStats(00000);
  histo1->SetLineColor(2); //red
  histo2->SetLineColor(4); //blue
  histo3->SetLineColor(6); //?
  histo1->SetMarkerColor(2); //red
  histo2->SetMarkerColor(4); //blue
  histo3->SetMarkerColor(6); //?
  histo1->Draw(); 
  histo2->Draw("SAME");
  histo3->Draw("SAME");
  addHeading("2",heading);
  string histoName1 = histo1->GetName(); 
  string histoName2 = histo2->GetName(); 
  string histoName3 = histo3->GetName(); 
  string fileNameEps = saveHistoAsEpsFile(dir,histoName1);
  string fileNameGif = saveHistoAsGifFile(dir,histoName1);
  string fileNameTable1 = saveHistoAsTableFile(dir,histo1);
  string fileNameTable2 = saveHistoAsTableFile(dir,histo2);
  string fileNameTable3 = saveHistoAsTableFile(dir,histo3);
  HtmlList * list = new HtmlList();
  add(list);
  HtmlListItem * item;
  item = new HtmlListItem();
  item->add(new HtmlString(histoName1+": shown in RED"));
  list->add(item);
 
  item = new HtmlListItem();
  item->add(new HtmlString(histoName2+": shown in BLUE"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlString(histoName3+": shown in PURPLE"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameGif,"Image in GIF Format"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameEps,"Image in EPS Format"));
  list->add(item); 

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameTable1,histoName1+" : Data in Table Format"));
  list->add(item);

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameTable2,histoName2+" : Data in Table Format"));
  list->add(item);

  item = new HtmlListItem();
  item->add(new HtmlAnchor(fileNameTable3,histoName3+" : Data in Table Format"));
  list->add(item);

  add(new HtmlImage(fileNameGif));
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
  return histoName+"_table.html";
}

