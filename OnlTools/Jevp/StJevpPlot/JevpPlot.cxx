#include "JevpPlot.h"
#include <TLegend.h>
#include <TCanvas.h>
#include <TStyle.h>
#include <rtsLog.h>
#include "JLatex.h"
#include <RTS/include/SUNRT/clockClass.h>

ClassImp(PlotHisto);
ClassImp(JevpPlot);

PlotHisto::PlotHisto(TH1 *hist)
{
  histo = hist;
  legendText = NULL;
  legendArgs = NULL;
}

PlotHisto::PlotHisto(PlotHisto &x)
{
    if(x.histo) {
	//printf("There is a plothisto\n");
	//printf("plothisto name was %s\n",x.histo->GetName());
	histo = (TH1 *)x.histo->Clone();
	//printf("new plothisto name is %s\n",x.histo->GetName());
    }
    else {
	//printf("There was not plothisto\n");
	histo = NULL;
    }

    if(x.legendText) {
	legendText = new char[sizeof(x.legendText) + 1];
	strcpy(legendText, x.legendText);
    }
    else 
	legendText = NULL;

    if(x.legendArgs) {
	legendArgs = new char[sizeof(x.legendArgs) + 1];
	strcpy(legendArgs, x.legendArgs);
    }
    else 
	legendArgs = NULL;
}


void PlotHisto::setLegText(const char *text)
{
  if(legendText) delete legendText;
  legendText = new char[strlen(text)+1];
  strcpy(legendText, text);
}

void PlotHisto::setLegArgs(const char *text)
{
  if(legendArgs) delete legendText;
  legendArgs = new char[strlen(text)+1];
  strcpy(legendArgs, text);
}

// PlotHisto::PlotHisto(TH1 *hist, char *legText, char *legArgs) {
//   histo = hist;

//   if(!legText) legendText = NULL;
//   else {
//     legendText = new char[strlen(legText)+1];
//     strcpy(legendText, legText);
//   }
  
//   if(!legArgs) legendArgs = NULL;
//   else {
//     legendArgs = new char[strlen(legArgs)+1];
//     strcpy(legendArgs, legArgs);
//   }
// }

PlotHisto::~PlotHisto()
{
  LOG(DBG, "Delete histo: %s can %d must %d",histo->GetName(), histo->TestBit(kCanDelete), histo->TestBit(kMustCleanup));

  if(histo) delete histo;
  if(legendText) delete legendText;
  if(legendArgs) delete legendArgs;
}

void JevpPlot::setDrawOpts(const char *opts)
{
  if(drawopts) delete drawopts;
  drawopts = new char[strlen(opts)+1];
  strcpy(drawopts, opts);
}

JevpPlot::JevpPlot(TH1 *firsthisto) { 
  external_maxy = -9999;
  logx = 0;
  logy = 0;
  optlogz = -1;
  palette = 1;
  gridx = -1;
  gridy = -1;
  needsdata = 1;

  nhistos=0;
  legendx1 = -1;
  legendx2 = -1;
  legendy1 = -1;
  legendy2 = -1;
  optstat = 1;
  drawopts = NULL;
  xaxis_label = NULL;
  yaxis_label = NULL;
  legend = NULL;
  parent = NULL;

  run = 0;
  refid = 0;
  refcomment = NULL;
  myname[0] = '\0';

  legend = NULL;

  if(firsthisto) addHisto(firsthisto);
}

double JevpPlot::getMaxY()   // this gets the max from the bins...
{
  double maxY = -9999;

  PlotHisto *curr = (PlotHisto *)histos.First();
  while(curr) {
    if(curr->histo) {
      TH1 *h = curr->histo;
      int maxbin = h->GetMaximumBin();
      double y = h->GetBinContent(maxbin);
      if(y > maxY) maxY = y;
    }
    curr = (PlotHisto *)histos.After(curr);
  }
  return maxY;
}

void JevpPlot::setMaxY(double maxY)   // this sets the actual histogram bounds...
{
  external_maxy = maxY;
}

void JevpPlot::reset()
{
  PlotHisto *curr = (PlotHisto *)histos.First();

  while(curr) {
    if(curr->histo) curr->histo->Reset();

    curr = (PlotHisto *)histos.After(curr);
  }
}

void JevpPlot::setParent(char *par)
{
  if(parent) delete parent;
  
  parent = new char[strlen(par) + 1];
  strcpy(parent, par);
  return;
}

char *JevpPlot::getParent()
{
  return parent;
}


JevpPlot::JevpPlot(JevpPlot &x)
{
  LOG(DBG, "Copy Constructor...");

  if(x.legend) {
    legend = new TLegend(*(x.legend));
  }
  else legend = NULL;
  
  run = x.run;
  refid = x.refid;
  if(x.refcomment) {
    refcomment = new char[strlen(x.refcomment)+1];
    strcpy(refcomment, x.refcomment);
  }
  else
    refcomment = NULL;

  optstat = x.optstat;
  logx = x.logx;
  logy = x.logy;
  optlogz = x.optlogz;
  palette = x.palette;
  gridx = x.gridx;
  gridy = x.gridy;
  lastUpdate = x.lastUpdate;
  nevts = x.nevts;
  external_maxy = x.external_maxy;

  legendx1 = x.legendx1;
  legendx2 = x.legendx2;
  legendy1 = x.legendy1;
  legendy2 = x.legendy2;
  if(x.xaxis_label) {
    xaxis_label = new char[strlen(x.xaxis_label) + 1];
    strcpy(xaxis_label, x.xaxis_label);
  }
  else
    xaxis_label = NULL;
  if(x.yaxis_label) {
    yaxis_label = new char[strlen(x.yaxis_label) + 1];
    strcpy(yaxis_label, x.yaxis_label);
  }
  else
    yaxis_label = NULL;
  
  PlotHisto *curr = (PlotHisto *)x.histos.First();
  while(curr) {
    PlotHisto *nph = new PlotHisto(*curr);
    histos.Add(nph);
    curr = (PlotHisto *)x.histos.After(curr);
  }

  nhistos = x.nhistos;
  if(x.drawopts) {
    drawopts = new char[strlen(x.drawopts) + 1];
    strcpy(drawopts, x.drawopts);
  }
  else
    drawopts = NULL;

  if(x.parent) {
    parent = new char[strlen(x.parent)+1];
    strcpy(parent, x.parent);
  }
  else
    parent = NULL;

  // copy elements...
  TListIter next(&x.elements);
  TObject *obj;
  //  CP;
  while((obj = (TObject *)next())) {
    LOG(DBG, "Copying an object...");
    TObject *nobj = obj->Clone();
    addElement(nobj);
  }

  GetPlotName();
  //CP;
}


void JevpPlot::setRefComment(char *text)
{
  if(refcomment) delete refcomment;
  refcomment = new char[strlen(text)+1];
  strcpy(refcomment, text);
}

// Carefull!  This deletes the memory for the histograms...
// A strange usage, but neccessary because the JevpPlot is
// transferred via ethernet to processes that know nothing
// about the original PlotHisto memory allocations...
//
// a.  histos added must be dynamically created using "new"
// b.  histos memory is owned by JevpPlot once added
JevpPlot::~JevpPlot() {  

  PlotHisto *curr = (PlotHisto *)histos.First();

  LOG(DBG,"DESTRUCTOR FOR PLOT:  %s %d",(curr != NULL) ? curr->GetName() : "noname", refid);

  PlotHisto *next;
  while(curr) {
    next = (PlotHisto *)histos.After(curr);
    histos.Remove(curr);  // remove from list

    LOG(DBG, "Deleting PlotHisto");
    delete curr;          // delete

    curr = next;
  }

  TObject *el = (TObject *)elements.First();
  TObject *nn;
  while(el) {
    nn = (TObject *)elements.After(el);
    elements.Remove(el);
    LOG(DBG, "Deleting Element");
    delete el;
    el = nn;
  }

  LOG(DBG, "Elements.size = %d histos.size = %d",elements.GetSize(),histos.GetSize());

  if(drawopts) {
    delete drawopts;
  }
  
  if(legend) {
    LOG(DBG,"Delete legend\n");
    delete legend;
    LOG(DBG, "Done delete legend\n");
  }
  
  if(refcomment) {
    delete refcomment;
  }

  LOG(DBG,"Done with jevpplot destructor...\n");
}

void JevpPlot::addElement(TObject *obj) {
  elements.Add(obj);
}

void JevpPlot::removeElement(TObject *obj) {
  elements.Remove(obj);
  delete obj;
}

void JevpPlot::addHisto(PlotHisto *hist) {
  histos.Add(hist);
  if(hist->histo) {
    hist->histo->SetLineWidth(1);
  }
  nhistos++;
  if(nhistos == 1) GetPlotName();
}

void JevpPlot::addHisto(TH1 *roothist) {
  PlotHisto *ph = new PlotHisto(roothist);
  addHisto(ph);
}

PlotHisto *JevpPlot::getHisto(int i) {
  int idx=0;
  
  PlotHisto *curr = (PlotHisto *)histos.First();

  while(curr) {
    if(i == idx) return curr;
    idx++;
    curr = (PlotHisto *)histos.After(curr);
  }
  
  return NULL;
}

void JevpPlot::removeHisto(int i)
{
  int idx = 0;
  PlotHisto *curr = (PlotHisto *)histos.First();
  
  while(curr) {
    if(idx == i) {
      histos.Remove(curr);
      nhistos--;
      return;
    }

    curr = (PlotHisto *)histos.After(curr);
  }
}


int JevpPlot::nHistos()
{
  return nhistos;
}

char *JevpPlot::GetPlotName()
{
  PlotHisto *curr = (PlotHisto *)histos.First();

  if(!curr) {
    LOG(ERR, "No PlotHisto\n");
    return NULL;
  }
  
  if(!curr->histo) {
    LOG(ERR, "No curr histo\n");
    return NULL;
  }

  LOG(DBG, "get name");
  char *name = (char *)curr->histo->GetName();
  
  LOG(DBG,"Name = %s",name);
  if(name == NULL) {
    LOG(ERR, "name is null\n");
  }

  LOG(DBG, "parent = 0x%x",parent);

  if(parent) {
    char tmp[100];
    strcpy(tmp, parent);
    strcat(tmp, "_");
    int l=strlen(tmp);
    if(memcmp(name, tmp, l) != 0) {
      sprintf(myname, "%s%s",tmp,name);
      LOG(DBG, "myname %s",myname);
    }
    else {
      strcpy(myname, name);
      LOG(DBG, "myname %s",myname);
    }
  }
  else {
    strcpy(myname, name);
  }

  
  LOG(DBG, "myname %s",myname);
  return myname;
}

int JevpPlot::isDataPresent()
{
  PlotHisto *curr;
  TListIter next(&histos);

  int data_present = 0;
  int nhist=0;
  while((curr = (PlotHisto *)next())) {
    nhist++;
    if(curr->histo) {
      if(curr->histo->GetEntries() > 0) {
	data_present = 1;
      }
    }
  }
  
  if(nhist==0) data_present = 1;

  LOG(DBG, "data_present=%d for histo: %s",data_present,myname);
  return data_present;
}

// Return 0 if no data
// Return > 0 if data
void JevpPlot::draw()
{
    RtsTimer_root clock;
    RtsTimer_root clock2;
    RtsTimer_root clock3;
    clock2.record_time();
    clock.record_time();
    clock3.record_time();
    LOG(NOTE, "Draw plot %s",GetName());

    TListIter next(&histos);
    PlotHisto *curr;

    // Get the plot dimension!
    int dimension = 0;
    curr = (PlotHisto *)histos.First();
    if(curr && curr->histo) dimension = curr->histo->GetDimension();

    //printf("dimension = %d\n", dimension);

    // Find out if data is present!
    int data_present = isDataPresent();

    //printf("data_present = %d\n", data_present);

    LOG(NOTE, "Dimensions: %lf", clock3.record_time());
    // Set the legend!
    if(legend != NULL) {
	delete legend;
	legend = NULL;
    }

    if(legendx1 >= 0) {
	//	printf("Lengend1\n");
	legend = new TLegend(legendx1,legendy1,legendx2,legendy2);
    }

    LOG(NOTE, "legends: %lf", clock3.record_time());
   
    // Incredible hack!   This is needed to prevent a "gPad->Update()" within
    // the gStyle->SetOptStat() call, effectively doubling the time
    // to plot!
    //
    // The update is done at the end of all the drawing, as obviously it should be...
    TVirtualPad *save = gPad;
    gPad = NULL;
    gStyle->SetOptStat(optstat);
    gPad = save;

    LOG(NOTE, "l1: %lf %d", clock3.record_time(), optstat);
    gPad->SetLogx(logx);
    LOG(NOTE, "l1: %lf", clock3.record_time());
    gPad->SetLogy(logy);
    LOG(NOTE, "l1: %lf", clock3.record_time());
    if(optlogz != -1) {
	gStyle->SetOptLogz(optlogz);
    }

    LOG(NOTE, "l1: %lf", clock3.record_time());
    gPad->SetGridx(gridx);
    gPad->SetGridy(gridy);
    LOG(NOTE, "l2: %lf", clock3.record_time());
    gStyle->SetPalette(palette);  

    LOG(NOTE, "logs: %lf", clock3.record_time());
    
    // Worry about histogram bounds only for 1-dimensional histograms
    // 2-d and above come from the histo itself...
    if(dimension == 1) {
	double max = 0;

	// Find the maximum!
	next.Reset();
	while((curr = (PlotHisto *)next())) {
	    double m;

	    //printf("Ploting aaa\n");

	    m = curr->histo->GetBinContent(curr->histo->GetMaximumBin());
      
	    LOG(NOTE, "Histo %s: (%s) m=%f",GetPlotName(), curr->histo->GetName(), m);
	    if(m > max) max = m;
	}

	// Set all plots within to the maximum!
	next.Reset();
	while((curr = (PlotHisto *)next())) {

	    //  printf("plotting b\n\n\n");
	    if(external_maxy > -9999) {
		LOG(NOTE, "set max to %f", (float)(external_maxy));
		curr->histo->SetMaximum(external_maxy);
	    }
	    else {
		LOG(NOTE, "set max to %f",(float)(max*1.1));
		curr->histo->SetMaximum(max * 1.1);
	    }
	}
    }
  
    LOG(NOTE, "dim: %lf", clock3.record_time());
    int plotnum= 1;
    char *same = NULL;
  
    // printf("While loop start\n");

    double t = clock.record_time();
    LOG(NOTE, "prep time: %lf", t);

    next.Reset();
    while((curr = (PlotHisto *)next())) {
	//printf("plot %d\n", plotnum);
	plotnum++;
   
	if(dimension == 1) {
	    if(logy) {
		if(curr->histo->GetMaximum() == 0.0) curr->histo->SetMaximum(10.0);
		if(curr->histo->GetMinimum() == 0.0) curr->histo->SetMinimum(1.0);
	    }
	    else {
		if(curr->histo->GetMaximum() == 0.0) curr->histo->SetMaximum(1.0);
	    }
	}
 
	if(legend) {
	    char *text = (char *)((curr->legendText) ? curr->legendText : "no text");
	    char *args = (char *)((curr->legendArgs) ? curr->legendArgs : "");
      
	    legend->AddEntry(curr->histo, text, args); 
	}
    
	char opts[256];
	if(drawopts)
	    strcpy(opts, drawopts);
	else opts[0] = '\0';

	if((dimension > 1) && (strlen(opts) == 0)) {
	    sprintf(opts, "colz");
	}

	if(same) strcat(opts,same);

	//printf("p[s %s\n", opts);

	LOG(NOTE, "opts---%s\n",opts);
	clock.record_time();
	curr->histo->SetBit(kNoContextMenu | kCannotPick);
	curr->histo->Draw(opts);
	t = clock.record_time();
	LOG(NOTE, "time drawing histo: %s %f", curr->histo->GetName(), t);
	same = (char *)"SAME";
    }

    //#ifdef JUNK
    // Draw additional elements...

    clock.record_time();
    TObject *element = (TObject *)elements.First();
    while(element) {
	//printf("Drawing element\n");
	LOG(DBG, "Drawing an element...");
	element->SetBit(kNoContextMenu | kCannotPick);
	element->Draw();
	element = (TObject *)elements.After(element);
    }
    t = clock.record_time();
    LOG(NOTE, "Time drawing elements: %lf", t);
    //#endif
  
    //#ifdef JUNK 
    if(legend) {
	//printf("legend->Draw()\n");
	legend->SetBit(kNoContextMenu | kCannotPick);
	legend->Draw();
	t = clock.record_time();
	LOG(NOTE, "Time drawing legend: %lf", t);
    }
    
    t = clock2.record_time();
    LOG(NOTE, "Total time drawing: %lf", t);
    //#endif
}

ULong_t JevpPlot::Hash() const {
    TString s(myname);
    return s.Hash();
}
