#include "PdfFileBuilder.h"

#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TPaveText.h>

#define TITLEBGCOLOR 14
#define BGCOLOR 17
#define HISTOCOLOR 10
#define BLACKCOLOR 1

JevpPlot *PdfFileBuilder::getPlotByName(char *name) {
  if(server) {
    return server->getPlot(name);
  }
  if(plotset) {
    return plotset->getPlot(name);
  }
  return NULL;
}

void PdfFileBuilder::write(char *filename, int displayNumber, int ignoreServerTags, char *serverTags)
{    
    //printf("Start..\n");
    //LOG(DBG, "pdffilebuilder write[%s], %d %d (%s)", filename, displayNumber, ignoreServerTags, serverTags);
    displays->setIgnoreServerTags(ignoreServerTags);
    displays->setServerTags(serverTags);
    displays->setDisplay(displays->getDisplayNodeFromIndex(displayNumber));
    displays->updateDisplayRoot();

    LOG(DBG, "write");
    writePdf(filename, 1);
    //printf("Done...\n");
}

void PdfFileBuilder::writePdf(char *filename, int combo_index)
{
    LOG("JEFF", "Writing pdf: %s index=%d",filename,combo_index);

    //displays->displayRoot->dump(0, "dr:");

    LOG(DBG, "Now write it...");

    if(displays == NULL) {
	LOG(CRIT, "displays == NULL");
    }
    DisplayNode *root = displays->getTab(combo_index);

    if(combo_index == 0) {
	LOG(DBG, "disproot = 0x%x root = 0x%x", displays->displayRoot, root);
	root = displays->displayRoot;
    }

    LOG("JEFF", "create PDF File: %s",filename);

    PdfIndex index;
    writeNodePdf(root, &index, NULL, filename, 1);
  
    LOG("JEFF", "created PDF File: %s", filename);

    // Now a summary....
    char endfilename[256];
    strcpy(endfilename, filename);
    strcat(endfilename, ")");
    TCanvas summary("c2");
    summary.Print(endfilename, "pdf,Portrait");

  
    LOG(DBG, "Index it...");
    // Index the file...
    char indexedfilename[256];
    strcpy(indexedfilename, filename);
    // strcat(indexedfilename, ".idx");
    LOG("JEFF", "Create indexed file: %s", indexedfilename);
    index.CreateIndexedFile(filename, indexedfilename);
    LOG("JEFF", "Created indexed file: %s", indexedfilename);
}

int PdfFileBuilder::writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page)
{
    LOG(DBG, "Checking node %s against server tags %s", node->name, serverTags);
    if(node == NULL) {
	LOG("JEFF", "node == NULL! page=%d",page);
    }

    int npages = 0;

    if(node->leaf) {   // We are writing histograms...
	writeHistogramLeavesPdf(node, index, prevIndexEntry, filename, page);
	LOG(DBG, "Return pdf");
	return 1;
    }
    else {   // We are just writing index entries
	// are we the child?
	index_entry *currIndexEntry;
	if(!node->parent || (node->parent->child == node)) {
	    currIndexEntry = index->add_child(prevIndexEntry, node->name, page, 0);
	}
	else {
	    currIndexEntry = index->add_sibling(prevIndexEntry, node->name, page, 0);
	}
    
	if(node->child) {
	    npages += writeNodePdf(node->child, index, currIndexEntry, filename, page);
	}
    
	if(node->next) {
	    npages += writeNodePdf(node->next, index, currIndexEntry, filename, page + npages);
	}

	LOG(DBG, "Return pages");
	return npages;
    }
}    


// If page = 1 prints out start tag --> "filename("
// But assumes a summary follows, so there is no end tag --> "filename)"
//
int PdfFileBuilder::writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page)
{
    LOG("JEFF", "Write histogram leaves: %s, page %d",node->name, page);
  	
  char fullName[1000];
  node->getTabName(fullName);
  


  //if((node->prev != NULL) || (!node->leaf)) {
  //LOG(ERR, "Shouldn't happen: prev=0x%x leaf=%d", node->prev, node->leaf);
  //}
  
  // create index first
  index_entry *cindex = index->add_child(prevIndexEntry, node->name, page, 0);
  DisplayNode *cnode = node->next;
  while(cnode) {
    cindex = index->add_sibling(cindex, cnode->name, page, 0);
    cnode = cnode->next;
  }
  
  // Now draw histograms...
  gStyle->SetCanvasColor(BGCOLOR);

  TCanvas *outerCanvas = new TCanvas("outerCanvas", "outerCanvas", 1000, 800);
  outerCanvas->SetFillColor(BGCOLOR);
  outerCanvas->cd();
  TPaveText *title = new TPaveText(0,.96,1,1, "NDC");
  title->SetFillColor(TITLEBGCOLOR);
  title->SetBorderSize(0);
  title->SetTextAlign(12);
  title->AddText(fullName);
  title->Draw();

  TPad *c1 = new TPad("c1","c1", 0,0 ,1, .96, BLACKCOLOR);
  //c1->SetCanvasColor(1);
  c1->Draw();
  //TCanvas *c1 = new TCanvas("c1","c1",1000,800);

  char fname[256];
  strcpy(fname, filename);
  if(page == 1) {
    strcat(fname, "(");
  }
  
  int wide = node->getIntParentProperty("wide");
  if(wide < 0) wide = 1;
  int deep = node->getIntParentProperty("deep");
  if(deep < 0) deep = 1;
  int scaley = node->getIntParentProperty("scaley");
  if(scaley <= 0) scaley = 0;
  
  c1->Clear();
  c1->Divide(wide, deep, .002, .002);
  int pad = 1;
  
  if(scaley) {
    double ymax = -999999;
    cnode = node;
    while(cnode) {

      LOG(DBG, "cnode->name = %s", cnode->name);
      JevpPlot *plot = getPlotByName(cnode->name);
      if(plot) {
	LOG(DBG, "got plot 0x%x",plot);
	double my = plot->getMaxY();
	if(my > ymax) ymax = my;
      }
      cnode = cnode->next;
    }
    
    
    //printf("Got scaley...  Setting max value to ymax=%lf\n",ymax*1.1);
    cnode = node;
    while(cnode) {
      JevpPlot *plot = getPlotByName(cnode->name);
      if(plot) {
	if(plot->logy) {
	  plot->setMaxY(ymax * 2);
	}
	else {
	  plot->setMaxY(ymax * 1.1);
	}
      }
      cnode = cnode->next;
    }
  }
  

  cnode = node;
  while(cnode) {
    c1->cd(pad);
    
    gPad->SetFillColor(BGCOLOR);
    gPad->SetFrameFillColor(HISTOCOLOR);
    gPad->SetTopMargin(.10);
    //gStyle->SetOptTitle(0);

    LOG(DBG, "Plotting %s on page %d / pad %d",cnode->name, page, pad);

    JevpPlot *plot = NULL;
    plot = getPlotByName(cnode->name);
    
    if(plot) {
	//LOG("JEFF", "Plot");
	//LOG("JEFF", "cname: %s", cnode->name);
	
	plot->draw();
    }
    else {
	//LOG("JEFF", "No Plot");
	LOG(DBG, "Can't find plot %s",cnode->name);
	DrawCrossOfDeath(cnode->name);
    }
    
    cnode = cnode->next;
    pad++;
  }
  
  while(pad <= wide*deep) {
    c1->cd(pad);
    gPad->SetFillColor(BGCOLOR);
    gPad->Draw();
    //TLatex *x = new TLatex(.5,.5," ");
    //x->Draw();
    //gPad->Draw();
    // printf("Drawing jeff %d\n",pad);
    pad++;
  }
  
  
  outerCanvas->Print(fname, "pdf,Portrait");

  delete c1;
  delete title;
  delete outerCanvas;

  return 1;
}


void PdfFileBuilder::DrawCrossOfDeath(char *str)
{
  TLine* a = new TLine(0.,0.,1.,1.);
  TLine* b = new TLine(0.,1.,1.,0.);
  TText* t = new TText(0.5,0.5,str);

//   // This is how we free the memory...
  a->SetBit(kCanDelete);
  b->SetBit(kCanDelete);
  t->SetBit(kCanDelete);
//   screen->addPlot(a);
//   screen->addPlot(b);
//   screen->addPlot(t);

  a->SetLineColor(2);
  b->SetLineColor(2);
  t->SetTextColor(3);
  t->SetTextAlign(22);

  // Already cd()'d to proper pad...
  a->Draw();
  b->Draw();
  t->Draw();

  //delete a;
  //delete b;
  //delete t;
  // gCanvas->Update();
  //cout << __PRETTY_FUNCTION__ << endl;
  return;

}
