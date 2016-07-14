#include "PdfFileBuilder.h"

#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>

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
  this->serverTags = serverTags;

  displays->setIgnoreServerTags(ignoreServerTags);
  displays->setServerTags(serverTags);
  displays->setDisplay(displays->getDisplayNodeFromIndex(displayNumber));
  displays->updateDisplayRoot();

  LOG("JEFF", "write");
  writePdf(filename, 1);
}

void PdfFileBuilder::writePdf(char *filename, int combo_index)
{
  LOG("JEFF", "Writing pdf: %s index=%d",filename,combo_index);

  //displays->displayRoot->dump(0, "dr:");

  LOG("JEFF", "Now write it...");

  DisplayNode *root = displays->getTab(combo_index);

  if(combo_index == 0) {
    LOG("JEFF", "disproot = 0x%x root = 0x%x", displays->displayRoot, root);
    root = displays->displayRoot;
  }

  LOG("JEFF", "writeNodePdf root: %s",filename);

  PdfIndex index;
  writeNodePdf(root, &index, NULL, filename, 1);
  
  LOG(DBG, "write endfilename");

  // Now a summary....
  char endfilename[256];
  strcpy(endfilename, filename);
  strcat(endfilename, ")");
  TCanvas summary("c2");
  summary.Print(endfilename, "pdf,Portrait");

  
  // Index the file...
  char indexedfilename[256];
  strcpy(indexedfilename, filename);
  // strcat(indexedfilename, ".idx");
  index.CreateIndexedFile(filename, indexedfilename);
}

int PdfFileBuilder::writeNodePdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page)
{
    LOG("JEFF", "Checking node %s against server tags %s", node->name, serverTags);

    int npages = 0;

    if(node->leaf) {   // We are writing histograms...
	writeHistogramLeavesPdf(node, index, prevIndexEntry, filename, page);
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

	return npages;
    }
}    


// If page = 1 prints out start tag --> "filename("
// But assumes a summary follows, so there is no end tag --> "filename)"
//
int PdfFileBuilder::writeHistogramLeavesPdf(DisplayNode *node, PdfIndex *index, index_entry *prevIndexEntry, char *filename, int page)
{
  LOG("JEFF", "Write histogram leaves: %s",node->name);
  
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
  gStyle->SetCanvasColor(19);
  TCanvas *c1 = new TCanvas("c1","c1",1000,800);

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
  c1->Divide(wide, deep);
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
    
    
    printf("Got scaley...  Setting max value to ymax=%lf\n",ymax*1.1);
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
    

    LOG(DBG, "Plotting %s on page %d / pad %d",cnode->name, page, pad);

    JevpPlot *plot = NULL;
    plot = getPlotByName(cnode->name);
    
    if(plot) {
      LOG(DBG, "Found plot %s",cnode->name);
      plot->draw();
    }
    else {
      LOG(DBG, "Can't find plot %s",cnode->name);
      DrawCrossOfDeath(cnode->name);
    }

    cnode = cnode->next;
    pad++;
  }
  
  while(pad <= wide*deep) {
    c1->cd(pad);
    TLatex *x = new TLatex(.5,.5," ");
    x->Draw();
    //gPad->Draw();
    // printf("Drawing jeff %d\n",pad);
    pad++;
  }
  
  
  c1->Print(fname, "pdf,Portrait");

  delete c1;
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
