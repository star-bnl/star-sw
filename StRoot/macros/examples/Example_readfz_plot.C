// $Id: Example_readfz_plot.C,v 1.4 2000/04/18 20:37:25 kathy Exp $
// $Log: Example_readfz_plot.C,v $
// Revision 1.4  2000/04/18 20:37:25  kathy
// St_DataSet,St_DataSetIter,St_Table classes are nowchanged to TDataSet,TDataSetIter,TTable
//
// Revision 1.3  1999/05/21 15:45:02  kathy
// fixed owner on Example_readfz_plot.C
//
// Revision 1.2  1999/05/21 15:33:49  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Pavel Nevski 
// what it does: 
//=======================================================================

// readfz_plot.C
//
// reads in geant fz file, gives a browser and then you can make a plot

{
gROOT->LoadMacro("geant.C");
Load();
geant(1,"/disk1/star/test/psc0049_08_40evts.fzd");
//this will run 1 event and give you a browser
// if you want to plot something from geant/event/g2t_track then

TDataSet *yp = chain->DataSet("geant");
//pointer to geant set

TDataSetIter geantIter(yp);
// iterator

St_g2t_track *pt = (St_g2t_track *) geantIter.Find("g2t_track");
// pointer to g2t_track

g2t_track_st *t = pt->GetTable();
// pointer to table

TH1F *h1 = new TH1F("g2t_rap","rapidity",100,-7.0,7.0);
//define histogram

for (Int_t i = 0; i < pt->GetNRows(); i++,t++){ h1->Fill(t->rapidity);}
//fill histogram

TCanvas *c1 = new TCanvas("c1","c1");
// make canvas

h1->Draw();
// on screen
}

