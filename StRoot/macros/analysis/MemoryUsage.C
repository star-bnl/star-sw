//$Id: MemoryUsage.C,v 1.3 2000/01/20 16:36:33 kathy Exp $
//$Log: MemoryUsage.C,v $
//Revision 1.3  2000/01/20 16:36:33  kathy
//updating to add nevents as input to MemoryUsage.C
//
//Revision 1.2  2000/01/19 14:33:48  kathy
//pick up input file from samples directory
//
//Revision 1.1  2000/01/19 03:23:32  kathy
//macro to make hist of memory usage vs event number - to be used by auto QA system
//
////////////////////////////////////////////////////////////
// owner: Kathy Turner & Curtis Lansdell
// description: reads in a file of values of memory usage
//            (one entry per line) and plots the entry number
//             vs memory usage value - sends histogram to 
//             postscript file
//
//     inputs to macro:
//        InFile - input text file of memory used 
//               - 1 entry per line
//        OutFile - output postscript file name
//        nevents - # entrys in InFile - default is 500
////////////////////////////////////////////////////////////

//#include "TH2.h"

void MemoryUsage(    
       const Char_t *InFile=
 "/afs/rhic/star/data/samples/mem_usage_dev_tfs_lin_fri_y2a_hc_lowdensity.txt",
       const Char_t *OutFile="mem_usage_dev_tfs_lin_fri_y2a_hc_lowdensity.ps",
       Int_t nevents=500)
{
 
   gROOT->Reset();

   cout << "MemoryUsage.C, input file name = " << InFile << endl;
   cout << "MemoryUsage.C, # entries in input file = " << nevents << endl;
   cout << "MemoryUsage.C, output postscript file = " << OutFile << endl;


   FILE *fp = fopen(InFile,"r");

   TPostScript *psf = 0;
   psf = new TPostScript(OutFile);  


   TH2F *h1 = new TH2F("MemUsage","memory usage vs event number",
      nevents,0.,Float_t(nevents),100,200000.,1600000.);
   h1->SetXTitle("event number");
   h1->SetYTitle("memory usage in bytes");
   h1->GetXaxis()->SetLabelSize(0.02);
   h1->GetYaxis()->SetLabelSize(0.02);
   h1->GetXaxis()->SetTitleOffset(1.3);
   h1->GetYaxis()->SetTitleOffset(1.3);
   h1->GetXaxis()->SetTitleSize(0.03);
   h1->GetYaxis()->SetTitleSize(0.03);

   Int_t x=0;
   Int_t ncols = 0;
   Int_t nlines = 0;

   while (1) {
      ncols = fscanf(fp,"%d",&x);
      if (ncols < 0) break;    
      if (nlines < 5) printf("x=%8d \n",x);
      h1->Fill(Float_t(nlines),Float_t(x));
      nlines++;
   }

   printf(" found %d points\n",nlines);

   fclose(fp);
// -----------------------------------

   TCanvas *HistCanvas = 
     new TCanvas("CanvasName","STAR Histogram Canvas",600,780);

// write title at top of canvas - first page
   TPaveLabel *Ltitle = new TPaveLabel(0.0,0.96,1.0,1.0,(char *)InFile,"br");
   Ltitle->SetTextFont(32);
   Ltitle->SetTextSize(0.5);
   Ltitle->Draw();
   
// now put in date & time at bottom right of canvas - first page
   TDatime HistTime;
   const Char_t *myTime = HistTime.AsString();
   TPaveLabel *Ldatetime = new TPaveLabel(0.7,0.01,0.95,0.03,myTime,"br");
   Ldatetime->SetTextSize(0.6);
   Ldatetime->Draw();

// now put in page # at bottom left of canvas - first page
   Int_t Ipagenum = 1;
   Char_t Ctmp[10];
   ostrstream Cpagenum(Ctmp,10);
   Cpagenum << Ipagenum << ends;
   TPaveLabel *Lpage = new TPaveLabel(0.1,0.01,0.2,0.03,Ctmp,"br");
   Lpage->SetTextSize(0.6);
   Lpage->Draw();

// create a pad on the canvas
   TPad *graphPad = new TPad("PadName","Pad Title",0.0,0.05,1.0,0.95);
   graphPad->Draw();
   graphPad->cd();

   h1->Draw();

   if (psf) {
     psf->Close();
     delete psf;
   }

}













