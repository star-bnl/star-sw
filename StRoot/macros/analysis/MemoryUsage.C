//$Id: MemoryUsage.C,v 1.6 2000/02/01 17:31:00 kathy Exp $
//$Log: MemoryUsage.C,v $
//Revision 1.6  2000/02/01 17:31:00  kathy
//fixed documentation
//
//Revision 1.5  2000/01/31 16:14:17  kathy
//update memory usage macro to read the input file twice and calculate the binning on the histogram by itself
//
//Revision 1.4  2000/01/21 22:01:32  kathy
//updated to print box plot and to input lower and upper range of vertical axis
//
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
//
////////////////////////////////////////////////////////////

void MemoryUsage(    
       const Char_t *InFile=
       "/afs/rhic/star/data/samples/mem_usage_dev_tfs_lin_y1b_hc_lowdensity_21jan00.txt",
       const Char_t *OutFile=
         "mem_usage_dev_tfs_lin_y1b_hc_lowdensity_21jan00.ps")
{
 
   gROOT->Reset();

   cout << "MemoryUsage.C, input file name = " << InFile << endl;
   cout << "MemoryUsage.C, output postscript file = " << OutFile << endl;

// --- open output ps file
   TPostScript *psf = 0;
   psf = new TPostScript(OutFile);  

   Int_t memmax=0;
   Int_t memmin=999999;
   Int_t x=0;
   Int_t ncols = 0;
   Int_t nlines = 0;

// --- read input file first time to get # entries, min & max values
   FILE *fp = fopen(InFile,"r");
   while (1) {
      ncols = fscanf(fp,"%d",&x);
      if (ncols < 0) break;    
      if (nlines < 5) printf("x=%8d \n",x);
      if (x > memmax) memmax = x;
      if (x < memmin) memmin = x;
      nlines++;
   }
   cout << " 1: num entries = " << nlines << endl;
   cout << " 1: min entry   = " << memmin << endl;
   cout << " 1: max entry   = " << memmax << endl;
   fclose(fp);


// determine # bins, low & high values of x axis:
   Int_t nevents=0;
   if (nlines <= 100) 
       nevents=100;
   elseif (nlines > 100 && nlines <= 1000){
      nevents = nlines/100;
      nevents = nevents + 1;
      nevents = nevents*100;
   }
   elseif (nlines > 1000)
       nevents=1000;

   Int_t lowy  = 0;
   if (memmin > 10000 ){
      lowy = memmin/10000;
      lowy = lowy*10000;
   }
   else 
      lowy = 10000;

   Int_t highy = 0;
   if (memmax > 10000 ){
      highy = memmax/10000;
      highy = highy + 1;
      highy = highy*10000;
   }
   else 
      highy = 10000;

   cout << "  use # bins =     " << nevents << endl;
   cout << "  use low scale  = " << lowy    << endl;
   cout << "  use high scale = " << highy   << endl;

// now book histogram
   TH2F *h1 = new TH2F("MemUsage","memory usage vs event number",
      nevents,0.,Float_t(nevents),100,Float_t(lowy),Float_t(highy));
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
// --- read input file 2nd time to fill histogram
   FILE *fp2 = fopen(InFile,"r");

   while (1) {
      ncols = fscanf(fp2,"%d",&x);
      if (ncols < 0) break;    
//      if (nlines < 5) printf("x=%8d \n",x);
      h1->Fill(Float_t(nlines),Float_t(x));
      nlines++;
   }
//   printf(" found %d points\n",nlines);
   fclose(fp2);

// ------------------------------------------------------------
// Now draw hist to canvas and save on ps file

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

   h1->Draw("box");

   if (psf) {
     psf->Close();
     delete psf;
   }

}













