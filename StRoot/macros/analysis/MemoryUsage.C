//$Id: MemoryUsage.C,v 1.2 2000/01/19 14:33:48 kathy Exp $
//$Log: MemoryUsage.C,v $
//Revision 1.2  2000/01/19 14:33:48  kathy
//pick up input file from samples directory
//
//Revision 1.1  2000/01/19 03:23:32  kathy
//macro to make hist of memory usage vs event number - to be used by auto QA system
//
////////////////////////////////////////////////////////////
// owner: Kathy Turner
// description: reads in a file of values of memory usage
//            (one entry per line) and plots the entry number
//             vs memory usage value - sends histogram to 
//             postscript file
//
////////////////////////////////////////////////////////////

void MemoryUsage(    
       const Char_t *InFile=
 "/afs/rhic/star/data/samples/mem_usage_dev_tfs_lin_fri_y2a_hc_lowdensity.txt",
       const Char_t *OutFile="mem_usage_dev_tfs_lin_fri_y2a_hc_lowdensity.ps")
{
//   example of macro to read data from an ascii file and
//   create a root file with an histogram and an ntuple.
 
   gROOT->Reset();

   cout << "MemoryUsage.C, input file name = " << InFile << endl;
   cout << "MemoryUsage.C, output postscript file = " << OutFile << endl;

   FILE *fp = fopen(InFile,"r");

   TPostScript *psf = 0;
   psf = new TPostScript(OutFile);  
  
   TH2F *h1 = new TH2F("MemUsage","memory usage vs event number",
      500,0.,500.,100,200000.,1600000.);
    h1->SetXTitle("event number");
    h1->SetYTitle(" memory usage in bytes");

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
    
   TCanvas *HistCanvas = 
     new TCanvas("CanvasName"," STAR Histogram Canvas",600,780);

// write title at top of canvas - first page
   TPaveLabel *Ltitle = 
    new TPaveLabel(0.1,0.96,0.9,1.0,
     " dev tfs linux fri y2a hc lowdensity ","br");
   Ltitle->SetTextFont(32);
   Ltitle->SetTextSize(0.5);
   Ltitle->Draw();
   
   h1->Draw();

   if (psf) {
    psf->Close();
    delete psf;
   }

}













