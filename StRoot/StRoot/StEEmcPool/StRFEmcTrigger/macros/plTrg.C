/*
Plot Trgger simu results
*/

void plTrg( int page=1, int keve=1)
{
  TString ifile="st_physics_1164056_raw_0001.hist.root";
  ifile="aa2.hist.root"; // ifile="save."+ifile;
   gROOT->Reset();
   gSystem->Load("St_base");
   gROOT->LoadMacro("/afs/rhic.bnl.gov/star/spin/balewski/root_macros/JBRoot.C");

// Connect the input file and get the  histogram
   TList *top = Johf(ifile.Data());

   TString dir1="RFTrigB";

   TIter Hi1(Jcd((dir1+"Hist").Data(),top));     

   TPad *pd1=Jcan(500,400,"file="+ifile+",   dir="+dir1, .99); 
   
   switch(page) {
   case 1: {
     pd1->Divide(2,3);
     const char *name[]={"bHT","eHT", "bJP", "eJP", "bTot","eTot"};
    
     for(int k=0;k<6;k++) {
       Jpl(name[k],k+1, Hi1,pd1);
       gPad->SetLogy();
     } 
   } break;
   default:
   }
} 
