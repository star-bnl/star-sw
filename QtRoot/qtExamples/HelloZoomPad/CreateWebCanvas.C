void CreateWebCanvas(const char *canvas ="c1")
{
   //
   // To see the output of this macro, 
   // Execute:
   //  1. execute $ROOTSYS/tutorials/hsimple.C at once to create hsimple.root file
   //
   //     > root $ROOTSYS/tutorials/hsimple.C
   //
   //  2. Execute $ROOTSYS/tutorials/ntuple1.C to create 4 TPad's "c1" TCanvas
   //     root[2] .x $ROOTSYS/tutorials/ntuple1.C to create 4 Tpad's "c1" TCanvas
   //
   //  3. Execute this macro to create the "TCanvas Web page" 
   //
   //------------------------
   //
   // Create the Pad zoomer widget
   TCanvas *c = (TCanvas *) gROOT->GetListOfCanvases()->FindObject(canvas);
   if (c) {
       TQtCanvas2Html  TQtCanvas2Html(c,  900, 600);
   } else  {
        fprintf(stderr," The TCanvas object <%s> has not been found\n", canvas);
     }
 }
