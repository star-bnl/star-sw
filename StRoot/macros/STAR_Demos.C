{
//
// This macro generates a Controlbar menu: To see the output, click begin_html <a href="gif/demos.gif" >here</a> end_html
// To execute an item, click with the left mouse button.
// To see the HELP of a button, click on the right mouse button.
 
   gROOT->Reset();
   bar = new TControlBar("vertical", "STAR Demos",600,400);
 
//   bar->AddButton("Create STAF \"particle\" table",".x CallMevSaveXDF.cxx", "Click it to create a new XDF file and fill it with \"mevsim\" STAF ,module");
   bar->AddButton("Plot histogram",   ".x  MakeHists.cxx", "Read XDF file with \"particle\" table and plot some histograms");
   bar->AddButton("Make analysis",       ".x  par_anal.cxx",     "Another example of the analysis of the \"particle\" table");
   bar->AddButton("Big Full Chain",       ".x  bfc.C",     "An example of production chain (1 event only)");
   bar->Show();
   gROOT->SaveContext();
   printf("Look for the new ROOT control bar window and pop it up !!!\n");
}
 
 
