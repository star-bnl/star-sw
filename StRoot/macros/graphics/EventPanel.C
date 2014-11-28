{
//
// This macro generates a Controlbar menu: To see the output, click begin_html <a href="gif/demos.gif" >here</a> end_html
// To execute an item, click with the left mouse button.
// To see the HELP of a button, click on the right mouse button.
   gROOT->Reset();
   gROOT->SetStyle("Default");
   TControlBar *bar = new TControlBar("vertical", "Control Panel"); 
   bar->AddButton("Load STAR libraries",".x Load.C", "Load the share libraries with STAR base classes defintions");
   bar->AddButton("Print view port size",   ".x Get3DSize.C", "Print the total size of all 3D axice");
   bar->AddButton("Read StEvent",".x EventRead.C(1,\"gtrack.event.root\")","Read StEvent objects from root file");
   bar->AddButton("Make StEvent with tfs",".x bfc.C(1,\"gstar tfs\")","Make StEvent objects with tfs");
   bar->AddButton("Make StEvent with trs",".x bfc.C(1,\"gstar trs\")","Make StEvent objects with trs");
   bar->AddButton("Make StEvent with tss",".x bfc.C(1,\"gstar tss\")","Make StEvent objects with tss");
   bar->AddButton("Draw ALL TPC hits",".x DrawTpcHits.C","Draw the global tracks by its helix object");
   bar->AddButton("Draw tracks",".x DrawTrackTpcHits.C","Draw the global tracks by its helix object");
   bar->AddButton("Draw g2t_tpc_hits",".x HitsDraw.C","Read STAF tables from XDF file and draw \"g2t_tpc_hits\" hits");
   bar->AddButton("Add Axice","St_PolyLine3D::Axis();","Add 3D axice to the cuurent TPad view");
   bar->AddButton("Shape demo",".x shapes.C","Run the \"ROOT\" GEANT shape demo");
   bar->AddButton("Help on Demos",".x EventDemosHelp.C", "Click Here For Help on Running the Demos");
   bar->AddButton("browser",     "{b = new TBrowser(\"HALL\");}", "Start the ROOT Browser");
   bar->Show();
   gROOT->LoadMacro("PadControlPanel.C");
   gROOT->SaveContext();
}
 
