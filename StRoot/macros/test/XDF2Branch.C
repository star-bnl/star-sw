{
  ////////////////////////////////////////////////////////////////////////////////////
  //                                                                                //
  // This ROOT macro shows how to make a copy (read/write) of the existen XDF file  //
  //                                                                                //
  //  It                                                                            //
  //  - reads events in XDF format                                                  //
  //  - writes the events into another XDF file                                     //
  //  - writes the same event at the same time into begin_html the  <a href="http://root.cern.ch/root/html/TFile.html#TFile:description">ROOT file</a> end_html (ROOT format) //
  //                                                                                //
  ////////////////////////////////////////////////////////////////////////////////////
  
   // Load the STAF2ROOT base classes
   extra share libraries:
   gSystem->Load("St_base.so");
   // Load the STAF2ROOT base classes
   gSystem->Load("xdf2root.so");
   // STAF base class ROOT dictionary
  
  
   // The "wrapper" classes for the STAF tables ROOT dictionary
   gSystem->Load("St_Tables.so");
  
  //  Copy the XDF file to test XDF I/O and ROOT I/O functions
  
   gBenchmark->Start("XDFcopy");  // start timer
    
 // Open the original XDF file to read the events
 
 St_XDFFile  xdf_in("/star/sol/users/fisyak/auau_ce_b0-2_4801_5000.xdf");
 
 // Open a new XDF file to write the events
 
// St_XDFFile  xdf_out("/scr20/fine/copy.xdf","w");
 
 // Create a new ROOT file to write the events
 
 St_DataSetTree root_out("/scr20/fine/copy.root");
 
 St_DataSet *set = 0;
 Int_t count = 0;
 while ( set = xdf_in.NextEventGet() && count < 2) { // read next event from XDF file
   if (!root_out.Tree()) root_out.MakeTree(set);
   root_out.FillTree(set); 
   delete set;                          // delete the present event to free space
   set = 0;
   count++;
 }
 root_out.Dump();

 // close all files
 
 xdf_in.CloseXDF();
 gBenchmark->Stop("XDFcopy");
 gBenchmark->Show("XDFcopy");

 root_out.Close();
 
 //Stop time and print benchmarks result

 // Some result:

printf("    Real Time Cpu Time  Os    CPU      RAM     input   output       size         name\n");
printf("      sec       sec                    Mb      file     file \n");
printf("\n ======== Your system: \n");
gBenchmark->Show("XDFcopy");

}
