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
  
   // Load the extra share libraries:
  
   gSystem->Load("libasu.so");
   gSystem->Load("libdsl.so");
  
   // STAF base class ROOT dictionary
  
   gSystem->Load("St_base.so")
  
   // The "wrapper" classes for the STAF tables ROOT dictionary
   gSystem->Load("St_Tables.so");
  
  //  Copy the XDF file to test XDF I/O and ROOT I/O functions
  
   gBenchmark->Start("XDFcopy");  // start timer
   
// St_XDFFile  xdf_in("/star/mds/data/SD98/auau200/fast2/central/hijing/set0001/regular/auau_ce_b0-2_0001_0010.xdf");
// St_XDFFile  xdf_in("/star/mds/data/SD98/auau200/evg/central/hijing/set0001/regular/auau_ce_b0-2_4801_5000.xdf");
 
 // Open the original XDF file to read the events
 
 St_XDFFile  xdf_in("/star/sol/users/fisyak/auau_ce_b0-2_4801_5000.xdf");
 
 // Open a new XDF file to write the events
 
 St_XDFFile  xdf_out("/scr21/fisyak/copy.xdf","w");
 
 // Create a new ROOT file to write the events
 
 TFile       root_out("/scr21/fisyak/copy.root","RECREATE");
 St_DataSet *set = 0;

 while ( set = xdf_in.NextEventGet()) { // read next event from XDF file
   xdf_out.NextEventPut(set);           // write this event into another XDF file
   set->Write();                        // write this event via sequential ROOT I/O
   delete set;                          // delete the present event to free space
   set = 0;
 }

 // close all files
 
 xdf_in.CloseXDF();
 xdf_out.CloseXDF();
 root_out.Close();
 
 //Stop time and print benchmarks result
 gBenchmark->Stop("XDFcopy");
 // Some result:

printf("    Real Time Cpu Time  Os    CPU      RAM     input   output       size         name\n");
printf("      sec       sec                    Mb      file     file \n");
printf(" 1. 175.44     51.00   NT  Intel 266   128   NT Server  local   109'989'060 auau_ce_b0-2_4801_5000.xdf\n");
printf(" 2. 146.14     50.45                            local   local\n");
printf(" 3. 165.70      6.57   SunOS rcf 5.6 SUNW,Ultra-4 MDS   scr21   109'726'200 auau_ce_b0-2_1_200.xdf\n");
printf(" 3. 179.13      6.07   SunOS sol 5.6 SUNW,Ultra-4 MDS   scr21   109'989'060 auau_ce_b0-2_4801_5000.xdf\n");
printf(" Remark: The numbers above were calculated with no ROOT I/O involved \n");
printf("\n ======== Your system: \n");
gBenchmark->Show("XDFcopy");

 
// {
//   // Load the extra share libraries:
//    gSystem->Load("libasu.so");
//     gSystem->Load("libdsl.so");
//   // STAF base class ROOT dictionary  
//     gSystem->Load("St_base.so")  
//   // The "wrapper" classes for the STAF tables ROOT dictionary
//    gSystem->Load("St_Tables.so");  
//
//   //"Open" ROOT file with the events 
//
//   TFile  star_event_file("/scr21/fisyak/copy.root","RECREATE");
//
//   // Start ROOT Browser to navigate the structures
//
//   TBrowser b;
// }
}
