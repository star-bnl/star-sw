void XDFtest(const Char_t *infile=0, const Char_t *outfile=0, const Char_t mode ='r', const Int_t maxcount=0, const Int_t compress=0)
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
  //  outfile = 0  - "read only" mode                                               //
  //                                                                                //
  //  mode = 'r'  - ROOT I/O only                                                   //
  //         'x'  - xDF output only                                                 //
  //         'b'  - bOTH XDF and ROOT I/O simultaneously  (not implemented yet)     //
  //         default = ROOT I/O output, no XDF output                               //
  //  maxcount    = 0 - read the input file till "end_of_file"                      //
  //              > 0 - the number of events to read                                //
  //  compress        - compression factor for ROOT I/O  (=0 - no compression)      //
  //                                                                                //
  //                                                                                //
  ////////////////////////////////////////////////////////////////////////////////////
  
   if (!infile) {
     printf(" Usage:\n");
     printf("\t XDFtest(const Char_t *infile=0, const Char_t *outfile=0, const Char_t mode =\'r\', const Int_t maxcount=0)\n");
     printf("\t\t mode = \'r\'  - ROOT I/O only\n");
     printf("\t\t        \'x\'  - xDF output only\n");
     printf("\t\t        \'b\'  - bOTH XDF and ROOT I/O simultaneously  (not implemented yet)\n");
     printf("\t\t default = ROOT I/O output, no XDF output\n");
     printf("\t\t maxcount= 0 - read the input file till \"end_of_file\"\n");
     printf("\t\t         > 0 - the number of events to read\n");
     printf("\t\t compress= 0 - compression factor for ROOT I/O  (=0 - no compression)\n");
     return;
   }
   // STAF base class ROOT dictionary
   
   gSystem->Load("St_base.so");
    // Load the extra share libraries:
   gSystem->Load("xdf2root.so");
   
   // The "wrapper" classes for the STAF tables ROOT dictionary
   gSystem->Load("St_Tables.so");
  
  //  Copy the XDF file to test XDF I/O and ROOT I/O functions
  
   gBenchmark->Start("XDF2ROOT");  // start timer
   
   Long_t in_id, in_size, in_flag, in_time;
   
   Long_t out_id, out_size, out_flag, out_time;
    
 // Open the original XDF file to read the events
 
  St_XDFFile  *xdf_in = 0;
  xdf_in     =  new St_XDFFile (infile);
 
 // Open a new XDF file to write the events
 
  St_XDFFile  *xdf_out = 0;
  if (outfile && (mode == 'x') )
        xdf_out  = new St_XDFFile(outfile,"w");
 
 // Create a new ROOT file to write the events
 
 TFile       *root_out = 0;
 if (outfile && (mode != 'x') ) {
    root_out = new TFile (outfile,"RECREATE"); 
    root_out->SetCompressionLevel(compress);
 }
 
 St_DataSet *set = 0;

 Int_t count = 0;
 gBenchmark->Start("XDFREAD");
 while ( set = xdf_in->NextEventGet() && (maxcount== 0 || count < maxcount)) { // read next event from XDF file
   gBenchmark->Stop("XDFREAD");
   printf(" next event %d\n",count);
    if (xdf_out) {
      gBenchmark->Start("XDFWRITE");
      xdf_out->NextEventPut(set);            // write this event into another XDF file
      gBenchmark->Stop("XDFWRITE");
    }
    if (root_out) {
      gBenchmark->Start("ROOTWRITE");
//      set->Write((Text_t *)0,0,20000000);                          // write this event via sequential ROOT I/O
      set->Write();
      gBenchmark->Stop("ROOTWRITE");
    }
   count++;
   delete set;                                           // delete the present event to free space
   set = 0;
   gBenchmark->Start("XDFREAD");
 }
 gBenchmark->Stop("XDFREAD");

 // close all files
 
 if (xdf_in)   { xdf_in->CloseXDF();  gBenchmark->Show("XDFREAD");   }
 if (xdf_out)  { xdf_out->CloseXDF(); gBenchmark->Show("XDFWRITE");  }
 if (root_out) { root_out->Close();   gBenchmark->Show("ROOTWRITE"); }
 
 //Stop time and print benchmarks result
 gBenchmark->Stop("XDF2ROOT");
 
 // Some result:

 // pick some file information:
    gSystem->GetPathInfo(infile,&in_id,&in_size,&in_flag,&in_time);
    gSystem->GetPathInfo(outfile,&out_id,&out_size,&out_flag,&out_time);
    
// printf("    Real Time Cpu Time  Os    CPU      RAM     input   output       size         name\n");
//  printf("      sec       sec                    Mb      file     file \n");
  printf("*** %d events XDF",count);
  if (xdf_out) printf("->XDF");
  else if (root_out) printf("->ROOT");
  printf(": \n");
  gBenchmark->Show("XDF2ROOT");
  printf(" input file: %d %s;", in_size,infile);
  if (outfile) 
    printf(" output file: %d %s; diff of size (in - out) = %d",out_size,outfile,(in_size-out_size));
  printf("\n");
 }
