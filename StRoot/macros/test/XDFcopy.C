{
  //  Copy the XDF file to test XDF I/O function
 gBenchmark->Start("XDFcopy");
// St_XDFFile  xdf_in("/star/mds/data/SD98/auau200/fast2/central/hijing/set0001/regular/auau_ce_b0-2_0001_0010.xdf");
 St_XDFFile  xdf_in("/star/mds/data/SD98/auau200/evg/central/hijing/set0001/regular/auau_ce_b0-2_4801_5000.xdf");
 St_XDFFile  xdf_out("/scr21/fisyak/copy.xdf","w");
 
 St_DataSet *set = 0;

 while ( set = xdf_in.NextEventGet()) {
   xdf_out.NextEventPut(set);
   delete set;
   set = 0;
 }

 xdf_in.CloseXDF();
 xdf_out.CloseXDF();
 gBenchmark->Stop("XDFcopy");
 // Some result:

printf("    Real Time Cpu Time  Os    CPU      RAM     input   output       size         name\n");
printf("      sec       sec                    Mb      file     file \n");
printf(" 1. 175.44     51.00   NT  Intel 266   128   NT Server  local   109'989'060 auau_ce_b0-2_4801_5000.xdf\n");
printf(" 2. 146.14     50.45                            local   local\n");
printf(" 3. 165.70      6.57   SunOS rcf 5.6 SUNW,Ultra-4 MDS   scr21   109'726'200 auau_ce_b0-2_1_200.xdf\n");
printf(" 3. 179.13      6.07   SunOS sol 5.6 SUNW,Ultra-4 MDS   scr21   109'989'060 auau_ce_b0-2_4801_5000.xdf\n");
printf("\n ======== Your system: \n");
gBenchmark->Show("XDFcopy");

}
