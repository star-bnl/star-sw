{
  //  Copy the XDF file to test XDF I/O function
// gSystem.Load("St_Module.dll");
// gSystem.Load("../mev/libSt_mevsim.dll");
 St_XDFFile  xdf_in("/star/mds/data/SD98/auau200/fast2/central/hijing/set0001/regular/auau_ce_b0-2_0001_0010.xdf");
 St_XDFFile  xdf_out("/scr2/fisyak/copy.xdf","w");
 
 St_DataSet *set = 0;

 while ( set = xdf_in.NextEventGet()) {
   xdf_out.NextEventPut(set);
   delete set;
   set = 0;
 }

 xdf_in.CloseXDF();
 xdf_out.CloseXDF();
}
