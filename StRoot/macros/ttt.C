{
   gSystem->Load("St_base.so");
G__loadfile("/afs/rhic/star/packages/dev/.share/tables/ctg_slat_phi.h");
St_Table tabs("ctg_slat_phi",1);
tabs.StafStreamer();
}
