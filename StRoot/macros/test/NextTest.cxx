{
 gROOT->Reset();
 gSystem.Load("St_base.so");
 
 St_FileSet set("./");
 St_DataSet *nextset=0;
 printf(" \n\ntry the simple ctor\n");
// TBrowser bb("Browser",&set);

 printf(" \n\ntry the complex ctor\n");
 St_DataSetIter next(&set,0);
 while(nextset = next())  { nextset->ls();  printf(" Level %d \n",next->GetDepth()); }
}
