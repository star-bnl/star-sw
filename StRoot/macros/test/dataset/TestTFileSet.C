{
  gROOT->Reset();
  gSystem.Load("libStar.so");
 
  TFileSet set("./");
  TDataSet *nextset=0;
  TDataSetIter next(&set,0);
  while(nextset = next())  { 
    nextset->ls();
    printf(" Level %d %s \n",
          next->GetDepth(),
          nextset->IsFolder()?"Directory":"File" ); 
  }
}

