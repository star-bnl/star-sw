TestSaveView(St_NodeView *view)
{
//  gSystem->MakeDirectory(GetName());
//  gSystem->ChangeDirectory(GetName());
  if (!view) return;
  
  St_DataSetIter next(view);
  St_NodeView *view = 0;
  Int_t iCounter = 0;
  Char_t  buffer[100];
  TString fileName;
  while( (view = (St_NodeView *)next()) )
  {
       fileName = view->GetName();
       sprintf(buffer,"%d",iCounter);
       fileName += buffer;
       ofstream out;
       out.open(fileName.Data());
       cout << iCounter+1 << ". " << fileName << endl;
       view->SavePrimitive(out);
       out.close();
       iCounter++;
  }    
  cout << iCounter << " files have been create" << endl;
};
