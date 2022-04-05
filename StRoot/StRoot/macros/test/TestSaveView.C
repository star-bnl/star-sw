TestSaveView(St_NodeView *viewFull)
{
//  gSystem->MakeDirectory(GetName());
//  gSystem->ChangeDirectory(GetName());
  if (!viewFull) return;
  
  St_DataSetIter next(viewFull,0);
  St_NodeView *view = 0;
  Int_t iCounter = 0;
//  Char_t  buffer[100];
  TString fileName;
  TString topDirectory="./";
  TString curDirectory = topDirectory;
  Char_t *fullPath = gSystem->ConcatFileName(curDirectory.Data(),viewFull->GetName());
  gSystem->MakeDirectory(fullPath);
  delete [] fullPath;

  while( (view = (St_NodeView *)next()) )
  {

       UInt_t numPosition = view->GetPosition()->GetId();
//       sprintf(buffer,";%d",numPosition);

       curDirectory = topDirectory;
       curDirectory += view->PathP();
//       curDirectory += buffer;

//       fileName  = view->GetName();
//       fileName += buffer;
       fileName = "Position";
       fileName += ".C.";
       fileName += "year_1b";

       // Create directory 
       cout << "Dir: " << curDirectory.Data() << endl;
       if (gSystem->MakeDirectory(curDirectory.Data())) {
          printf(" can not create %s for %s \n",curDirectory.Data(), view->GetName());
          return;
       }

       fullPath = gSystem->ConcatFileName(curDirectory.Data(),fileName.Data());
       ofstream out;
       out.open(fullPath);
//==       cout << iCounter+1 << ". " << fullPath << endl;
       delete [] fullPath;
       view->SavePrimitive(out);
       out.close();
       iCounter++;
  }    
  cout << iCounter << " files have been create" << endl;
};
