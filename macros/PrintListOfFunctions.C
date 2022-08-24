void PrintListOfFunctions(TH1 *hist = 0) {
  TCollection *col = 0;
  if (hist) col = hist->GetListOfFunctions();
  else      col = gROOT->GetListOfFunctions();
  if (! col) return;
  if (hist) {cout << "List of Functions for hist " << hist->GetName() << endl;}
  TIter next(col);
  TObject *obj;
  while ((obj = next())) {
    cout << obj->GetName() << "\t" << obj->GetTitle() << endl;
  }
}
