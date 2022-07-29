void PrintListOfFunctions() {
  TIter next(gROOT->GetListOfFunctions());
  TObject *obj;
  while ((obj = next())) {
    cout << obj->GetName() << "\t" << obj->GetTitle() << endl;
  }
}
