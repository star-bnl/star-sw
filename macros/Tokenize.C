void Tokenize(TString opt = "gstar,Y2004,tfs,tcl,SvtSlowSim,tpcI,svtIT,ITTF,Physics,Idst,event,analysis,EventQA,tags,Tree,evout,miniMcMk",
	      TString separator = "[^ ;,]+") { 
#if 1

  TObjArray *array = opt.Tokenize(separator);
  cout << "GetEntriesFast\t" << array->GetEntriesFast() << endl;
  cout << "GetEntries\t" << array->GetEntries() << endl;
  cout << "GetSize\t" << array->GetSize() << endl;
  TIter next(array);
  TObjString *objs;
  while ((objs = (TObjString *) next())) {cout << objs->GetString() << endl;}
#else
  TObjArray array;
  &array = opt.Tokenize(separator);
  TIter next(&array);
  while ((objs = (TObjString *) next())) {cout << objs->GetString() << endl;}
#endif
}
