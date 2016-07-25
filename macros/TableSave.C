void TableSave(TTable *tab=0) {
  if (! tab) return;
  ofstream out;
  TString file(tab->GetName());
  file += ".C";
  out.open(file);
  tab->SavePrimitive(out);
  out.close();
  cout << file << " created" << endl;
}
