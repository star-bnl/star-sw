void save(Char_t path, TTable *t) {
  ofstream *out = new ofstream(path);
  cout << "Open " << path << endl;
  t->SavePrimitive(*out,"");
  delete out;
}
