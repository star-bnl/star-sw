void testLoad(const Char_t *lib="") {
  gSystem->Load("libpgf77VMC");
  gSystem->Load("libSt_base");
  gSystem->Load("libStUtilities");
  gSystem->Load(lib);
}
