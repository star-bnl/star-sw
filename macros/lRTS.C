void lRTS() {
  gSystem->Load("libStDb_Tables");
  gSystem->Load("RTS");
  gSystem->AddIncludePath(" -D_LARGEFILE64_SOURCE -I$STAR/StRoot/RTS/include -I$STAR/StRoot/RTS/trg/include -I$STAR/StRoot/RTS/include/TPC -I$STAR/StRoot/RTS/src/SFS -I$STAR/StRoot/RTS/src -I$STAR/StRoot/RTS/src/EVP_READER");
}
