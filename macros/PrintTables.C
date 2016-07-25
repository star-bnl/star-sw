void PrintTables(const Char_t *MakerN="geant") {
  if (! StMaker::GetChain() ) return;
  StMaker *mk = StMaker::GetChain()->Maker(MakerN);
  if (! mk) return;
  TDataSetIter dir(mk);
  TDataSet *data = dir.Cd(".data");
  if (! data) return;
  TDataSetIter tableI(data);
  TTable *table = 0;
  while ((table = (TTable *) tableI())) {
    if (! table->HasData()) continue;
    Int_t N = table->GetNRows();
    if (N > 10) N = 10;
    table->Print(0,N);
  }
}
