void Check2Tables(TTable *table1 = 0, TTable *table2 = 0) {
  /*
    root.exe tpcPadGainT0B.20071127.151800.root StarDb/Calibrations/tpc/tpcPadGainT0B.20071127.151801.root
    gSystem->Load("libStDb_Tables.so")
    TTable *tabl1 = (TTable *) _file0->Get("tpcPadGainT0B")
    TTable *tabl2 = (TTable *) _file1->Get("tpcPadGainT0B")
    .x Check2Tables.C(tabl1,tabl2)
   */
  if (! table1 || ! table2) return;
  TTable *table[2] = {table1, table2};
  Int_t NRows[2] = {0,0};
  for (int idb = 0; idb < 2; idb++) {
    NRows[idb] = table[idb]->GetNRows();
  }
  Int_t nrows = TMath::Min(NRows[0],NRows[1]);
  if (NRows[0] != NRows[1]) {
    cout << "No. of rows is different " << NRows[0] << "\t" << NRows[1] << endl; 
  }
  if (TString(table[0]->GetName()) != TString(table[1]->GetName())) {
    cout << "Names are different " << table[0]->GetName() << " != " << table[1]->GetName() << endl;
    return;
  }
  if (TString(table[0]->GetTitle()) != TString(table[1]->GetTitle())) {
    cout << "Titles are different " << table[0]->GetTitle() << " != " << table[1]->GetTitle() << endl;
    return;
  }
  if (table[0]->GetRowSize() != table[1]->GetRowSize()) {
    cout << "RowSizes are different " << table[0]->GetRowSize() << " != " << table[1]->GetRowSize() << endl;
    return;
  }
  Char_t *c0 = table[0]->GetArray();
  Char_t *c1 = table[1]->GetArray();
  int n = nrows*table[0]->GetRowSize();
  Int_t NN = nrows;
  if (NN > 10) NN = 10;
  if (memcmp(c0,c1,n)) {
    cout << "Content is different" << endl;
    cout << "Db table ===========================" << endl;
    //    table[0]->Print(0,NN);
    cout << "Cint table ===========================" << endl;
    //    table[1]->Print(0,NN);
    tpcPadGainT0B_st *t0 = ((St_tpcPadGainT0B*) table[0])->GetTable();
    tpcPadGainT0B_st *t1 = ((St_tpcPadGainT0B*) table[1])->GetTable();
    for (Int_t s = 0; s < nrows; s++, t0++,t1++) {
      for (Int_t row = 0; row < 45; row++) {
	if (row == 12) continue;
	for (Int_t pad = 0; pad < 182; pad++) {
	  if (t0->T0[row][pad]   != t1->T0[row][pad] || 
	      t0->Gain[row][pad] != t1->Gain[row][pad]) {
	    cout << "s/r/p " << s+1 << " / " << row+1 << " / " << pad+1 
		 << "\tGain new/old = " << t0->Gain[row][pad] << " / " << t1->Gain[row][pad] 
		 << "\tT00 new/old = " << t0->T0[row][pad] << " / " << t1->T0[row][pad] << endl;
	  }
	}
      }
    }
  }
  else                  {
    cout << "Table are the same" << endl;
  }
}
