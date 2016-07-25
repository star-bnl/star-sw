void PrintZero(TTable *table=0) {
const Int_t NumberOfPadsAtRow[45] = {
  88, 96,104,112,118,126,134,142,150,158, // Inner
  166,174,182,
  98,100,102,104,106,106,108, // Outer
  110,112,112,114,116,118,120,122,122,124,
  126,128,128,130,132,134,136,138,138,140,
  142,144,144,144,144
};
  if (! table) return;
  tpcPadGainT0_st *t = ((St_tpcPadGainT0 *)table)->GetTable();
  Int_t sec, row, pad;
  for(sec=1;sec<=24;sec++) 
    for (row=1;row<=45;row++) {
      Int_t alive = 0;
      for (pad=1;pad<=NumberOfPadsAtRow[row];pad++) {
	if (t->Gain[sec-1][row-1][pad-1] < 0.1) {
	  cout << sec << "\t" << row << "\t" << "\t" << pad << "\t" << t->Gain[sec-1][row-1][pad-1] << endl;}
	else alive++;
      }
      if (! alive) cout << "=============== sec " << sec << "\trow " << row << "completely dead" << endl;
      else         cout << "============================== last pad " << pad << endl;
    }
}
