void Convert() {
  struct Date_t {Int_t date, time;};
  Date_t DT[] = {
    {20010701, 120000},
    {20010924, 4},
    {20030106, 0},
    {20040104, 1},
    {20040205, 1},
    {20040217, 1},
    {20040324, 1},
    {20050111, 220001},
    {20050403, 10000},
    {20060308, 115800},
    {20060406, 50000},
    {20060510, 150601},
    {20070321, 42}
  };
  Int_t N = sizeof(DT)/sizeof(Date_t);
  TDatime t;
  for (Int_t i = 0; i < N; i++) {
    t.Set(DT[i].date,DT[i].time);
    UInt_t u = t.Convert();
    cout << u << ",\t" << DT[i].date << ",\t" << DT[i].time << endl;
  }
}
