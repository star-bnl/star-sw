void TestTDatime() {
  UInt_t time = 1486585817;
  TUnixTime u(time); cout << "TUnixTime u(" << time << ") = " << u.GetLString();
  cout << "GMT\t" << u.GetGString() << endl;
  Int_t dd = 20170208; 
  Int_t tt = 203017;
  TDatime d(dd,tt); cout << "TDatime d(" << dd << "," << tt << ") = " << d.AsString() << endl;
  TUnixTime u2(d,kTRUE); 
  cout << "GMT\t" << u2.GetGString() << endl;
  cout << "u = " << u() << "\tu2 = " << u2() << endl;
}
