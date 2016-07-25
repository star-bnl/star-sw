void TrTest() {
  TRMatrix A(3,3," 1 2 3 1 2 3 4 5 6");
  cout << A;
  //  TRSymMatrix S(A,TRArray::kATxA);
  TRSymMatrix S(A,TRArray::kAxAT);
  cout << S;
  TRSymMatrix t1(S,TRArray::kInvertedA);
  cout << t1;
  TRSymMatrix t2(S,TRArray::kInvertedPosDef);
  cout << t2;
  TRSymMatrix t3(S,TRArray::kInverted);
  cout << t3;
  TRMatrix SA(S);
  cout << SA;
  cout << S;
  TRSymMatrix t2(S,TRArray::kInvertedA);  cout << "t2::kInvertedA\t" << t2;
  TRMatrix i2(SA,TRArray::kAxS,t2);      cout << "i2\t" << i2;
  TRSymMatrix t3(S,TRArray::kInvertedPosDef);  cout << "t3::kInvertedPosDef\t" << t3;
  TRMatrix i3(SA,TRArray::kAxS,t3);      cout << "i3\t" << i3;
  TRSymMatrix t1(S,TRArray::kInverted);  cout << "t1::kInverted\t" << t1;
  TRMatrix i1(SA,TRArray::kAxS,t1);      cout << "i1\t" << i1;
}
