#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#pragma link C++ class TMemStat-;
#pragma link C++ class TTreeHelper-;
#pragma link C++ class THelixTrack-;
#pragma link C++ class THack-;
#pragma link C++ class TUnixTime;
#pragma link C++ class TDirIter;
#pragma link C++ class TRArray+;
#pragma link C++ class TRMatrix+;
#pragma link C++ class TRSymMatrix+;
#pragma link C++ class TRVector+;

#pragma link C++ enum TRArray::ETRMatrixType;
#pragma link C++ enum TRArray::ETRMatrixCreatorsOp;

#pragma link C++ function operator-=(TRArray &, const Double_t &);
#pragma link C++ function operator+=(TRArray &, const Double_t &);
#pragma link C++ function operator*=(TRArray &, const Double_t &);
#pragma link C++ function operator*(const TRArray &, const TRArray &);
#pragma link C++ function operator-=(TRArray &, const TRArray &);
#pragma link C++ function operator+=(TRArray &, const TRArray &);
#pragma link C++ function operator==(TRArray &, const Double_t &);
#pragma link C++ function operator==(TRArray &, const TRArray &);
#pragma link C++ function operator<<(ostream &, const TRArray &);
#pragma link C++ function operator>>(istream &, TRArray &);

#pragma link C++ function operator<<(ostream &, const TRMatrix &);
#pragma link C++ function operator<<(ostream &, const TRSymMatrix &);
#pragma link C++ function operator<<(ostream &, const TRVector &);
