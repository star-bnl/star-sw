#ifndef STSVTCLUSTERMATRIX_HH
#define STSVTCLUSTERMATRIX_HH

#include "Stiostream.h"
#include <stdlib.h>
#include <math.h>
#include <new>

class StSvtClusterMatrix {

public:

   StSvtClusterMatrix();
   StSvtClusterMatrix(int mRow, int mCol);
   StSvtClusterMatrix(const StSvtClusterMatrix &);
   int  SetDimension(int mRow, int mCol=0);
   void GetDimension(int& mRow, int& mCol) const; 
   int Rows() const;
   int Columns() const;
   ~StSvtClusterMatrix();
  
  int mCluRow;//AA
  int mCluCol;//AA

  friend istream&  operator>>  (istream& s, StSvtClusterMatrix& Svtcluster);//AA
  double&   operator () ( int row, int col);//AA

private:
  double **mData;
  int MatrixAlloc(int mRow, int mCol);
  int MatrixDeAlloc();
};

#endif
