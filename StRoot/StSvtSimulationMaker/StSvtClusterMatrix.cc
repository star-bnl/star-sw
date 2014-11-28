#include <Stiostream.h>
#include "StSvtClusterMatrix.hh"

StSvtClusterMatrix::StSvtClusterMatrix(){

mCluRow = 0;
mCluCol = 0;
mData = NULL;

} 

StSvtClusterMatrix::StSvtClusterMatrix(int mRow, int mCol){

  //initialize matrix

mCluRow = mRow;
mCluCol = mCol;
 
if ( MatrixAlloc(mRow, mCol) )
	  {
		 
		 exit(-1);
	  }
}

StSvtClusterMatrix::StSvtClusterMatrix(const StSvtClusterMatrix& cluster){
  //copy constructor

mCluRow = cluster.mCluRow;
mCluCol = cluster.mCluCol;

MatrixAlloc(mCluRow, mCluCol );  //allocate memory

 for( int i=0; i<mCluRow; i++)
  for( int j=0; j<mCluCol; j++)
    {
      mData[i][j] = cluster.mData[i][j] ;
    }

}

StSvtClusterMatrix::~StSvtClusterMatrix(){
  //delete the data

if(mData != NULL)
  MatrixDeAlloc(); //free the memory
    }

int StSvtClusterMatrix::Rows() const
{

return mCluRow;

}

int StSvtClusterMatrix::Columns() const
{

return mCluCol;

}


//AA
double& StSvtClusterMatrix::operator ()( int row, int col)
{//returns the value stored at nrow and col
	  return mData[row][col];
}



int StSvtClusterMatrix::SetDimension(int mRow, int mCol)
{  //set the dimension and allocates the memory of the matrix

	 if(mCol == 0) //if default column: square matrix
		 mCol = mRow;

	 StSvtClusterMatrix temp(mRow, mCol);

	 for(int i=0; i<mCluRow; i++)
	 for(int j=0; j<mCluCol; j++)
		temp(i,j) = mData[i][j]; //copy the existing data

	 //delete the existing data;
	MatrixDeAlloc();

	MatrixAlloc(mRow,mCol);

	mCluRow = mRow;
	mCluCol = mCol;

 	for(int i=0; i<mCluRow; i++)
	for(int j=0; j<mCluCol; j++)
		mData[i][j] = temp.mData[i][j]; //copy the existing data

	 return 0; //could be used to return the success
				  // or failure of allocation
}

void StSvtClusterMatrix::GetDimension(int& mRow, int& mCol) const
{  //report the dimension of the matrix
	 mRow = mCluRow;
	 mCol = mCluCol;
}

istream&  operator>>(istream& s, StSvtClusterMatrix& cluster)
{  //read matrix from a stream

	  for(int i=0; i<cluster.mCluRow; i++ )
	  for(int j=0; j<cluster.mCluCol; j++ )
	    s>>cluster(i,j);

	  return s;  
}

//private methods
int StSvtClusterMatrix::MatrixAlloc( int mRow, int mCol)
{
        try                                 //test for exceptions
         {				     
	   mData = new double*[mRow];       //allocate memory for rows
	   for(int j=0; j<mRow; j++)
	    mData[j] = new double[mCol];
	  }
	  catch (double*)
	  {  // ENTER THIS BLOCK ONLY IF xalloc IS THROWN.
		return -1;
	  }
	  return 0;
}

int StSvtClusterMatrix::MatrixDeAlloc()
{  //null matrix 
	 for (int i=0; i<mCluRow; i++)
		 delete[] mData[i];        // STEP 1: DELETE THE COLUMNS


		 delete[] mData;           // STEP 2: DELETE THE ROWS


	  return 0;
}
