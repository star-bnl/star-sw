#ifndef STMULTIARRAY_H
#define STMULTIARRAY_H

///class StMultiArray imitate multidimensional array. max number of
///dimensions == 8


/// class StMultiArrayBase is a base, non templated part of StMultiArray
class StMultiArrayBase
{
protected:
StMultiArrayBase(int n1,int n2);
StMultiArrayBase(int n1,int n2,int n3);
StMultiArrayBase(int n1,int n2,int n3,int n4);
StMultiArrayBase(int n1,int n2,int n3,int n4,int n5);
StMultiArrayBase(int n1,int n2,int n3,int n4,int n5,int n6);
StMultiArrayBase(int n1,int n2,int n3,int n4,int n5,int n6,int n7);
StMultiArrayBase(int n1,int n2,int n3,int n4,int n5,int n6,int n7,int n8);
void Init(int *sz,int n);
void Clear();
void AddIdx(int i);
int  GetIdx();
int  GetKdx();
public:
static void Test();
private:
int mNDes;
int mDes[8+1];
int mTally;
int mSft;
};

template<class T> 
class StMultiArray :public StMultiArrayBase {
public:
StMultiArray(int n1,int n2):StMultiArrayBase(n1,n2){mA=0;}
StMultiArray(int n1,int n2,int n3):StMultiArrayBase(n1,n2,n3){mA=0;}
StMultiArray(int n1,int n2,int n3,int n4):StMultiArrayBase(n1,n2,n3,n4){mA=0;}
StMultiArray(int n1,int n2,int n3,int n4,int n5):StMultiArrayBase(n1,n2,n3,n4,n5){mA=0;}
StMultiArray(int n1,int n2,int n3,int n4,int n5,int n6):StMultiArrayBase(n1,n2,n3,n4,n5,n6){mA=0;}
StMultiArray(int n1,int n2,int n3,int n4,int n5,int n6,int n7):StMultiArrayBase(n1,n2,n3,n4,n5,n6,n7){mA=0;}
StMultiArray(int n1,int n2,int n3,int n4,int n5,int n6,int n7,int n8):StMultiArrayBase(n1,n2,n3,n4,n5,n6,n7,n8){mA=0;}
operator T (){return mA[GetIdx()];}	
operator T*(){return mA+GetKdx();}	
StMultiArray &operator[](int i) {AddIdx(i); return *this;}
void operator=(T *array){mA=array; Clear();}
void operator=(T  value){mA[GetIdx()]=value;}
private:
T   *mA;
};
#endif //STMULTIARRAY_H

