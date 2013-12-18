#ifndef MULTIKEYMAP_H
#define MULTIKEYMAP_H
#include <vector>
class StMultiKeyMapMIter;
class StMultiKeyNode;

class StMultiKeyMapM
{
friend class StMultiKeyMapMIter;
public:
StMultiKeyMapM(int nKeys);
~StMultiKeyMapM();
void Clear(const char *opt="");
void Add(const void *obj,const float  *keys);
void Add(const void *obj,const double *keys);
const StMultiKeyNode *GetTop() const 	{return mTop;}
      StMultiKeyNode *GetTop()       	{return mTop;}
                void  SetTop(StMultiKeyNode *top) {mTop = top;}
double Quality();
   int MakeTree(int keepArray=0);
   int ls(const char *file="") const;
   int Size() const;
   int GetNKey() const { return mNKey;} 
StMultiKeyNode** GetArr() 		{return &mArr[0];}
// statics
static void Test();
static void Test2();


protected:
int mNKey;
StMultiKeyNode *mTop;
std::vector<StMultiKeyNode*> mArr; //!
};



class StMultiKeyNode
{
friend class StMultiKeyMapMIter;
public:
StMultiKeyNode(int nKeys);
StMultiKeyNode(const StMultiKeyNode &source);
virtual ~StMultiKeyNode();
virtual void Set(const void *obj,const float  *keys);
        void Set(const void *obj,const double *keys);
        void Add(const void *obj,const float  *keys);
virtual void Add(StMultiKeyNode *node);
virtual       double Quality();
virtual       int    ls(const char *file="") const;

  const float *GetKeys() const { return mKeys;}
        float  GetKey()  const { return mKeys[int(mIKey)];}
        int    GetIKey() const { return mIKey;} 
        void   SetIKey(int ik) { mIKey=ik;    } 
        int    GetNKey() const { return mNKey;} 
        int    GetNumb(int way) const { return mNumb[way];}
        int    Size() const    { return mNumb[0]+mNumb[1]+1;}
        void  *GetObj () const { return (void*)mObj ;}
        void Clear();
static int GetNInst();
//	Non user functions
protected:
StMultiKeyNode *LLink() const {return mLink[0];}
StMultiKeyNode *RLink() const {return mLink[1];}
private:
void Init();


// statics
public:

protected:
char   mNKey;			//keys number defined
char   mIKey;			//key number used on this level
int    mNumb[2];    		//Number of left/right objects
StMultiKeyNode *mLink[2];	//Left/Right subtree pointers
const void *mObj;		//Some user object mapped to keys
float *mKeys;			//keys for this object
int    mId;
};


class StMultiKeyMapMIter
{
public:
StMultiKeyMapMIter(const StMultiKeyNode *node,const float *kMin=0,const float *kMax=0);
void          Set(const StMultiKeyNode *node,const float *kMin=0,const float *kMax=0);
void       Update(const float *kMin=0,const float *kMax=0);

~StMultiKeyMapMIter();
StMultiKeyNode *operator*() const { return (StMultiKeyNode*)mStk[mLev];}
StMultiKeyMapMIter &operator++();
int Level() const {return mLev;}
float *GetKMin() const {return mKMin;}
float *GetKMax() const {return mKMax;}
const int *Touched() const {return mTouched;}
private:
void Left(const StMultiKeyNode *node);
int FullCheck();
int FilterLeft(const StMultiKeyNode *node) const;
int FilterRite(const StMultiKeyNode *node) const;
protected:
mutable int mTouched[3];
std::vector<float> mMinMax;
float *mKMin;
float *mKMax;
int mNK;
int mLev;
std::vector<const StMultiKeyNode*> mStk;
};
#endif //MULTYKEYBINTREE_H
