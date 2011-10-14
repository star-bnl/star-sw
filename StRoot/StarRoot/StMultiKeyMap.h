#ifndef MULTYKEYMAP_H
#define MULTYKEYMAP_H
#include <vector>
class StMultiKeyMapIter;
class StMultiKeyNode;

class StMultiKeyMap
{
friend class StMultiKeyMapIter;
public:
StMultiKeyMap(int nKeys);
~StMultiKeyMap();
void Clear(const char *opt="");
void Add(const void *obj,const float  *keys);
void Add(const void *obj,const double *keys);
const StMultiKeyNode *GetTop() const {return mTop;}
      StMultiKeyNode *GetTop()       {return mTop;}
double Quality();
void MakeTree();
int    ls(const char *file="") const;
int    Size() const;
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
friend class StMultiKeyMapIter;
public:
StMultiKeyNode(int nKeys);
StMultiKeyNode(const StMultiKeyNode &source);
virtual ~StMultiKeyNode();
virtual void Set(const void *obj,const float  *keys);
        void Set(const void *obj,const double *keys);
virtual void Add(StMultiKeyNode *node,        StMultiKeyNode **keeper);
virtual void Add(const void *obj,const float *keys, StMultiKeyNode **keeper);
virtual       double Quality();
virtual       int    ls(const char *file="") const;

  const float *GetKeys() const { return mKeys;}
        float  GetKey()  const { return (mObj)? mKeys[int(mIKey)]:mDiv;}
        void   SetKey(float f) { assert(!mObj); mDiv = f;}
        int    GetIKey() const { return mIKey;} 
        int    GetNKey() const { return mNKey;} 
        int    GetNumb(int way) const { return mNumb[way];}
        int    Size() const    { return GetNumb(0)+GetNumb(1);}
        void  *GetObj () const { return (void*)mObj ;}
void Clear();
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
short  mNumb[2];    		//Number of left/right objects
StMultiKeyNode *mLink[2];	//Left/Right subtree pointers
const void *mObj;			//Some user objact mapped to keys
union {
  float *mKeys;			//keys for this object
  float  mDiv;			//one key if it is pure node
};
int    mId;
};


class StMultiKeyMapIter
{
public:
StMultiKeyMapIter(const StMultiKeyNode *node,const float *kMin=0,const float *kMax=0);
void          Set(const StMultiKeyNode *node,const float *kMin=0,const float *kMax=0);

~StMultiKeyMapIter();
StMultiKeyNode *operator*() const { return (StMultiKeyNode*)mStk[mLev];}
StMultiKeyMapIter &operator++();
int Level() const {return mLev;}
float *getKMin() const {return mKMin;}
float *getKMax() const {return mKMax;}
const int *Touched() const {return mTouched;}

private:
void Down(const StMultiKeyNode *node);
void SelfCheck();
int FilterLeft(const StMultiKeyNode *node) const;
int FilterRite(const StMultiKeyNode *node) const;
protected:
int mBoundsOn;
mutable int mTouched[2];
std::vector<float> mMinMax;
float *mKMin;
float *mKMax;
int mNK;
int mLev;
std::vector<const StMultiKeyNode*> mStk;
};
#endif //MULTYKEYBINTREE_H
