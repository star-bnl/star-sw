#ifndef MULTYKEYMAP_H
#define MULTYKEYMAP_H
#include <vector>
#include <list>
class StMultyKeyMapIter;
class StMultyKeyNode;
class StMultyKeyMap;

class StMultyKeyNode
{
public:
typedef std::vector<float> FKeys_t;
typedef std::vector<StMultyKeyNode*> Links_t;
friend class StMultyKeyMap;
friend class StMultyKeyMapIter;

StMultyKeyNode():mMap(0),mId(0){;} 
virtual ~StMultyKeyNode(){;}
virtual const char * ClassName() const {return "StMultyKeyNode";}
virtual void Set(const void *obj,const float  *keys){assert(0);}
virtual void Add(StMultyKeyNode *pair){assert(0);}

virtual   const float *GetKeys() const 		{  	    return 0;}
virtual         int    GetIKey() const 		{ assert(0);return 0;} 
virtual         void   SetIKey(int ik) 		{ assert(0)         ;} 
virtual         void  *GetObj () const 		{           return 0;}
virtual   const float *GetDow() const     	{ assert(0);return 0;}
virtual   const float *GetStp() const     	{ assert(0);return 0;}
                int    MaxDeep(int *deep=0) const; 	
                int    GetNumb() const; 	
                double Quality(int *numb=0) const; 	
virtual         int    ls(const char* ="" ) const;
virtual         void   Clear(const char* =0)	{ assert(0)         ;}
//	Non user functions
virtual StMultyKeyNode* Link(int jlink) const 	{ return 0   ;}
        StMultyKeyMap  *GetMap() const  	{ return mMap;}
void    SetMap(StMultyKeyMap *map)    		{ mMap = map ;}
int     GetNKey() const; 
int     GetNBin() const; 
private:
public:
StMultyKeyMap *mMap;
int mId;
};


class StMultyKeyDivd: public StMultyKeyNode
{
friend class StMultyKeyMap;
friend class StMultyKeyMapIter;
public:
StMultyKeyDivd();
virtual ~StMultyKeyDivd();
const char *ClassName() const {return "StMultyKeyDivd";}
void Add(StMultyKeyNode *pair);

int    GetIKey() const 		{ return mIKey ;} 
void   SetIKey(int ikey) 	{ mIKey = ikey ;} 
const float *GetDow() const     { return &mDow[0];}
const float *GetStp() const     { return &mStp[0];}

void   Clear(const char* opt=0);
//	Non user functions
StMultyKeyNode *Link(int jlink) const 	{return mLink[jlink];}
protected:
int    mIKey;		//key index used on this level
Links_t mLink;	//subtree pointers
FKeys_t mDow;
FKeys_t mStp;
};

class StMultyKeyPair: public StMultyKeyNode
{
friend class StMultyKeyMap;
friend class StMultyKeyMapIter;
public:
StMultyKeyPair();
virtual ~StMultyKeyPair(){;}
const char *ClassName() const {return "StMultyKeyPair";}
virtual void Set(const void *obj,const float  *keys);

  const float *GetKeys() const { return &mKeys[0];}
        void  *GetObj () const { return (void*)mObj ;}
        void Clear(const char* opt=0);
//	Non user functionsprivate:
protected:
const void *mObj;		//Some user object mapped to keys
FKeys_t mKeys;			//keys for this object
};

class StMultyKeyMap: public StMultyKeyDivd
{
friend class StMultyKeyMapIter;
friend class StMultyKeyDivd;
friend class StMultyKeyPair;
public:
StMultyKeyMap(int nKeys,int nBins=10);
~StMultyKeyMap();
void Clear(const char *opt="");
void Add(StMultyKeyNode* pair)		{StMultyKeyDivd::Add(pair);}
void Add(const void *obj,const float  *keys);
void Add(const void *obj,const double *keys);
const StMultyKeyNode *GetTop() const 	{return this;}
      StMultyKeyNode *GetTop()       	{return this;}
   int MakeTree(int keepArray=0);
   int Size() const 			{return mSize;}
   int GetNKey() const 			{return mNKey;} 
   int GetNBin() const 			{return mNBin;} 
   int GetJKey();  			 
StMultyKeyNode** GetArr() 		{return &mArr[0];}
StMultyKeyPair *MakePair();
StMultyKeyDivd *MakeNode();
// statics
static void Test();
static void Test2();

protected:
int mNKey;
int mNBin;
int mSize;
unsigned int mJKey;
std::vector<StMultyKeyNode*> mArr; //!
std::list<StMultyKeyPair*> mPairs; //!
std::list<StMultyKeyDivd*> mNodes; //!
};


class StMultyKeyMapIter
{
public:
class myStk_t {public:const StMultyKeyNode *node; int ibin;int rite;};

  StMultyKeyMapIter(const StMultyKeyNode *node,const float *kMin=0,const float *kMax=0);
  void	Set(const StMultyKeyNode *node,const float *kMin=0,const float *kMax=0);
  void	Update(const float *kMin=0,const float *kMax=0);
  void	Reset();
   int	InitLev();

  ~StMultyKeyMapIter();
  StMultyKeyNode *operator*() const { return (StMultyKeyNode*)mStk[mLev].node;}
  StMultyKeyMapIter &operator++();
  int Level() const 		{return mLev;}
  const StMultyKeyMapIter::myStk_t &GetStk(int lev=-1) const {return (lev<0)? mStk[mLev]:mStk[lev];}
  float *GetKMin() const 	{return mKMin;}
  float *GetKMax() const 	{return mKMax;}
  const int *Touched() const 	{return mTouched;}
  private:
  int FullCheck();
  protected:
  mutable int mTouched[3];
  std::vector<float> mMinMax;
  float *mKMin;
  float *mKMax;
  int mNK;
  int mNB;
  int mLev;
  const StMultyKeyNode* mTop;
  std::vector<myStk_t> mStk;
};


inline int    StMultyKeyNode::GetNKey() const 	{ return mMap->GetNKey();} 
inline int    StMultyKeyNode::GetNBin() const 	{ return mMap->GetNBin();} 
#endif //MULTYKEYBINTREE_H
