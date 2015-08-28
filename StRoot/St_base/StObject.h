// $Id: StObject.h,v 1.20 2015/08/28 19:54:18 perev Exp $
// $Log: StObject.h,v $
// Revision 1.20  2015/08/28 19:54:18  perev
// Add specific copy constructor to StObject.
// This ctr set zero to bit 1<<22. This boit means that object belongs
// to structured container. But copy obviously not.
//
// Revision 1.19  2012/06/11 15:08:41  fisyak
// std namespace, warn off for x64
//
// Revision 1.18  2012/02/21 18:48:37  perev
// I/O mode flag added
//
// Revision 1.15  2007/04/26 04:16:41  perev
// Remove senseless comment
//
// Revision 1.14  2007/03/15 16:27:32  fine
//  Allow user to select the arbitrary StEvent object to be drawn with StEventDisplay
//
// Revision 1.13  2002/11/26 02:23:38  perev
// new ROOT adoptation
//
// Revision 1.12  2001/05/30 17:46:42  perev
// StEvent branching
//
// Revision 1.11  2000/12/09 02:11:02  perev
// clone() called Clone()
//
//

#ifndef STAR_StObject
#define STAR_StObject
 
 
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StObject class is a base class to implement StEvent                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TDataSet.h"

class StXRefManager;
class StXRefMain;
class StRefArray;
class StStrArray;

enum EStObjectDrawBit { kMark2Draw = BIT(24)}; // mark object to be rendered by "EventDisplay"

//_____________________________________________________________________________
class StObject : public TObject {

public:
          StObject(){;}
          StObject(const StObject &sto);
          StObject &operator=(const StObject &sto);
  virtual ~StObject();
  virtual void Browse(TBrowser *b);
  
  virtual Bool_t IsFolder() 	const;
  virtual TObject *clone() 	const {return ((TObject*)this)->Clone();}
  Int_t   isZombie() 		const {return IsZombie();}
  virtual void makeZombie(int flg=1)
    {if (flg) {MakeZombie();} else {((UInt_t*)this)[1] &=~(kZombie);}} 
  UInt_t  Ztreamer(TBuffer &R__b);
  ClassDef(StObject,3) // Base class for StEvent

static UInt_t 	         fgTally;

};

//_____________________________________________________________________________
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  StUUDGen                                                             //
//  The uuidgen creates a new universally unique identifier (UUID)      //
//  using the libuuid(3) library.					//
//                                                                      //
//////////////////////////////////////////////////////////////////////////
class StUUId
{
protected:
   UInt_t fID[4];
public:
    	StUUId();
virtual ~StUUId(){};
void 	Generate();
StUUId  &operator=(const StUUId &from);
StUUId  &operator=(const char  *from);
Int_t  IsNull() const {return (fID[3]==0);}

//void  Streamer(TBuffer &b);
int Compare(const StUUId &u2) const;
ClassDef(StUUId,1)
};   


#include "TDataSet.h"

//_____________________________________________________________________________
class StXRef :public TDataSet 
{
public:
StXRef(const char *brName="", StXRefMain *evt=0, UInt_t tally=0);
virtual 		       ~StXRef();

virtual		StXRefMain     *MakeMain() = 0 ;
 virtual		      void      Synchro(int /* toMain */) = 0 ;
virtual		StXRefMain     *GetMain();
virtual		       int      IsMain() const		{return 0;}
virtual		      void      SetMain(StXRefMain *m) 	{fMain  = m;}
virtual		      void      Add(TDataSet *ds);
	const 	StUUId 	       &GetUUId() const       	{return   fUUId;}
		void         	SetUUId(const StUUId &id){fUUId    = id;}
		void         	GenUUId()		{fUUId.Generate();}
		void         	SetTally(UInt_t tally) 	{fTally = tally;}
		UInt_t         	GetTally()             	{return   fTally;}
private:

StUUId         fUUId;		//!
UInt_t         fTally; 		//!
StXRefMain     *fMain;          //!
ClassDef(StXRef,1)
};   



//_____________________________________________________________________________
class StXRefMain :public StXRef {
public:
    	 StXRefMain(const char *name="") : StXRef(name){};
virtual ~StXRefMain();
virtual  	void  		Split()=0;
virtual	       	StXRefMain     *MakeMain() {return this;}
virtual		      void      Synchro(int toMain){};
ClassDef(StXRefMain,1)
};

//_____________________________________________________________________________
class TPageMap  {

enum EPageMap {kPAGE=2048,kBITS=11,kBITZ=22,kMASK=0x7ff,kLAST=0xfffff800};
public:
 TPageMap();
~TPageMap();

ULong_t *GET(UInt_t udx) ;
ULong_t *Get(UInt_t udx) ;
void     GetMiMax(UInt_t &udxMin,UInt_t &udxMax) {udxMin=fMinUdx;udxMax=fMaxUdx;}

static  void     Test();
private:
ULong_t *NewPage();

ULong_t *fTopPage;
ULong_t *fLstPage;
UInt_t   fLstUdx;
ULong_t *fList;
UInt_t   fMinUdx;
UInt_t   fMaxUdx;


};
#ifndef __CINT__
#include <list>
#include <vector>
#ifndef ST_NO_NAMESPACES
using namespace std;
#endif
class StProxyUrr;

typedef list<StProxyUrr*>  	        StCollList;
typedef StCollList::iterator 		StCollListIter;
typedef list<StXRefManager*> 		StXRefManagerList;
typedef StXRefManagerList::iterator 	StXRefManagerListIter;
typedef vector<UInt_t> 			UIntVector;
typedef UIntVector::iterator 		UIntVectorIter;
//_____________________________________________________________________________


//_____________________________________________________________________________
class StProxyUrr : public UIntVector {
public:
  	StProxyUrr(TObject  *coll){fArr = coll;		   fType=0;}
  	StProxyUrr(TObject  **adr){fAdr = adr; 		   fType=1;}
  	StProxyUrr(StObject **adr){fAdr = (TObject **)adr; fType=1;}
       ~StProxyUrr(){fArr = 0;}
int       GetType() {return fType;}
TObject   *GetArr()  {return fArr;}
TObject  **GetAdr()  {return fAdr;}

private:
int  fType;
union {
  TObject  *fArr;
  TObject **fAdr;};
};


class StXRefManager : public TObject
{
public:
    	StXRefManager(const StUUId &id);
       ~StXRefManager();
        
        void    Cd();
        void    AddColl (      StProxyUrr *rarr);
        void    AddColl (const StStrArray *sarr);
        void    Update ();
        void    Clear (Option_t*);
static  void    Cd        (StXRef     *xref);
static  void    Open      (StXRef     *xref);
static  void    Close     (StXRef     *xref);
static  TDataSet *GetMain();

private:
        Int_t  fUpd;
        UInt_t fTally;
        StUUId fUUId;		//!
        StCollList  fColList;
        TPageMap  fObjTab;
        StXRefMain *fMain;
public:
static StXRefManagerList fgManagerList;
static StXRefManager 	*fgManager;
static int 	         fgRWmode; //0=read,1=write,-1=undefined

};

#endif // -__CINT__
#endif


















