
/*******************************************************************
 *
 * $Id: StEEmcSmdGeom.h,v 1.1 2003/03/28 15:50:00 balewski Exp $
 *
 * Author: Wei-Ming Zhang
 *****************************************************************
 *
 * Description: Interface to EEMC-SMD database
 *
 *****************************************************************
 *
 * $Log: StEEmcSmdGeom.h,v $
 * Revision 1.1  2003/03/28 15:50:00  balewski
 * first
 *
 *
 *******************************************************************/
#ifndef STEEMCSMDGEOM_H
#define STEEMCSMDGEOM_H
#include "StThreeVectorD.hh"
#include "StThreeVectorF.hh"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StMaker;

#define nStrips         288             
#define nMods           12             

struct StructEEmcSmdParam {
  float zPlane[3];
  float rOffset[3];
};

struct StructEEmcStripId{
  int  moduleId;
  int  layerId;
  int  etaId;
  int  planeId;
};

struct StructEEmcStrip{
  StructEEmcStripId stripId;
  StThreeVectorD    end1; 
  StThreeVectorD    end2; 
  float length;
};

// Define vector of strip pointers and its Iterator
#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<int> intVec;  
typedef vector<StructEEmcStrip*> EEmcStripPtrVec;  
#else
typedef vector<int,allocator<int>> intVec; 
typedef vector<StructEEmcStrip*,allocator<StructEEmcStrip*>> EEmcStripPtrVec; 
#endif   
typedef vector<StructEEmcStrip*>::iterator EEmcStripPtrVecIter; 

struct StructEEmcSmdModule {
  int   moduleId;	
  int   planeId;        
  float phiMin;
  float phiMax;
  float rMin;
  float rMax;
  EEmcStripPtrVec stripPtrVec;  
};

class StEEmcSmdGeom{
 private:
  StructEEmcSmdParam     mEEmcSmdParam;      //! general geometry variables
  StructEEmcSmdModule    mEEmcUModule[12];   //! 12 U modules. 
  StructEEmcSmdModule    mEEmcVModule[12];   //! 12 V modules. 
  bool                   mIsSectorIn[12];    //! sector status. 

  void initGeomFromFile(const Char_t* = "/star/u/wzhang/myafs/anlsmd/strip_geometry.txt");
  static StEEmcSmdGeom* sInstance;

 public:
  static StEEmcSmdGeom* instance();   // handle the only instance
  static StEEmcSmdGeom* instance(intVec sIdVec);   

  void init();         // init the dbase

// set sectors
  void setSectors(const intVec sIdVec); 

// return sector status  
  bool IsSectorIn(const Int_t sId) const; 

// instance and initialize a strip 
  StructEEmcStrip EEmcInitStrip();

// geometry access members
  // return SMD geometry parameters  
  StructEEmcSmdParam EEmcSmdParam() const; 

  // return SMD U module of mId 
  StructEEmcSmdModule EEmcUModule(const Int_t mId) const;

  // return SMD V module of mId 
  StructEEmcSmdModule EEmcVModule(const Int_t mId) const;

  // return module Id of a point 
  Int_t EEmcModuleId(const Int_t planeId, const StThreeVectorD& point) const;
  
  // return a DCA strip from a global point (float *dca carries a sign)  
  StructEEmcStrip EEmcStrip(const Int_t planeId, const StThreeVectorD& point, Float_t* dca);

  // match two strips 
  bool EEmcMatchStrips(const StructEEmcStripId stripId1, 
	  	       const StructEEmcStripId stripId2, Int_t nTolerance);

// print out memebers 
  void printGeom(ostream& os = cout) const;
  void printModule(const StructEEmcSmdModule Module, ostream& os = cout) const;
  void printStrip(const StructEEmcStrip Strip, ostream& os = cout) const;
  void printStripId(const StructEEmcStripId StripId, ostream& os = cout) const;
  
//protected
 protected:  
  StEEmcSmdGeom();
  virtual ~StEEmcSmdGeom(); 
};

inline bool StEEmcSmdGeom::IsSectorIn(Int_t sId)
       const {return mIsSectorIn[sId - 1];}
inline StructEEmcSmdParam StEEmcSmdGeom::EEmcSmdParam()
       const {return mEEmcSmdParam;}
inline StructEEmcSmdModule StEEmcSmdGeom::EEmcUModule(const Int_t mId) 
       const {return mEEmcUModule[mId - 1];}
inline StructEEmcSmdModule StEEmcSmdGeom::EEmcVModule(const Int_t mId) 
       const {return mEEmcVModule[mId - 1];}

#endif
 
