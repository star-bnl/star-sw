/*******************************************************************
 *
 * $Id: StTofGeometry.h,v 1.2 2002/01/22 04:57:03 geurts Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Interface to TOFp database
 *
 *****************************************************************
 *
 * $Log: StTofGeometry.h,v $
 * Revision 1.2  2002/01/22 04:57:03  geurts
 * STAR dBase access routine, bugfixes and doxygenized
 *
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
#ifndef STTOFGEOMETRY_H
#define STTOFGEOMETRY_H
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "tofSlatGeom.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StMaker;


struct StructTofSlatEta{
  int ieta;
  float cosang;
  float eta;
  float eta_max;
  float eta_min;
  float r;
  float z;
  float z_min;
  float z_max;
};


struct StructTofSlatPhi{
  int iphi;
  float phi;
  float phi_max;
  float phi_min;
};


struct StructTofParam {
  //int detector;
  int i_eta_max,i_eta_min;
  int i_phi_max,i_phi_min;
  int n_counter_eta, n_counter_phi;
  int n_tray_eta, n_tray_phi;
  float counter_thickness, counter_width;
  float r;
  float tray_height, tray_width, tray_length, tray_phi_zero;
};

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<Int_t> idVector;  
typedef idVector::iterator idVectorIter;
#else
typedef vector<Int_t,allocator<Int_t>> idVector; 
typedef idVector::iterator idVectorIter;
#endif


struct StructSlatHit {
  Int_t             slatIndex;   
  StThreeVectorD    hitPosition;
  idVector          trackIdVec;
};


#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<tofSlatGeom_st> slatGeomVector;
typedef vector<StructTofSlatEta> tofSlatEtaVector;  
typedef vector<StructTofSlatPhi> tofSlatPhiVector;  
typedef vector<StructSlatHit> tofSlatHitVector; 
typedef vector<StructSlatHit> tofSlatHitVector; 
#else
typedef vector<tofSlatGeom_st,allocator<tofSlatGeom_st>> slatGeomVector;
typedef vector<StructTofSlatEta,allocator<StructTofSlatEta>> tofSlatEtaVector; 
typedef vector<StructTofSlatPhi,allocator<StructTofSlatPhi>> tofSlatPhiVector; 
typedef vector<SructSlatHit,allocator<StructSlatHit>> tofSlatHitVector;
typedef vector<SructSlatHit,allocator<StructSlatHit>> tofSlatHitVector;
#endif   
typedef slatGeomVector::iterator slatGeomIter;
typedef vector<StructSlatHit>::iterator tofSlatHitVectorIter; 
typedef vector<StructSlatHit>::iterator tofSlatHitVectorIter; 



class StTofGeometry{
 private:
  StructTofParam     mTofParam;      //! global geometry variables
  tofSlatEtaVector   mTofSlatEtaVec; //! vector with eta-dependent variables.
  tofSlatPhiVector   mTofSlatPhiVec; //! vector with phi settings
  slatGeomVector     mTofSlatVec;    //! combined geometry structure
  unsigned short mTofDaqMap[48];     //! daq-to-slatId map
  unsigned short mTofSlatMap[42];    //! slatId-to-daq map (derived from mTofDaqMap)

  int calcSlatId(const int iphi, const int ieta) const;
  void initGeomFromXdf(const Char_t* = 
		       "/afs/rhic/star/users/geurts/public/dbase/ctg_pars.xdf");
  void initGeomFromDbase(StMaker*);
  void initDaqMap();

 public:
  StTofGeometry();
  ~StTofGeometry();

  void init();         // init the dbase and get the data from it
  void init(StMaker*); // init the dbase and get the data from it
  StructTofParam tofParam() const; // geometry access member

  // return slat geometry of slatId
  tofSlatGeom_st  tofSlat(const Int_t slatId) const;

  // function to calculate the normal vector to a slat 
  StThreeVectorD tofSlatNormPoint(const Int_t slatId) const;

  // function to calculate the normal vector to a slats-plane 
  StThreeVectorD tofPlaneNormPoint(const Int_t slatId) const;

  // print out stuff
  void printGeo(ostream& os = cout) const;
  void printSlat(const Int_t slatId, ostream& os = cout) const;

  // tofCross members (note return value of tofSlatCross changed from Bool_t)
  int tofSlatCross(const StThreeVectorD& point, const tofSlatGeom_st tofSlat) const;
  int tofSlatCrossId(const StThreeVectorD& point) const;
  int tofSlatCrossId(const int volumeId) const;

  tofSlatHitVector tofHelixToArray(const StPhysicalHelixD& helix,
				   idVector slatIdVec);

  unsigned short daqToSlatId(const int) const;
  int slatIdToDaq(const Int_t) const;
};

inline StructTofParam StTofGeometry::tofParam()
     const {return mTofParam;}
inline unsigned short StTofGeometry::daqToSlatId(const int daqId)
     const {return mTofDaqMap[daqId];}
inline int StTofGeometry::slatIdToDaq(const Int_t slatId)
     const {return mTofSlatMap[slatId];}

#endif
