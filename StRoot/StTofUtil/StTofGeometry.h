/*******************************************************************
 *
 * $Id: StTofGeometry.h,v 1.1 2001/09/28 19:09:40 llope Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Interface to TOFp database
 *
 *****************************************************************
 *
 * $Log: StTofGeometry.h,v $
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
#ifndef STTOFGEOMETRY_H
#define STTOFGEOMETRY_H
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#define NTOFSLATS 48    //wjl

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

struct StructSlatGeom {
  int ieta;
  float z, z_min, z_max, cosang,r;
  float eta_min, eta_max, eta;
  int iphi;
  float phi, phi_min, phi_max;
  //Int_t            trayId; 
  int            trayId; 
};


#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StructSlatGeom> slatGeomVector;
typedef vector<StructTofSlatEta> tofSlatEtaVector;  
typedef vector<StructTofSlatPhi> tofSlatPhiVector;  
#else
typedef vector<StructSlatGeom, allocator<StructSlatGeom> > slatGeomVector;
typedef vector<StructTofSlatEta, allocator<StructTofSlatEta> > tofSlatEtaVector; 
typedef vector<StructTofSlatPhi, allocator<StructTofSlatPhi> > tofSlatPhiVector; 
#endif   
typedef slatGeomVector::iterator slatGeomIter;


#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<Int_t>  idVector;  
typedef idVector::iterator idVectorIter;
#else
typedef vector<Int_t, allocator<Int_t> >  idVector; 
typedef idVector::iterator idVectorIter;
#endif   

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

struct StructSlatHit {
  Int_t             slatIndex;   
  StThreeVectorD    hitPosition;
  idVector          trackIdVec;
};

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StructSlatHit>  tofSlatHitVector; 
#else
typedef vector<SructSlatHit, allocator<StructSlatHit> >  tofSlatHitVector;
#endif   
typedef vector<StructSlatHit>::iterator tofSlatHitVectorIter; 

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StructSlatHit>  tofSlatHitVector; 
#else
typedef vector<SructSlatHit, allocator<StructSlatHit> >  tofSlatHitVector;
#endif   
typedef vector<StructSlatHit>::iterator tofSlatHitVectorIter; 


class StTofGeometry{
 private:
  StructTofParam     mTofParam;           //! global geometry variables
  tofSlatEtaVector   mTofSlatEtaVec;      //! vector with eta-dependent variables.
  tofSlatPhiVector   mTofSlatPhiVec;      //! vector with phi settings
  slatGeomVector     mTofSlatVec;         //! combined geometry structure
  unsigned short mTofDaqMap[NTOFSLATS];              //! daq-to-slat-id map
  int calcSlatId(int iphi, int ieta) const; 
  void initGeomFromXdf();
  void initDaqMap();
 public:
  StTofGeometry();
  ~StTofGeometry();

  void init(); // init the dbase and get the data from it
  StructTofParam   tofParam()      const;    // geometry access member
  //slatGeomVector   tofSlatVec()    const; 

  // return slat geometry of slatId
  StructSlatGeom  tofSlat(Int_t slatId)  const;

  // function to calculate the normal vector to a slat 
  StThreeVectorD tofSlatNormPoint(Int_t slatId) const;

  // function to calculate the normal vector to a slats-plane 
  StThreeVectorD tofPlaneNormPoint(Int_t slatId) const;

  // print out stuff
  void printGeo(ostream& os = cout)                const;
  void printSlat(Int_t slatId, ostream& os = cout) const;

  // tofCross members (note return value of tofSlatCross changed from Bool_t)
  int tofSlatCross(StThreeVectorD& point, StructSlatGeom tofSlat) const;
  int tofSlatCrossId(StThreeVectorD& point) const;
  int tofSlatCrossId(int volumeId) const;


  tofSlatHitVector tofHelixToArray(StPhysicalHelixD& helix,
                          idVector slatIdVec);

  unsigned short daqToSlatId(int) const;
};

inline StructTofParam StTofGeometry::tofParam()          const {return mTofParam;}
inline unsigned short StTofGeometry::daqToSlatId(int i) const {return mTofDaqMap[i];}

#endif
