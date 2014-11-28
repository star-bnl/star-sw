/*******************************************************************
 *
 * $Id: StTofGeometry.h,v 1.8 2006/08/15 21:42:13 jeromel Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Interface to TOFp database
 *
 *****************************************************************
 *
 * $Log: StTofGeometry.h,v $
 * Revision 1.8  2006/08/15 21:42:13  jeromel
 * Fix rhic -> rhic.bnl.gov
 *
 * Revision 1.7  2005/07/06 19:25:55  fisyak
 * Use templated StThreeVector and StPhysicalHelix
 *
 * Revision 1.6  2004/06/10 15:53:50  dongx
 * -head file "StHelixD.hh" included
 * -simplify the macro definition
 *
 * Revision 1.5  2004/06/09 21:26:32  dongx
 * add function projTrayVector(..) to increase the matching speed
 *
 * Revision 1.4  2003/07/02 20:55:03  geurts
 * changed all InitXXX() methods from private to public
 *
 * Revision 1.3  2003/04/15 03:24:18  geurts
 * many, many changes:
 * . updated and extended StructSlatHit, introduced tofSlatHitVector and Iterator
 * . generalize (2+1)-D slat model from 3 layers to n layers, default n=5
 * . introduced new member functions which identify 3x3 and 5x5 neighbours
 * . introduced SetDebug() option.
 * . minor updates in mTofParam parameters
 *
 * Revision 1.2  2002/01/22 04:57:03  geurts
 * STAR dBase access routine, bugfixes and doxygenized
 *
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
#ifndef STTOFGEOMETRY_H
#define STTOFGEOMETRY_H
#include "tofSlatGeom.h"
#include "Rtypes.h"
#include "StHelixD.hh"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StMaker;
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"

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
typedef vector<Int_t,allocator<Int_t> > idVector; 
typedef idVector::iterator idVectorIter;
#endif


struct StructSlatHit {
  Int_t             slatIndex;   
  StThreeVectorD    hitPosition;
  idVector          trackIdVec;
  Int_t             hitProfile;
  Float_t           s;
  Float_t           theta_xy;
  Float_t           theta_zr;
  vector<StThreeVector<double> > layerHitPositions;
  Int_t             matchFlag;
};


#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<tofSlatGeom_st> slatGeomVector;
typedef vector<StructTofSlatEta> tofSlatEtaVector;  
typedef vector<StructTofSlatPhi> tofSlatPhiVector;  
typedef vector<StructSlatHit> tofSlatHitVector; 
#else
typedef vector<tofSlatGeom_st,allocator<tofSlatGeom_st> > slatGeomVector;
typedef vector<StructTofSlatEta,allocator<StructTofSlatEta> > tofSlatEtaVector; 
typedef vector<StructTofSlatPhi,allocator<StructTofSlatPhi> > tofSlatPhiVector; 
typedef vector<SructSlatHit,allocator<StructSlatHit> > tofSlatHitVector;
#endif   
typedef slatGeomVector::iterator slatGeomIter;
typedef vector<StructSlatHit>::iterator tofSlatHitVectorIter; 


class StTofGeometry{
 private:
  bool mDebug;                       //! debug flag
  StructTofParam     mTofParam;      //! global geometry variables
  tofSlatEtaVector   mTofSlatEtaVec; //! vector with eta-dependent variables.
  tofSlatPhiVector   mTofSlatPhiVec; //! vector with phi settings
  slatGeomVector     mTofSlatVec;    //! combined geometry structure
  unsigned short mTofDaqMap[48];     //! daq-to-slatId map
  unsigned short mTofSlatMap[42];    //! slatId-to-daq map (derived from mTofDaqMap)

  int calcSlatId(const int iphi, const int ieta) const;
 public:
  StTofGeometry();
  ~StTofGeometry();

  void SetDebug();
  bool Debug();
  void init();         // init the dbase and get the data from it
  void init(StMaker*); // init the dbase and get the data from it
  void initGeomFromXdf(const Char_t* = 
		       "/afs/rhic.bnl.gov/star/users/geurts/public/dbase/ctg_pars.xdf");
  void initGeomFromDbase(StMaker*);
  void initDaqMap();
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

  static const unsigned int mMaxSlatLayers;
  tofSlatHitVector tofHelixToArray(const StPhysicalHelixD& helix,
				   idVector slatIdVec);

  float slatHitPosition(StThreeVectorD*);  // calculate local hit position on slat
  float slatPhiPosition(StThreeVectorD*);  // calculate local hit position on slat
  unsigned short daqToSlatId(const int) const;
  int slatIdToDaq(const Int_t) const;
  idVector slatNeighbours(const int);
  idVector slatNeighboursWide(const int);

  Bool_t  projTrayVector(const StHelixD &helix, idVector &trayVec) const;

};

inline StructTofParam StTofGeometry::tofParam()
     const {return mTofParam;}
inline unsigned short StTofGeometry::daqToSlatId(const int daqId)
     const {return mTofDaqMap[daqId];}
inline int StTofGeometry::slatIdToDaq(const Int_t slatId)
     const {return mTofSlatMap[slatId];}
inline void StTofGeometry::SetDebug(){mDebug = true;}
inline bool StTofGeometry::Debug(){return mDebug;}

const unsigned int StTofGeometry::mMaxSlatLayers(5);
#endif
