#define DIST_SWITCH


#ifndef L3_COORDINATE_TRANSFORMER_H
#define L3_COORDINATE_TRANSFORMER_H

#include "l3Coordinates.h"

#include <math.h>

class l3CoordinateTransformer{
 public:
  // basic geometry fixed here ... this could be also taken from db
  static int    numberOfPadsAtRow[45];
  static double radialDistanceAtRow[45]; 
  static double SectorSin[24];
  static double SectorCos[24];
  static double innerSectorPadPitch;
  static double outerSectorPadPitch;

  // parameters of the transformation in different regions of the tpc 
  // east means sector 13-24, west means 1-12 
  double drift_length_inner ;
  double drift_length_outer ;
  double lengthPerTb;
  
 public:
  
  // max tb
  double max_tb_inner;
  double max_tb_outer;
  int transformation_errors;
  
 public:
  // Constructors
  l3CoordinateTransformer() ;  // Init parameters and variables
  virtual ~l3CoordinateTransformer() ; // Delete variables

  int LoadTPCLookupTable(char * filename = "/L3/etc/map.bin");
  
  // Memberfunctions
  // raw -> global
  void raw_to_global(const l3ptrsCoordinate &raw,
		     l3xyzCoordinate &global);
  
  void raw_to_local(const l3ptrsCoordinate &raw,
		    l3xyzCoordinate &local ) ;
  
  void local_to_global(const l3ptrsCoordinate &raw ,
		       const l3xyzCoordinate &local ,
		       l3xyzCoordinate &global ) ;
  
  // global -> raw
  void global_to_raw(const l3xyzCoordinate &global , 
		     l3ptrsCoordinate &raw ) ;
  void global_to_raw(int sector, int row, 
		     const l3xyzCoordinate &global , 
		     l3ptrsCoordinate &raw ) ;
  void global_to_local(const l3xyzCoordinate &global, 
		       l3xyzCoordinate &local ,
		       l3ptrsCoordinate &raw) ;
  void global_to_local(int sector, int row, 
		       const l3xyzCoordinate &global, 
		       l3xyzCoordinate &local ) ;
  void local_to_raw(const l3xyzCoordinate &global , 
		    const l3xyzCoordinate &local , 
		    l3ptrsCoordinate &raw ) ; 
  void local_to_raw( int row ,
		     const l3xyzCoordinate &local ,
		     l3ptrsCoordinate &raw  ) ;
  
  //
  //   Get parameters
  inline double GetSectorCos ( int i ) { 
    if ( i < 0 || i >= 24 ) 
      return -999. ;
    else 
      return SectorCos[i] ; 
  };

  inline double GetSectorSin ( int i ) { 
    if ( i < 0 || i >= 24 ) 
      return -999. ;
    else 
      return SectorSin[i] ; 
  };

  inline double GetRadialDistanceAtRow ( int i ) { 
    if ( i < 0 || i >= 45 ) 
      return -999. ;
    else 
      return radialDistanceAtRow[i] ; 
  };

  inline int GetNumberOfPadsAtRow ( int i ) 
      { 
	if ( i < 0 || i >= 45 ) 
	  return -999 ;
	else
	  return numberOfPadsAtRow[i] ; 
      } ;


  // Set paramaters needed for the transformation in different ways
  // This is only for the transformation z <-> timebucket the others are fixed


  //void Set_parameters_by_hand(const double lengthPerTb = .6176,    // 3.85 GeV
  void Set_parameters_by_hand(const double lengthPerTb = 0.584, 
			      const double drift_length_inner = 201., 
			      const double drift_length_outer = 201.) ;
  void Get_parameters_from_db() ;
  void Use_transformation_provided_by_db() ;
  void Print_parameters() ;

  inline double Get_drift_length_inner() { return drift_length_inner ;} ;
  inline double Get_drift_length_outer() { return drift_length_outer ;} ;
  inline double Get_lengthPerTb() { return lengthPerTb ; } ;
  inline double Get_max_timebucket_inner() { return  max_tb_inner; } ;
  inline double Get_max_timebucket_outter() { return  max_tb_outer; } ;
  
  inline int Get_transformation_errors() { return transformation_errors; } ;

 private:
  float dpad;
  float dtb;
  float maxtb;
  
  int npad, ntb;
  
  //int fd;
  //void * file;
  //int filesize;

  float *TPCmap;
};

#endif
