/*****************************************************************
 * $Id: StTofDatabase.h,v 1.1 2001/04/24 15:56:08 geurts Exp $ 
 *
 * Author: Wei-Ming Zhang, April 2001 
 *
 *****************************************************************
 * Description:
 * TOF geometry and parameter database
 *
 *****************************************************************
 *                              
 * $Log: StTofDatabase.h,v $
 * Revision 1.1  2001/04/24 15:56:08  geurts
 * *** empty log message ***
 *
 *
 *****************************************************************/

#ifndef ST_TOF_GEOMETRY_H
#define ST_TOF_GEOMETRY_H

#define TOF_SIM_GEOMETRY 1
#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "ctf/St_ctg_Module.h"
#include "ctf/St_cts_Module.h"
#include "StThreeVectorD.hh"

typedef St_ctg_geo           StafStTofParam; 
typedef St_ctg_slat          StafStTofSlatParam; 
typedef St_ctg_slat_eta      StafStTofSlatEta; 
typedef St_ctg_slat_phi      StafStTofSlatPhi; 
typedef St_cts_mpara         StafStTofSimParam; 
typedef St_g2t_track         StafStG2TTrack; 
typedef St_g2t_ctf_hit       StafStG2THit; 

typedef ctg_geo_st           StTofParam; 
typedef ctg_slat_st          StTofSlatParam; 
typedef ctg_slat_eta_st      StTofSlatEta; 
typedef ctg_slat_phi_st      StTofSlatPhi; 
typedef cts_mpara_st         StTofSimParam; 
typedef g2t_track_st         StTofG2TTrack; 
typedef g2t_ctf_hit_st       StTofG2THit; 

    struct StructTofSlat {
  	  StructTofSlat() { }
          StTofSlatParam     slatParam;      
          StTofSlatEta       slatEta;      
          StTofSlatPhi       slatPhi;      
          Int_t              trayId;   // trayId is new (not in STAF database) 
    };

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<StructTofSlat> tofSlatVector;
    typedef vector<StructTofSlat>::iterator tofSlatIter;
    typedef vector<ctg_slat_eta_st> tofSlatEtaVector;  
    typedef vector<ctg_slat_phi_st> tofSlatPhiVector;  
#else
    typedef vector<StructTofSlat, allocator<StructTofSlat> > tofSlatVector;
    typedef vector<StrcutTofSlat, allocator<StructTofSlat> >::iterator tofSlatIter;
    typedef vector<ctg_slat_eta_st, allocator<ctg_slat_eta_st> > tofSlatEtaVector; 
    typedef vector<ctg_slat_phi_st, allocator<ctg_slat_phi_st> > tofSlatPhiVector; 
#endif   

class StTofDatabase {
public:

    StTofDatabase();
    virtual ~StTofDatabase();

    void fillDb();               // fill TOF Db with xdf files or StDatabase 
    double             version()                        const;
    StTofParam         tofParam()                       const;    
    tofSlatEtaVector   tofSlatEtaVec()                  const; 
    tofSlatPhiVector   tofSlatPhiVec()                  const; 
    tofSlatVector      tofSlatVec()                     const; 
    StTofSimParam      tofSimParam()                    const;    
    StructTofSlat*     tofSlat(Int_t slatId)            const;

// function to calculate the normal vector to a slat 
    StThreeVectorD     tofSlatNormPoint(Int_t slatId)   const;

// function to calculate the normal vector to a slats-plane 
    StThreeVectorD     tofPlaneNormPoint(Int_t slatId)  const;
	
    void   printGeo(ostream& os = cout)                 const;
    void   printSlat(Int_t slatId, ostream& os = cout)  const;
//  void   printSim(ostream& os = cout)                 const;
   
private:

    double             mVersion;            //! 
    StTofParam         mTofParam;           //!  
    tofSlatEtaVector   mTofSlatEtaVec;      //!
    tofSlatPhiVector   mTofSlatPhiVec;      //!
    tofSlatVector      mTofSlatVec;         //!  
    StTofSimParam      mTofSimParam;        //!  

};

inline double          StTofDatabase::version()           const 
{
    return mVersion;
}

inline StTofParam      StTofDatabase::tofParam()          const 
{
    return mTofParam;
}

inline StTofSimParam   StTofDatabase::tofSimParam()       const 
{
    return mTofSimParam;
}

inline tofSlatEtaVector   StTofDatabase::tofSlatEtaVec()  const 
{
    return mTofSlatEtaVec;
}

inline tofSlatPhiVector   StTofDatabase::tofSlatPhiVec()  const 
{
    return mTofSlatPhiVec;
}

inline tofSlatVector      StTofDatabase::tofSlatVec()     const 
{
  return mTofSlatVec;
}

#endif
