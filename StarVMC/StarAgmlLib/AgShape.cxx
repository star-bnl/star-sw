#include "AgShape.h"
ClassImp(AgShape);

#include <iostream>
#include "AgBlock.h"

#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include "TGeoArb8.h"
#include "TGeoTube.h"
//#include "TGeoTubeSeg.h"
#include "TGeoCone.h"
//#include "TGeoConeSeg.h"
#include "TGeoSphere.h"
#include "TGeoPara.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoEltu.h"
#include "TGeoHype.h"
//#include "TGeoCtub.h"

#include "TClass.h"

#include <assert.h>

#include "StMessMgr.h"

std::map< Int_t, std::vector<TString> > AgShape::mParList;
std::map< TString, Int_t >              AgShape::mShapeIds;
std::map< Int_t, TString >              AgShape::mShapeNames;
std::map< Int_t, TString >              AgShape::mRootCtors;   // ROOT constructors
std::map< Int_t, std::vector<TString> > AgShape::mRootMembers; // ROOT data members
//
// =============================================================================================================
//
// Simpleton class to initialize the names of TGeo parameters in the order in which they appear
// on the constructor's arguementlists.  Setup a preprocessor macro to help prevent carpal tunnel.
//
// =============================================================================================================
//


// MACRO Add: associates a given shape with a given parameter
#define Add(shape,par) {					\
    AgShape::mParList[AgShape::k##shape].push_back(#par);	\
    AgShape::mShapeIds[#shape]=AgShape::k##shape;		\
    AgShape::mShapeNames[AgShape::k##shape]=#shape;		\
  }
// MACRO Ctr: associates a given shape constructor with a given shape name
#define Ctr(shape,name) {				\
    AgShape::mRootCtors[ AgShape::k##shape]=#name;	\
  }
// MACRO Mbr: Provides the ordered list specifying the parameters of a ROOT shape
#define Mbr(shape,name) {						\
    AgShape::mRootMembers[ AgShape::k##shape].push_back(#name);}

//
struct _Dummy { 
  _Dummy()
  { // TGeoBBox(dx,dy,dz);
    Add(Bbox,dx);  Add(Bbox,dy);  Add(Bbox,dz);
    Mbr(Bbox,fDX); Mbr(Bbox,fDY); Mbr(Bbox,fDZ);
    Ctr(Bbox,TGeoBBox);
    // TGeoTrd1(Double_t dx1, Double_t dx2, Double_t dy, Double_t dz)
    Add(Trd1,dx1); Add(Trd1,dx2); Add(Trd1,dy);  Add(Trd1,dz);
    Mbr(Trd1,fDx1);Mbr(Trd1,fDx2);Mbr(Trd1,fDy); Mbr(Trd1,fDz);
    Ctr(Trd1,TGeoTrd1);
    // TGeoTrd2(Double_t dx1, Double_t dx2, Double_t dy1, Double_t dy2, Double_t dz)
    Add(Trd2,dx1); Add(Trd2,dx2); Add(Trd2,dy1); Add(Trd2,dy2); Add(Trd2,dz);
    Mbr(Trd2,fDx1);Mbr(Trd2,fDx2);Mbr(Trd2,fDy1);Mbr(Trd2,fDy2);Mbr(Trd2,fDz);
    Ctr(Trd2,TGeoTrd2);
    //TGeoTrap(Double_t dz, Double_t theta, Double_t phi, Double_t h1, Double_t bl1, Double_t tl1, Double_t alpha1, Double_t h2, Double_t bl2, Double_t tl2, Double_t alpha2)
    Add(Trap,dz);  Add(Trap,thet); Add(Trap,phi);
    Mbr(Trap,fDz); Mbr(Trap,fTheta); Mbr(Trap,fPhi);
    Add(Trap,h1);  Add(Trap,bl1);  Add(Trap,tl1); Add(Trap,alp1);
    Mbr(Trap,fH1); Mbr(Trap,fBl1); Mbr(Trap,fTl1);Mbr(Trap,fAlpha1);
    Add(Trap,h2);  Add(Trap,bl2);  Add(Trap,tl2); Add(Trap,alp2);
    Mbr(Trap,fH2); Mbr(Trap,fBl2); Mbr(Trap,fTl2);Mbr(Trap,fAlpha2);
    Ctr(Trap,TGeoTrap);
    // TGeoTube(Double_t rmin, Double_t rmax, Double_t dz)
    Add(Tube,rmin); Add(Tube,rmax); Add(Tube,dz);
    Mbr(Tube,fRmin);Mbr(Tube,fRmax);Mbr(Tube,fDz);
    Ctr(Tube,TGeoTube);
    // TGeoTubeSeg(Double_t rmin, Double_t rmax, Double_t dz, Double_t phi1, Double_t phi2)
    Add(Tubs,rmin); Add(Tubs,rmax); Add(Tubs,dz); Add(Tubs,phi1); Add(Tubs, phi2);
    Mbr(Tubs,fRmin);Mbr(Tubs,fRmax);Mbr(Tubs,fDz);Mbr(Tubs,fPhi1);Mbr(Tubs, fPhi2);
    Ctr(Tubs,TGeoTubeSeg);
    // TGeoCone(Double_t dz, Double_t rmin1, Double_t rmax1, Double_t rmin2, Double_t rmax2)
    Add(Cone,dz); Add(Cone,rmn1); Add(Cone,rmx1); Add(Cone,rmn2); Add(Cone,rmx2);
    Mbr(Cone,fDz);Mbr(Cone,fRmin1);Mbr(Cone,fRmax1);Mbr(Cone,fRmin2);Mbr(Cone,fRmax2);
    Ctr(Cone,TGeoCone);
    // TGeoConeSeg(Double_t dz, Double_t rmin1, Double_t rmax1, Double_t rmin2, Double_t rmax2, Double_t phi1, Double_t phi2)
    Add(Cons,dz); Add(Cons,rmn1); Add(Cons,rmx1); Add(Cons,rmn2); Add(Cons,rmx2); Add(Cons,phi1); Add(Cons,phi2);
    Mbr(Cons,fDz);Mbr(Cons,fRmin1);Mbr(Cons,fRmax1);Mbr(Cons,fRmin2);Mbr(Cons,fRmax2);Mbr(Cons,fPhi1);Mbr(Cons,fPhi2);
    Ctr(Cons,TGeoConeSeg);

    // TGeoSphere(Double_t rmin, Double_t rmax, Double_t theta1 = 0, Double_t theta2 = 180, Double_t phi1 = 0, Double_t phi2 = 360)
    Add(Sphe,rmin); Add(Sphe,rmax); Add(Sphe,the1); Add(Sphe,the2); Add(Sphe,phi1); Add(Sphe,phi2);
    Mbr(Sphe,fRmin);Mbr(Sphe,fRmax);Mbr(Sphe,fTheta1);Mbr(Sphe,fTheta2);Mbr(Sphe,fPhi1);Mbr(Sphe,fPhi2);
    Ctr(Sphe,TGeoSphere);

    // TGeoPara(Double_t dx, Double_t dy, Double_t dz, Double_t alpha, Double_t theta, Double_t phi)
    Add(Para,dx); Add(Para,dy); Add(Para,dz); Add(Para,alph); Add(Para,thet); Add(Para,phi);
    Mbr(Para,fX); Mbr(Para,fY); Mbr(Para,fZ); Mbr(Para,fAlpha); Mbr(Para,fTheta); Mbr(Para,fPhi);
    Ctr(Para,TGeoPara);
    // 	TGeoPcon(const char* name, Double_t phi, Double_t dphi, Int_t nz)
    Add(Pcon,phi1); Add(Pcon,dphi); Add(Pcon,nz);
    Mbr(Pcon,fPhi);Mbr(Pcon,fDphi);Mbr(Pcon,fNz);
    Ctr(Pcon,TGeoPcon);
    // 	TGeoPgon(const char* name, Double_t phi, Double_t dphi, Int_t nedges, Int_t nz)
    Add(Pgon,phi1); Add(Pgon,dphi); Add(Pgon,npdiv); Add(Pgon,nz);
    Mbr(Pgon,fPhi);Mbr(Pgon,fDphi);Mbr(Pgon,fNedges);Mbr(Pgon,fNz);
    Ctr(Pgon,TGeoPgon);
    // 	TGeoEltu(Double_t a, Double_t b, Double_t dz)
    Add(Eltu,p1); Add(Eltu,p2); Add(Eltu,dz);
    Mbr(Eltu,fA); Mbr(Eltu,fB); Mbr(Eltu,fDz);
    Ctr(Eltu,TGeoEltu);
    // 	TGeoHype(Double_t rin, Double_t stin, Double_t rout, Double_t stout, Double_t dz)
    Add(Hype,dz); Add(Hype,rin); Add(Hype,stin); Add(Hype,rout); Add(Hype,stout);
    Mbr(Hype,fDz); Mbr(Hype,fRin); Mbr(Hype,fStin); Mbr(Hype,fRout); Mbr(Hype,fStout);   
    /*
    Add(Hype,rin); Add(Hype,stin); Add(Hype,rout); Add(Hype,stout); Add(Hype,dz);
    Mbr(Hype,fRin);Mbr(Hype,fStin);Mbr(Hype,fRout);Mbr(Hype,fStout);Mbr(Hype,fDz);
    */
    Ctr(Hype,TGeoHype);

    // 	TGeoGtra(Double_t dz, Double_t theta, Double_t phi, Double_t twist, Double_t h1, Double_t bl1, Double_t tl1, Double_t alpha1, Double_t h2, Double_t bl2, Double_t tl2, Double_t alpha2)
    Add(Gtra,dz); 
    Add(Gtra,thet); 
    Add(Gtra,phi); 
    Add(Gtra,h1); 
    Add(Gtra,bl1); 
    Add(Gtra, tl1); 
    Add(Gtra,alp1); 
    Add(Gtra,h2); 
    Add(Gtra,bl2); 
    Add(Gtra,tl2); 
    Add(Gtra,alp2);
    Add(Gtra,twis); 

    Mbr(Gtra,fDz);
    Mbr(Gtra,fTheta);
    Mbr(Gtra,fPhi);
    Mbr(Gtra,fH1);
    Mbr(Gtra,fBl1);
    Mbr(Gtra,fTl1);
    Mbr(Gtra,fAlpha1);
    Mbr(Gtra,fH2);
    Mbr(Gtra,fBl2);
    Mbr(Gtra,fTl2);
    Mbr(Gtra,fAlpha2);
    Mbr(Gtra,fTwist);

    Ctr(Gtra,TGeoGtra);


    // 	TGeoCtub(Double_t rmin, Double_t rmax, Double_t dz, Double_t phi1, Double_t phi2, Double_t lx, Double_t ly, Double_t lz, Double_t tx, Double_t ty, Double_t tz)
    Add(Ctub,rmin); Add(Ctub,rmax); Add(Ctub,dz); Add(Ctub,phi1); Add(Ctub,phi2);
    Mbr(Ctub,fRmin);Mbr(Ctub,fRmax);Mbr(Ctub,fDz);Mbr(Ctub,fPhi1);Mbr(Ctub,fPhi2);
    Add(Ctub,lx);   Add(Ctub,ly);   Add(Ctub,lz);
    Mbr(Ctub,fNlow[0]); Mbr(Ctub,fNlow[1]); Mbr(Ctub,fNlow[2]);
    Add(Ctub,hx);   Add(Ctub,hy);   Add(Ctub,hz);
    Mbr(Ctub,fNhigh[0]); Mbr(Ctub,fNhigh[1]); Mbr(Ctub,fNhigh[2]);
    Ctr(Ctub,TGeoCtub);
    // 	TGeoTorus(Double_t r, Double_t rmin, Double_t rmax, Double_t phi1 = 0, Double_t dphi = 360)
    Add(Torus,r); Add(Torus,rmin); Add(Torus,rmax); Add(Torus,phi1); Add(Torus,dphi);
    Ctr(Torus,TGeoTorus);
    // Division
    //$$$		Add(Division,iaxis); Add(Division,ndiv); Add(Division,start); Add(Division,step); Add(Division,numed);
    Add(Division,iaxis); Add(Division,ndiv); Add(Division,c0); Add(Division,start); Add(Division,step); Add(Division,numed); // ??  Add(Division,serial);
    // and meh
  };
} _dummy;
#undef Add
#undef Ctr
#undef Mbr

// =============================================================================================================
AgShape::AgShape(const Char_t *name): TNamed(name,"An AgShape") 
{
  mId = mShapeIds[ GetName() ];
  mIsRunTime = false;
  // If this is a division, pull in the serial parameter from the parent
  // block's attributes
  if ( mId == kDivision )
    {	    
      AgAttribute *attr = AgBlock::previous()->attribute();
      if ( attr->isSet("serial") )
	{
	  par("serial") = attr->par("serial");
	}
    }
}

// =============================================================================================================
AgShape::AgShape(const AgShape &other)
{
  SetName( other.GetName() );
  SetTitle( other.GetTitle() );

  mId = mShapeIds[ GetName() ];
  mParameters = other.mParameters;
  mZ = other.mZ;  mRmin = other.mRmin;  mRmax = other.mRmax;

  mStoredShapes = other.mStoredShapes;
  mStoredParams = other.mStoredParams;

  mBlock = other.mBlock;
  mModule = other.mModule;

  mIsRunTime = other.mIsRunTime;
}

// =============================================================================================================
Int_t AgShape::type(){ return mId; }

// =============================================================================================================
void AgShape::Print( Option_t *otps ) const
{

  if ( mId == kUnknown )
    {
      for ( UInt_t i=kBbox;i<=kDivision;i++ )
	{
	  TString name = mShapeNames[i];
	  LOG_INFO << "+ " << name.Data() << ":";
	  for ( UInt_t j=0;j<mParList[i].size();j++ )
	    {
	      LOG_INFO << " " << mParList[i][j];
	    }
	  LOG_INFO << endm;
	}
    }
  else
    {
      TString name = GetName();
      LOG_INFO << "[+] " << name.Data() << ":";
      std::map<std::string, Double_t > mypar=mParameters;
      for ( UInt_t j=0;j<mParList[mId].size();j++ )
	{
	  TString key=mParList[mId][j];
	  if ( isSet(key.Data()) )
	    {  LOG_INFO << Form(" %s=%7.4g",key.Data(),mypar[key.Data()]); }
	  else
	    { LOG_INFO << Form(" %s=<unset>",key.Data()); }
	}
      LOG_INFO << endm;

      std::vector<Double_t> zz=mZ;
      std::vector<Double_t> rmin=mRmin;
      std::vector<Double_t> rmax=mRmax;

      for ( UInt_t i=0;i<zz.size();i++ )
	{
	  LOG_INFO << Form("  [%02i] z=%7.4g rmin=%7.4g rmax=%7.4g",i,zz[i],rmin[i],rmax[i]) << endm;
	}
    }

}


// =============================================================================================================
Bool_t AgShape::operator == ( const AgShape &other ) const
{

  // TODO: Is this level of tolerance really small enough?
  //$$	const Double_t tolerance = 0.005;
  //$$  const Double_t tolerance = 0.005E-15;
  const Double_t tolerance=0.0;

  // Check if same memory location
  if ( this == &other )
    return true;

  // Check if they are not the same shape
  if ( mId != other.mId )
    return false;

  // Check if all of the set parameters agree
  std::vector<TString> parlist = mParList[ mId ];

  // Check if this is a division, and return false if
  // if is...
  if ( mId == kDivision ) return false;
	  

  std::map<std::string, Double_t> mypar = mParameters;
  std::map<std::string, Double_t> itspar = other.mParameters;

  for ( UInt_t i=0;i<parlist.size();i++ )
    { 
      const char* key = parlist[i].Data();

      if ( isSet(key) and other.isSet(key) )
	{
	  Double_t par1 = mypar[key];
	  Double_t par2 = itspar[key];
	  if ( TMath::Abs( par1 - par2 ) > tolerance ) return false;
	}

      if ( isSet(key) and !other.isSet(key) ) return false; // Or one set other not
      if ( !isSet(key) and other.isSet(key) ) return false;

    }

  if ( mId != kPcon and mId != kPgon ) return true;

  // Check the sections fo pcons and pgons
  std::vector<Double_t> zz = other.mZ;
  std::vector<Double_t> rmin=other.mRmin;
  std::vector<Double_t> rmax=other.mRmax;
  for ( UInt_t i=0;i<mZ.size();i++ )
    {
      if ( TMath::Abs(mZ[i]    - zz[i])   > tolerance   ) return false;
      if ( TMath::Abs(mRmin[i] - rmin[i]) > tolerance   ) return false;
      if ( TMath::Abs(mRmax[i] - rmax[i]) > tolerance   ) return false;
    }

  return true;
}

// =============================================================================================================
#if 0 // lift
Bool_t AgShape::isSet( const Char_t *par ) const
{
  TString key=par;
  return ( mParameters.find(key) != mParameters.end() );
}
#endif
Bool_t AgShape::hasPar(const Char_t *par ) const
{
  TString key=par;
  // Unknown shapes may have any parameter
  if ( mId == kUnknown ) return true;
  // Otherwise we are limited to the parameter list
  std::vector<TString> parlist = mParList[ mId ];
  for ( UInt_t i=0;i<parlist.size();i++ )
    if ( parlist[i] == key ) return true;
  return false;
}
// =============================================================================================================
Double_t &AgShape::par( const Char_t *name )
{ static Double_t _dummy = 0;
  // key=name;

  // If the named parameter is not appropriate for this shape,
  // issue a warning and return a reference to _dummy
  if ( !hasPar(name) )
    {
      if ( AgBlock::active() )
	{
	  TString block=AgBlock::active()->GetName();
	  Warning("par(const Char_t *name)",Form("Shape %s has no parameter %s in volume %s",GetName(),name,block.Data()));
	}
      else
	{
	  Warning("par(const Char_t *name)",Form("Shape %s has no parameter %s",GetName(),name));
	}
      return _dummy;
    }

  return mParameters[name];
}

// Double_t AgShape::GetPar( const Char_t *name )
// { static Double_t _dummy = 0;
//   TString key=name;
//   // If the named parameter is not appropriate for this shape,
//   // issue a warning and return a reference to _dummy
//   if ( !hasPar(name) )
//     {
//       Warning("par(const Char_t *name)",Form("Shape %s has no parameter %s",GetName(),name));
//       return _dummy;
//     }

//   return mParameters[key];
// }

// =============================================================================================================
Double_t &AgShape::Z( UInt_t i )
{ static Double_t _dummy = 0;
  if ( mId != kPcon and mId != kPgon )
    {
      Warning("Z(UInt_t i)",Form("Shape %s is not a pcon or a pgon",GetName()));
      return _dummy;
    }
  if ( i>=mZ.size() ) mZ.resize(i+1);
  return mZ[i];
}

Double_t &AgShape::Rmin( UInt_t i )
{ static Double_t _dummy = 0;
  if ( mId != kPcon and mId != kPgon )
    {
      Warning("Rmin(UInt_t i)",Form("Shape %s is not a pcon or a pgon",GetName()));
      return _dummy;
    }
  if ( i>=mRmin.size() ) mRmin.resize(i+1);
  return mRmin[i];
}
Double_t &AgShape::Rmax( UInt_t i )
{ static Double_t _dummy = 0;
  if ( mId != kPcon and mId != kPgon )
    {
      Warning("Rmax(UInt_t i)",Form("Shape %s is not a pcon or a pgon",GetName()));
      return _dummy;
    }
  if ( i>=mRmax.size() ) mRmax.resize(i+1);
  return mRmax[i];
}



// =============================================================================================================
TGeoShape *AgShape::Make()
{
  if ( mId == kUnknown or mId == kDivision ) return NULL; // This is a division of an existing volume/shape

  //  Info("AgShape::Make()",GetTitle());
  //  Print();

  UInt_t size = mParameters.size();
  if ( mId == kPcon or mId == kPgon ) size += 3*par("nz");
  assert(size);

  // Create the local storage for the parameters
  Double_t *param = new Double_t[ size ];

  // Push the data onto the parameters in the order expected
  for ( UInt_t i=0;i<mParList[mId].size(); i++ )
    {
      param[i] = par( mParList[mId][i] );
    }


  // For the case of the pcon and pgon, we need to add in the sections
  Int_t offset=0;
  if ( mId==kPcon ) offset=3;
  if ( mId==kPgon ) offset=4;
  if ( offset )
    for ( UInt_t i=0;i<mZ.size();i++ )
      {
	param[offset+3*i+0]=mZ[i];
	param[offset+3*i+1]=mRmin[i];
	param[offset+3*i+2]=mRmax[i];
      }

  // We also need to work around a bug in ROOT where the phi angle is
  // not properly reset for phi < 0
  if ( mId==kPcon || mId==kPgon )
    {
      if ( param[0] < 0 ) param[0]+=360.0;
    }


  // Obtain the TClass corresponding to the requested shape
  TClass *_class = TClass::GetClass( mRootCtors[ mId ] );
  if ( !_class )
    {
      Warning("Make()","Runtime error.  Could not find TClass/dictionary %s for shape %s",
	      mRootCtors[mId].Data(),mShapeNames[mId].Data());

      delete [] param; param=0;

      return NULL;
    }

  TGeoShape *_shape = (TGeoShape *)_class->New();
  _shape->SetDimensions(param);

  // Need to compute the bounding box
  _shape->ComputeBBox();

  // Store shape for later
  mStoredShapes.push_back( _shape );
  mStoredParams.push_back( mParameters );

  delete [] param; param = 0;

  return _shape;

}
// =============================================================================================================
TGeoShape *AgShape::MakeIfUnique()
{

  // Create a new shape unless the shape matches the past state of
  // an existing shape

  for ( UInt_t i=0;i<mStoredShapes.size();i++ )
    {
      TGeoShape *_shape = mStoredShapes[i];
      std::map<std::string, Double_t> params=mStoredParams[i];
      // loop over parameters
      Bool_t equal = true;
      for ( UInt_t j=0;j<mParList[mId].size();j++ )
	{
	  TString key=mParList[mId][j];
	  Double_t a = par(key.Data());
	  Double_t b = params[key.Data()];
	  equal &= a==b;
	}

      // NEED TO ADD CODE TO CHECK DIMENSIONS OF PCON/PGON
      if ( mId == kPcon or mId == kPgon )
	{
	  TGeoPcon *pcon=(TGeoPcon*)_shape;
	  equal&=  par("nz") == (Double_t)pcon->GetNz();
	  if ( equal )
	    {
	      for ( Int_t ii=0;ii<pcon->GetNz();ii++ )
		{
		  equal &= Z(ii)==pcon->Z(ii);
		  equal &= Rmin(ii)==pcon->Rmin(ii);
		  equal &= Rmax(ii)==pcon->Rmax(ii);
		}
	    }

	}

      if ( equal ) return _shape;
    }

  return Make();

}
// =============================================================================================================
void AgShape::Inherit( AgBlock *block )
{

  AgShape *other = block->shape();
  if ( other -> type() == AgShape::kDivision )
    {
      // In the case of divisions, we inherit shape parameters
      // from the grandmother volume
      AgBlock *grand_mother=block->mother();
      assert(grand_mother);
      other=grand_mother->shape();
    }
  
  // List of parameters eligible for inheritance
  std::vector< TString > parlist = mParList[ mId ];

  // Loop over these parameters
  for ( UInt_t i=0;i<parlist.size();i++ )
    {
      TString key=parlist[i];
      
      // Do not inherit parameters which are set by the shape command,
      // unless this has been flagged as a runtime shape.
      Bool_t skip = isSet(key);
      if ( skip )
	{
	  continue;
	}
      
      if ( other->hasPar(key) && other->isSet(key) )
	{
	  par(key) = other->par(key);
	}
    }

  //
  // Now set defaults for any parameter which remains unset
  //
  switch (mId)
    {
    case (AgShape::kSphe):
      if ( !isSet("rmin") ) par("rmin")=0;
      if ( !isSet("the1") ) par("the1")=0;
      if ( !isSet("the2") ) par("the2")=180;
      if ( !isSet("phi1") ) par("phi1")=0;
      if ( !isSet("phi2") ) par("phi2")=360;
      break;
    case (AgShape::kTorus):
      if ( !isSet("phi1") ) par("phi1")=0;
      if ( !isSet("dphi") ) par("dphi")=360;
      break;
    case (AgShape::kTubs):
      if ( !isSet("phi1") ) par("phi1")=0;
    case (AgShape::kTube):
      if ( !isSet("rmin") ) par("rmin")=0;
      break;
    case (AgShape::kCons):
      if ( !isSet("phi1") ) par("phi1")=0;
    case (AgShape::kCone):
      if ( !isSet("rmn1") ) par("rmn1")=0;
      if ( !isSet("rmn2") ) par("rmn2")=0;
      break;
    case (AgShape::kCtub):
      if (!isSet("hx")&&!isSet("hy")&&!isSet("hz")) {par("hx")=0.; par("hy")=0.; par("hz")=1.; }
      if (!isSet("lx")&&!isSet("ly")&&!isSet("lz")) {par("lx")=0.; par("ly")=0.; par("lz")=1.; }
      break;
    case (AgShape::kDivision):
      if ( !isSet("step") )   par("step")=0;
      if ( !isSet("start" ) ) par("start")=0;
      if ( !isSet("c0") )     par("c0")=0;
      if ( !isSet("numed" ) ) par("numed")=0;
      break;
    default:
      /* nada */
      break;
    }

}

// =============================================================================================================
Bool_t AgShape::morphed()
{
  for ( UInt_t i=0;i<mStoredParams.size();i++ )
    {
      std::map<std::string, Double_t> params=mStoredParams[i];
      // loop over parameters
      Bool_t equal = true;
      for ( UInt_t j=0;j<mParList[mId].size();j++ )
	{
	  TString key=mParList[mId][j];
	  Double_t a = par(key);
	  Double_t b = params[key.Data()];
	  equal &= a==b;
	}
      if ( true ) return false; // Found one match
    }
  return true; // No shapes match
}

// =============================================================================================================
Bool_t AgShape::parameterized()
{
  if ( mIsRunTime )
    {
      return true;  // Once set as a parameterized shape, it is always a parameterized shape
    }

  Bool_t go = true;
  for ( UInt_t i=0;i<mParList[mId].size();i++ )
    {
      TString key = mParList[mId][i];
      go &= par(key)==0;
    }

  if ( go ) // Once it detected as parameterized it is always parameterized
    {
      mIsRunTime = true;
    }
  return go;
}
