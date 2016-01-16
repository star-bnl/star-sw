#include "StarTGeoStacker.h"
ClassImp(StarTGeoStacker);

//#define __DEBUG_BUILD__
//#define __DEBUG_PLACE__

#include "AgModule.h"
#include "AgBlock.h"
#include "AgPlacement.h"
#include "AgMaterial.h"
#include "AgShape.h"

#include "TGeoManager.h"
#include "TGeoMaterial.h"
#include "TGeoMedium.h"
#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoShape.h"
#include "TGeoMatrix.h"

#include "TGeoBBox.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include "TGeoArb8.h"
#include "TGeoTube.h"
#include "TGeoCone.h"
#include "TGeoSphere.h"
#include "TGeoPara.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoEltu.h"
#include "TGeoHype.h"
#include "TGeoTorus.h"
#include "TVector3.h"

#include "TClass.h"

//#include "TIter.h"
#include "TObjArray.h"
#include "TBrowser.h"
#include <iostream>

#include "TDataMember.h"
#include "TList.h"
#include "TString.h"
#include <assert.h>

#include "StMessMgr.h"

#undef min
#undef max

std::map< Int_t, TString > StarTGeoStacker::mClassMap;


// ------------------------------------------------------------------------------------------------------------
// -- 
// -- Compare checks whether the given volume is consistent with the supplied shape
// --
// ------------------------------------------------------------------------------------------------------------
const Double_t _epsil = 2.0E-6;
#define check(CLASS,x,y) {equal &= ( agshape->par(#x)==((TGeo##CLASS*)tgshape)->y()); }
#define angle(CLASS,x,y) {						\
    Double_t a = agshape->par(#x);					\
    Double_t b = ((TGeo##CLASS*)tgshape)->y();				\
    TVector3 pA,pB;							\
    pA.SetMagThetaPhi(1.0,45.0*TMath::DegToRad(),a*TMath::DegToRad());	\
    pB.SetMagThetaPhi(1.0,45.0*TMath::DegToRad(),b*TMath::DegToRad());	\
    a=pA.Phi(); b=pB.Phi();						\
    equal &= (TMath::Abs(a-b)<_epsil);					\
  }


Bool_t isAssembly( TGeoVolume *volume )
{
  return volume->IsA() == TClass::GetClass("TGeoVolumeAssembly");
}

//
// Parses the title string for the specified tag and returns
// the floating point value stored in between <tag> ... </tag>
//
Bool_t GetFloatValue( TString tag, TString title, Double_t &value )
{
  value = 0;
  if ( title.Contains(tag) )
    {
      TString start="<" +tag+">";
      TString end  ="</"+tag+">";
      Int_t   lstart = start.Length();
      Int_t   istart = title.Index(start)+lstart;
      Int_t   iend   = title.Index(end);
      //      std::cout << istart << " " << iend << std::endl;
      TString myvalue = title(istart,iend-istart);
      value = myvalue.Atof();
      return true;
    }
  return false;

}

Bool_t Compare( TGeoVolume *volume, AgMedium *agmedium )
{

  // No medium params to check?  Return true.
  if ( isAssembly(volume) ) return true;

  TGeoMedium *medium = volume->GetMedium();
  const Char_t *keys[]=
    {
      "isvol",  // GetParam(0)
      "ifield", // GetParam(1)
      "fieldm", // GetParam(2)
      "tmaxfd", // GetParam(3)
      "stemax", // GetParam(4)
      "deemax", // GetParam(5)
      "epsil",  // GetParam(6)
      "stmin"   // GetParam(7)
    };
  for ( UInt_t i=0;i<sizeof(keys)/sizeof(Char_t*);i++ )
    {
      if ( medium->GetParam(i) != agmedium->par(keys[i]) )
        {	 
          return false;
        }
    }
  return true;
}


Bool_t Compare( TGeoVolume *volume, AgShape *agshape )
{

  // No shape parameters to check?  return true.
  if ( isAssembly(volume) ) return true;

  //
  // Return early if agshape is a division (protects against badness
  // when the volume is really the division of the volume and the 
  // shape will be not a shape...)
  //
  if ( agshape -> type() == AgShape::kDivision )
    {
      return false;
    }



  TGeoShape *tgshape  = volume->GetShape();
  TClass    *tgclass  = tgshape->IsA();

  // Check that the classes are the same
  if ( StarTGeoStacker::mClassMap[ agshape->type() ] != tgclass->GetName() ) return false;
  Bool_t equal = true;

  switch ( agshape->type() )
    {
    case( AgShape::kBbox ):
      check(BBox,dx,GetDX); check(BBox,dy,GetDY); check(BBox,dz,GetDZ);
      break;

    case (AgShape::kTrd1):
      check(Trd1,dx1,GetDx1); check(Trd1,dx2,GetDx2); check(Trd1,dy,GetDy);  check(Trd1,dz,GetDz);
      break;

    case (AgShape::kTrd2):
      check(Trd2,dx1,GetDx1); check(Trd2,dx2,GetDx2); check(Trd2,dy1,GetDy1); check(Trd2,dy2,GetDy2); check(Trd2,dz,GetDz);
      break;

    case (AgShape::kTrap):
      check(Trap,dz,GetDz);  check(Trap,thet,GetTheta); check(Trap,phi,GetPhi); 
      check(Trap,h1,GetH1);  check(Trap,bl1,GetBl1);  check(Trap,tl1,GetTl1); 
      check(Trap,alp1,GetAlpha1); check(Trap,h2,GetH2);  check(Trap,bl2,GetBl2);  check(Trap,tl2,GetTl2); check(Trap,alp2,GetAlpha2);
      break;

    case (AgShape::kCtub): // Tube family
      equal&= ( agshape->par("lx")==((TGeoCtub*)tgshape)->GetNlow()[0] );
      equal&= ( agshape->par("ly")==((TGeoCtub*)tgshape)->GetNlow()[1] );
      equal&= ( agshape->par("lz")==((TGeoCtub*)tgshape)->GetNlow()[2] );
      equal&= ( agshape->par("hx")==((TGeoCtub*)tgshape)->GetNhigh()[0] );
      equal&= ( agshape->par("hy")==((TGeoCtub*)tgshape)->GetNhigh()[1] );
      equal&= ( agshape->par("hz")==((TGeoCtub*)tgshape)->GetNhigh()[2] );
    case (AgShape::kTubs):
      angle(TubeSeg,phi1,GetPhi1); angle(TubeSeg, phi2, GetPhi2);
    case (AgShape::kTube):
      check(Tube,rmin,GetRmin); check(Tube,rmax,GetRmax); check(Tube,dz,GetDz);
      break;

    case (AgShape::kHype):
      check(Tube,rmin,GetRmin); check(Tube,rmax,GetRmax); check(Tube,dz,GetDz);
      check(Hype,stin,GetStIn); check(Hype,stout,GetStOut);
      break;

    case (AgShape::kCons): // Cone family
      angle(ConeSeg,phi1,GetPhi1); angle(ConeSeg,phi2,GetPhi2);
    case (AgShape::kCone):
      check(Cone,dz,GetDz); check(Cone,rmn1,GetRmin1); check(Cone,rmx1,GetRmax1); check(Cone,rmn2,GetRmin2); check(Cone,rmx2,GetRmax2);
      break;

    case (AgShape::kSphe):
      check(Sphere,rmin,GetRmin);   check(Sphere,rmax,GetRmax);
      angle(Sphere,the1,GetTheta1); angle(Sphere,the2,GetTheta2);
      angle(Sphere,phi1,GetPhi1);   angle(Sphere,phi2,GetPhi2);
      break;

    case (AgShape::kPara):
      check(Para,dx,GetX); check(Para,dy,GetY); check(Para,dz,GetZ);
      angle(Para,alph,GetAlpha); angle(Para,thet,GetTheta); angle(Para,phi,GetPhi);
      break;

    case (AgShape::kPgon): // Polycone family
      check(Pgon,npdiv,GetNedges)
      case (AgShape::kPcon):
      angle(Pcon,phi1,GetPhi1); check(Pcon,dphi,GetDphi); check(Pcon,nz,GetNz);
      for ( UInt_t i=0;i<(UInt_t)agshape->par("nz");i++ )
	{
	  equal &= ( agshape->Z(i)    == ((TGeoPcon*)tgshape)->Z(i));
	  equal &= ( agshape->Rmin(i) == ((TGeoPcon*)tgshape)->Rmin(i));
	  equal &= ( agshape->Rmax(i) == ((TGeoPcon*)tgshape)->Rmax(i));
	}
      break;

    case (AgShape::kEltu):
      check(Eltu,p1,GetA); check(Eltu,p2,GetB); check(Eltu,dz,GetDz);
      break;

    case (AgShape::kGtra):
      check(Gtra,dz,GetDz); angle(Gtra,thet,GetTheta); angle(Gtra,phi,GetPhi); angle(Gtra,twis,GetTwistAngle);
      check(Gtra,h1,GetH1); check(Gtra,bl1,GetBl1); check(Gtra, tl1,GetTl1); angle(Gtra,alp1,GetAlpha1);
      check(Gtra,h2,GetH2); check(Gtra,bl2,GetBl2); check(Gtra, tl2,GetTl2); angle(Gtra,alp2,GetAlpha2);
      break;

    case (AgShape::kTorus):
      check(Torus,r,GetR);
      check(Torus,rmin,GetRmin);
      check(Torus,rmax,GetRmax);
      angle(Torus,phi1,GetPhi1);
      check(Torus,dphi,GetDphi);
      break;

    case (AgShape::kDivision):
      equal=false; // Always return false for divisions because TGeo doesn't let us check


      break;

    }

  return equal;
}
#undef check
#undef angle


//
////////////////////////////////////////////////////////////////////////////////////////////////
//
// Helper function to build a TGeoMaterial or Mixture from an AgMaterial
// =====================================================================
//
// A new TGeoMaterial is created based on the parameters provided by the user.  For materials,
// the A, Z and density must be defined.  Optionally, the radiation length and hadronic 
// absorption length may be specified.  If left out, ROOT/TGeo computes the radiation length.
//
// A new TGeoMixture is created for mixtures, based on the components provided by the user.
// Only the components and density provided are used.  If provided, the radiation and/or
// absorption lengths are ignored.  (A warning is issued in this case).
//
////////////////////////////////////////////////////////////////////////////////////////////////
//
TGeoMaterial *BuildMaterial( AgMaterial &ag_material )
{

  TGeoMaterial *material = 0;
  TGeoMixture  *mixture  = 0;

  TString mat_name = ag_material.GetName();
  TString name;

  //  std::cout << "BuildMaterial: " << mat_name.Data() << std::endl;

  Double_t aa   = ag_material.par("a");      
  Double_t zz   = ag_material.par("z");
  Double_t dd   = ag_material.par("dens");
  Double_t radl = (ag_material.isSet("radl")) ? ag_material.par("radl") : 0;
  Double_t absl = (ag_material.isSet("absl")) ? ag_material.par("absl") : 0;
  Double_t ww   = 0.;

  Int_t    ic = 0;
  Int_t    nc = ag_material.numberOfComponents();

  switch ( ag_material.type() )
    {
    case( AgMaterial::kMixture ):
      if ( ag_material.sumWeights() - 1.0 < -1.0E-5 ) 
	{	  
	  LOG_WARN << "Warning: sum of all weights does not add up to 1.0" << endm;
	  ag_material.Print();
	}
      mixture = new TGeoMixture( mat_name, nc, dd );
      for ( ic = 0; ic<nc; ic++ )
	{
	  ag_material.Component(ic,name,aa,zz,ww); mixture->DefineElement(ic,aa,zz,ww);
	}
      if ( ag_material.isSet("radl")) ag_material.Warning(ag_material.GetName(),"The RADL attribute was set, but has no effect in AgSTAR/AgML");
      return (TGeoMaterial*)mixture;
      break;
    case( AgMaterial::kCompound ):
      mixture = new TGeoMixture( mat_name, nc, dd );
      for ( ic = 0; ic<nc; ic++ )
	{
	  ag_material.Component(ic,name,aa,zz,ww); mixture->DefineElement(ic,zz,(Int_t)ww);
	}
      if ( ag_material.isSet("radl")) ag_material.Warning(ag_material.GetName(),"The RADL attribute was set, but has no effect in AgSTAR/AgML");
      return (TGeoMaterial*)mixture;
      break;
    case( AgMaterial::kMaterial ):

      //
      // Create a new TGeoMaterial from a, z, density
      //
      if ( ag_material.isSet("radl"))	  material = new TGeoMaterial( mat_name, aa, zz, dd, radl, absl);	
      else                                material = new TGeoMaterial( mat_name, aa, zz, dd);
      return material;

      break;
    default:
      assert(2+2==5); // Because we should never ever ever get here!
    }

  // And we should certainly never get to this point, but we've
  // already died, so meh
  assert(2+2==5);
  return NULL;

}
////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////



Bool_t IsSameMedium( TGeoMedium *t, AgMedium *a )
{
  if ( t == NULL )
    {
      return false;
    }

  const Char_t *keys[]={"isvol","ifield","fieldm","tmaxfd","stemax","deemax","epsil","stmin"};  
  for ( UInt_t i=0;i<sizeof(keys)/sizeof(Char_t*);i++ )
    {
      if ( a->isSet( keys[i] ) )
	{
	  if ( t->GetParam(i) != a->par( keys[i] ) ) return false;
	}
    }

  return true;
}

TGeoMedium *GetMedium( TString name, AgMedium *medium )
{
  TIter next( gGeoManager -> GetListOfMedia() );
  TGeoMedium *medi = 0;
  while ( (medi = (TGeoMedium *)next() ) )
    {
      if ( name != medi->GetName() )         // Find the medium w/ the fully qualified medium name
	{
	  continue;
	}
      if ( IsSameMedium( medi, medium ) )    // Check parametes and return the medium if it is the same
	{
	  return medi;
	}
    }
  return NULL;
}


// ------------------------------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------- BEGIN --
// ------------------------------------------------------------------------------------------------------------
StarTGeoStacker::StarTGeoStacker( const Char_t *name, const Char_t *title )
  : StarAgmlStacker(name,title)
{

  // Ensure that we have a geometry manager
  if ( !gGeoManager ) {
    TGeoManager *manager = new TGeoManager("dyson","The STAR Geometry Manager"); assert(manager);
  }


  // Export basic materials to ROOT
  const Char_t *ag[]={"Hydrogen","Deuterium","Helium","Lithium","Berillium","Carbon","Nitrogen","Neon","Aluminium","Iron","Copper","Tungsten","Lead","Uranium","Air","Vacuum","Silicon"};
  for ( UInt_t i=0;i<sizeof(ag)/sizeof(Char_t *);i++ )
    {
      AgMaterial &m = AgMaterial::Get(ag[i]);
      TString name = ag[i];
      name.ToUpper();
      TGeoMaterial *temp = new TGeoMaterial(name, m.par("a"),m.par("z"),m.par("dens"),m.par("radl"),m.par("absl"));

      // If ROOT didn't register it... register it...
      if ( !gGeoManager->GetMaterial(name))
	{
	  gGeoManager->AddMaterial( temp );
	}
      gGeoManager->GetMaterial(name)->SetTitle("AgML Default Material");
      
    }
  

}




// ------------------------------------------------------------------------------------------------------------
Bool_t StarTGeoStacker::Build( AgBlock *block )
{ 

  assert(block); // Cannot be passed a NULL block, duh
#ifdef __DEBUG_BUILD__
  __DEBUG_BUILD__ std::cout << "-----------------------------------------------------------------------------" << std::endl;
  __DEBUG_BUILD__ Info("Build(...)","Building block %s",block->GetName());
#endif

  TString block_name = block->GetName();

  static Int_t numed = 1; // Why does ROOT make us keep track of this?
  static Int_t numat = 1;

  // Get the module
  AgModule   *module   = block -> module();

  // Get the material, medium and shape from the block
  mMaterial = *block -> material();
  mMedium   = *block -> medium();
  mShape    = *block -> shape();
  mAttribute= *block -> attribute();

  // Set the mother block
  block -> SetMother( AgBlock::previous() );

  // Handle inheritance of attributes
  mAttribute.Inherit( AgBlock::previous() );

  
  //
  /////////////////////////////////////////////////////////////////////////
  //
  // Perform the creation of the material, medium, shape, etc...
  //
  /////////////////////////////////////////////////////////////////////////
  //

  TString       mod_name     = module->GetName();
  TString       blk_name     = block->GetName();

  TString       mat_name     = mMaterial.GetName();

  //
  // If no material was specified, copy the material definition from the previous
  // block into this block.
  //
  if ( mat_name == "None" )
    {
      AgMaterial *mine = block -> material();
      AgMaterial *moms = AgBlock::previous()->material();

      mine -> Inherit( AgBlock::previous() );
      mine -> SetName(moms->GetName());
      
      mat_name = AgBlock::previous()->material()->GetName();
      
    }

  //
  // Lookup the material.  If it is not found, create a new one.
  //
  TGeoMaterial *material      = gGeoManager->GetMaterial( mat_name );                  
  if ( !material )                                                                     
    {
      material = BuildMaterial( mMaterial );
      material -> SetName(mat_name);
      material -> SetTitle(Form("Material %s last touched in block %s of module %s",mat_name.Data(),block->GetName(),module->GetName()));
      material -> SetUniqueID( numat++ );
    }


#ifdef __DEBUG_BUILD__
  __DEBUG_BUILD__ material -> Print();
#endif

  // Medium name
  TString       med_name     = mMedium.GetName();  

  //This naming convention results in a unique medium per volume
  //TString       fqmed_name   = mod_name + "_" + blk_name + " " + med_name;

  // This naming convention results in a unique medium per material
  TString fqmed_name = mat_name; 
  if ( med_name != "None" ) 
    {
      fqmed_name += "::"; 
      fqmed_name+=med_name;
    }
  else
    {
      TString parent = AgBlock::previous()->medium()->GetName();
      fqmed_name += "::";
      fqmed_name+=parent;
    }

  //
  // It is possible that medium parameters were set in the material.
  // IF THEY ARE, then OVERWRITE... note that this hack probably
  // breaks inheritance.
  //
  const Char_t *keys[]={"isvol","ifield","fieldm","tmaxfd","stemax","deemax","epsil","stmin"};
  for ( int i=0;i<8;i++ )
    if ( mMaterial.isSet(keys[i])) 
      {
	mMedium.par(keys[i])=mMaterial.par(keys[i]);     
      }

  //
  // Get the medium.  If it does not exist, create it.
  //
  TGeoMedium   *medium = ::GetMedium( fqmed_name, &mMedium );
  if ( !medium )                                             
    {

      medium = new TGeoMedium(  fqmed_name,
				numed++,
				material->GetUniqueID(),
				mMedium.par("isvol"),
				mMedium.par("ifield"),
				mMedium.par("fieldm"),
				mMedium.par("tmaxfd"),
				mMedium.par("stemax"),
				mMedium.par("deemax"),
				mMedium.par("epsil"),
				mMedium.par("stmin") );

      medium -> SetMaterial(material); // because I don't want to keep track of the material ID
      medium  -> SetTitle(Form("Medium %s last touched in block %s of module %s",med_name.Data(),block->GetName(),module->GetName()));

    }
  

#ifdef __DEBUG_BUILD__
  __DEBUG_BUILD__ medium -> Print();
#endif

  //
  // Create the shape.  Future versions of the code should move the shape creation
  // to this location.
  //

  if ( mShape.type() == AgShape::kUnknown )
    { gErrorIgnoreLevel=1; 
      block->Error( "Build", Form("Invalid shape for block %s.",block->GetName()) );
      assert(mShape.type() != AgShape::kUnknown );
    }


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Create the volume //


  TGeoShape  *shape  = 0;
  TGeoVolume *volume = 0;

  //
  // TODO: Analyze the code and verify that we _do not_ look at the create operator for shape parameters.
  //
  // Loop over the shape's parameters.  If a parameter is not set, look for
  // a parameter set in the Create operator.  If no such parameter was set,
  // we do not know what to do and we need to throw an assert.
  //
  std::vector<TString> &parList = mShape.parList();
  for ( UInt_t i=0;i<parList.size();i++ )
    {
      TString key = parList[i];
      if ( !mShape.isSet(key) )
	{ gErrorIgnoreLevel=1; 
	  Error("Build(AgBlock *block)",Form("Block %s is missing shape parameter '%s'",block->GetName(),key.Data()));
	  mShape.Print();
	  assert( mShape.isSet( parList[i] ) );
	}
    }

  //
  /////////////////////////////////////////////////////////////////////////////////
  //
  // Creation of a volume based on a shape
  //
  /////////////////////////////////////////////////////////////////////////////////
  //
  TString volume_title;
  if ( mShape.type() != AgShape::kDivision )
    {
      shape = mShape.Make();
      shape->SetTitle(Form("A %s created in block %s of module %s",shape->GetName(),block->GetName(),module->GetName()));

      // Create the volume applying the nicknaming convention
      TString nn = StarAgmlStacker::nickname( block->GetName() );	  
      if ( block->GetAssembly() ) 
	{
	  volume = new TGeoVolumeAssembly( nn );
	}
      else 
	{
	  volume = new TGeoVolume( nn, shape, medium );
	}

      volume_title += nn; 
      volume_title += " ";
      volume_title += shape->GetName();
      volume_title += " ";

      mVolumeTable[nn]=volume;    // Add the nicknamed volume to the volume table
      block -> addNickname( nn ); // Add the nickname to the block


      if ( mDebugOptions[block_name].Contains("shape") )
	{
	  std::cout << "== Debug Shape: block="<<block_name.Data() << " ==================================" << std::endl;
	  volume->InspectShape();
	}


    }




  //
  // Division of a mother volume
  //
  if ( mShape.type() == AgShape::kDivision )
    {

      //
      // Get the division parameters
      //
      Int_t iaxis    = mShape.par("iaxis");
      Int_t ndiv     = mShape.par("ndiv");
      Double_t start = 0.0; // mShape.par("start");
      if ( mShape.isSet("start") ) 
	{
	  start = mShape.par("start");
	}
      if ( mShape.isSet("c0") )
	{
	  start = mShape.par("c0"); // not sure why this comes through sometimes
	  mShape.par("start")=mShape.par("c0");
	}
       
      Double_t step  = mShape.par("step");
      Double_t numed = mShape.par("numed");

      // Division style
      TString option="";
      if ( mShape.par("step") <= 0 )	{	  option+="N";	}
      else                              {	  option+="S";	}
      if ( mShape.par("start") <= 0 )	{	  option+="";   }
      else                              {	  option+="x";  }

      TString mother_name = AgBlock::previous()->nickname();


      // If the volume has daughters we cannot divide...
      volume = gGeoManager -> FindVolumeFast( mother_name );
      if ( !volume->GetNdaughters() )
	{

	  // HERE WE NEED TO USE THE VOLUME NICKNAMING !!!CONVENTION!!!
#if 0
	  volume = gGeoManager -> Division( block->GetName(), mother_name, iaxis, ndiv, start, step, numed, option );
#else
	  TString nn = StarAgmlStacker::nickname( block->GetName() );
	  volume = gGeoManager -> Division( nn, mother_name, iaxis, ndiv, start, step, numed, option );
	  mVolumeTable[nn]=volume;
	  block -> addNickname(nn);
#endif

	  volume_title += nn; 
	  volume_title += " ";
	  volume_title += "Division of ";
	  volume_title += mother_name;
	  volume_title += " ";

	}

    }

  TString myattr = "Attributes for ";
  myattr += volume->GetName();
  myattr += ": ";

  //
  // Set GEANT3 Attributes
  // =====================
  //
  const Char_t *att[] = {"seen", "colo", "lsty", "lwid", "fill"};
  for ( Int_t i=0;i<5;i++ )
    {
      if ( !mAttribute.isSet( att[i] ) ) 
	{
	  continue;
	}
      Int_t val = mAttribute.par(att[i]);
      myattr += Form( "%s = %i ", att[i], val );
      gGeoManager->SetVolumeAttribute( volume->GetName(), att[i], val );
    }

  //
  // Handle serial attribute by placing a <serial>number</serial> into the
  // title of the volume.  The serial number will be checked at create/position
  // time, and a new volume generated if the serial number changes
  // ===========================================================================
  //
  if ( mAttribute.isSet("serial") )
    {
      volume_title += "<serial>";
      volume_title += Form("%8.3f ",mAttribute.par("serial"));
      volume_title += "</serial>";
    }

  //
  // Apply transparency
  //
  if ( mAttribute.isSet("trans") )
    {
      volume->SetTransparency( (UInt_t)mAttribute("trans") );
    }

  // Add the volume to the volume stack and map
  volume -> SetTitle( volume_title );

  mVolumeStack.push_back( volume );
  mVolumeTable[ block -> GetName() ] = volume;


//   ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Add groups //
//   //
//   // Groups allow the user to place multiple, comoving volumes into a single reference frame
//   //
//   const vector<TString> &groups = block->groups();

//   for ( UInt_t i=0; i<groups.size();i++ )
//     {
//       TString name = groups[i];
//       if ( !mVolumeTable[name] )
// 	{

// 	  Info(GetName(),Form("Adding reference group %s",name.Data()));

// 	  TGeoVolume *group = new TGeoVolumeAssembly(name);
// 	  mVolumeTable[ name ] = group;
// 	  volume->AddNode( group, 1, gGeoIdentity );
// 	}
//       else
// 	Warning(GetName(),Form("A volume group %s has already been defined",name.Data()));
//     }


  // Time to get out of here
  return false;

}

// ------------------------------------------------------------------------------------------------------------
TGeoVolume *makeCopyVolume( TGeoVolume *org, AgShape shape, Bool_t copyDaughters=false )
{
  //
  // Creates volume with the specified shape.  By default we will _not_
  // copy the nodes into the new volume.
  //
  TGeoVolume *vol = org->MakeCopyVolume( shape.Make() );

  //$$$  vol->SetTitle(Form("Parameterized volume with shape %s",shape.GetName()));
  vol -> SetTitle( org -> GetTitle() );

  if ( copyDaughters )
    {
      return vol;
    }
  vol->ClearNodes();

  return vol;
}


// ------------------------------------------------------------------------------------------------------------
// 
// Sanity check on TGeoVolume to ensure that the shape, medium and material look reasonable
//
Bool_t sanityCheck( TGeoVolume *volume )
{
  
  TGeoShape *shape = volume->GetShape();
  //YF  TString    typeof(shape->ClassName());
  
  // Shape with volume=0 is probably an error
  if ( shape->Capacity() <= 0. )
    { Int_t old=gErrorIgnoreLevel;    
      gErrorIgnoreLevel=1; // Some warnings will not be supressed
      AgBlock::module()->Warning(AgModule::module()->GetName(),Form("Volume %s shape parameters invalid.  NULL bounding box capacity.",volume->GetName()));
      //      volume -> Print();
      //      volume -> InspectShape();
      gErrorIgnoreLevel=old;
      return false;
    }


  return true;
}



// ------------------------------------------------------------------------------------------------------------
Bool_t StarTGeoStacker::Position( AgBlock *block, AgPosition position )
{
  assert(block);
  //
  // Get the block's name
  //
  TString block_name = block->GetName();

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Get the mother and daughter matching the current shape
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgBlock    *mother_block = AgBlock::Find( position.mother() );
  TString     mother_name  = mother_block -> nickname();
  TString     group_name   = position.group();
  TGeoVolume *mother       = mVolumeTable[ mother_name ];
  TGeoVolume *group        = mVolumeTable[ group_name ];
  TGeoVolume *daughter     = 0;

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Obtain the pointer to the shape of the block we are positioning.
  // If the shape is a division, issue a warning.  Divisions of volumes
  // do not need to be positioned.  In principle this error should throw
  // an exception in the XML.
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgShape *shape = block->shape();
  if ( shape->type() == AgShape::kDivision )
    {
      Warning("Position(...)",Form("Attempt to position %s which is a division of %s",block->GetName(),mother->GetName()));
      return true;
    }

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Obtain the pointer to the attribute of the block we are positioning.  
  // This is for ensuring the serial numbers match.
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgAttribute *attribute  = block->attribute();
  Double_t     att_serial = -999.0;
  if ( attribute -> isSet("serial") )
    {
      att_serial = attribute->par("serial");
    }


  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Search for the TGeo volume with the same name and shape as the given
  // block.  This volume should have been created in the ::Build(...)
  // phase.  If it wasn't created, we may be dealing with a parameterized
  // volume.  In that case we will need to create the volume.  Otherwise,
  // we will throw an error and bomb out of the program.
  //
  // Also ensure that serial number matches, if given.
  //
  ///////////////////////////////////////////////////////////////////////////////
  //
  TIter next( gGeoManager -> GetListOfVolumes() ),	nextG( gGeoManager->GetListOfGVolumes() );
  TGeoVolume *vol = 0;
  //Int_t count=0;

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // First we must resolve any parameters specified by the
  // position operator
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgShape pos_shape = *block->shape();
  AgShape sav_shape = *block->shape();
  Int_t   nshape = 0;
  for ( UInt_t i=0;i<pos_shape.parList().size();i++ )
    {
      TString key=pos_shape.parList()[i];
      if ( position.isSet(key) )
	{
	  pos_shape.par(key) = position.par(key);
	  nshape++;
	}
    }

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // If there were position arguements, perform first a sanity check and
  // then inherit any remaining parameters from the active block (i.e.
  // the one which is doing the positioning).
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  Bool_t parameterized = false;
  if ( nshape )
    {

      if ( !block->shape()->parameterized() ) // [ERROR]: Block placed with position arguements but is not a parameterized block
	{ gErrorIgnoreLevel=1; 
	  Error("Position( block, position )", Form("Block %s is not setup to accept shape arguements",block->GetName()));
	  std::cout << std::endl;
	  std::cout << "The block was defined with shape:"<< std::endl;
	  block->shape()->Print();
	  assert( block->shape()->parameterized() );
	}

      //
      // Flag this as a paramterized shape which will pick up its shape
      // parameters from the pos_shape object
      //
      parameterized = true; 


      //
      // Inheritance from the active block
      //
      std::vector<TString> pars = shape->parList();

      //
      // Inherit from the active block, unless it is a division.
      // In which case navigate back up the stack until we find
      // a non-division
      AgShape *ancestor = AgBlock::active()->shape();
      //
      if ( ancestor->type() == AgShape::kDivision )
	{
	  ancestor = AgBlock::previous()->shape();
	  UInt_t hist=0;
	  while (ancestor->type()==AgShape::kDivision)
	    {
	      ancestor=AgBlock::previous(hist++)->shape();
	      assert(ancestor);
	    }
	}

      for ( UInt_t i=0;i<pars.size();i++ )
	{
	  TString key=pars[i];
	  if (pos_shape.par(key) == 0.)
 	    {	 
 	      if ( ancestor -> isSet(key) )
 		{
 		  pos_shape.par(key) = ancestor->par(key);
 		}
 	    }
 	}
    }



      

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // We will (temporarily) set the shape to the position shape
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  if ( nshape )
    {
      block->SetShape( pos_shape );
    }

  std::vector< TGeoVolume * > sisters; // list of similar volumes for error detection/debug purposes below
  while ( (vol=(TGeoVolume*)next() ) )
    {

      TString volume_name = realname( vol->GetName() );

      // Name of the block must match the name of the volume
      if ( block_name != volume_name ) continue;
      // Add volumes with the same name to the list of sisters
      sisters.push_back(vol);

      //
      /////////////////////////////////////////////////////////////
      //
      // If the shape is a parameterized shape, we will copy/clone
      // the specified shape and break.
      //
      //    >>> I believe this fails on serialization... <<<
      //
      /////////////////////////////////////////////////////////////
      //

      // Require matching serial numbers
      Double_t vol_serial = -999.0;
      if ( GetFloatValue( "serial", vol->GetTitle(), vol_serial ) )
	{
	  if ( (att_serial != vol_serial) && att_serial > -999.0 ) continue;
	}

      if ( parameterized )
	{
	  daughter = makeCopyVolume( vol, pos_shape );
	  //	  if ( TPAD ) daughter->Print();
	  break;
	}

      // Look for volumes which match the block's shape
      if ( ! ::Compare( vol, block->shape() ) ) continue;



      daughter = vol;
    };

  if ( !daughter )

    while ( (vol=(TGeoVolume*)nextG() ) )
      {

	TString volume_name = realname( vol->GetName() );

	// Name of the block must match the name of the volume
	if ( block_name != volume_name ) continue;
	// Add volumes with the same name to the list of sisters
	sisters.push_back(vol);

	// Require matching serial numbers
	Double_t vol_serial = -999.0;
	if ( GetFloatValue( "serial", vol->GetTitle(), vol_serial ) )
	{
	  if ( (att_serial != vol_serial) && att_serial > -999.0 ) continue;
	}

	// If the shape is a parameterized shape, we will copy/clone
	// the specified shape
	if ( parameterized )
	  {
	    daughter = makeCopyVolume( vol, pos_shape );
	    break;
	  }

	// Look for volumes which match the block's shape
	if ( ! ::Compare( vol, block->shape() ) ) continue;



	daughter = vol;
      };


  if (!daughter) // ERROR: Daughter was not found, print debug information
    { gErrorIgnoreLevel=1; 
      Error("Position(AgBlock *block,AgPlacement position",
	    Form("This shouldn't happen... daughter %s was not found.  Was she built?",block_name.Data()));
      std::cout << std::endl;
      std::cout << "Some potentially useful debug information follows:" << std::endl;

      std::cout << std::endl;
      std::cout << "List of defined blocks" << std::endl;
      AgBlock::List();
      std::cout << std::endl;
      std::cout << "List of ROOT multi and runtime volumes:" << std::endl;
      gGeoManager->GetListOfGVolumes()->Print();
      std::cout << std::endl;
      std::cout << "List of ROOT volumes:" << std::endl;
      gGeoManager->GetListOfVolumes()->Print();
      std::cout << std::endl;
      std::cout << "We were loking for a volume with the shape: " << std::endl;
      shape->Print();
      std::cout << std::endl;
      std::cout << "We found " << sisters.size() << " volumes with the same name:" << std::endl;
      for ( UInt_t ii=0;ii<sisters.size(); ii++)
	{
	  sisters[ii]->InspectShape();
	}

      assert(daughter); // Because we've already created it in Build

    }

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Every positioned volume (node) gets a unique copy number.  We will count the
  // number of instances where the current block has been positioned w/in the
  // specified mother volume, and increment the copy counter.
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  Int_t copy = 1;
  for ( Int_t i=0;i<mother->GetNdaughters();i++ )
    {
      TGeoNode *node   = mother->GetNode(i);
      TString   name   = node->GetVolume()->GetName();
      TString   myname = realname(name);
      if ( myname.Contains( block->GetName() ) )
	{
	  copy++; // ok... this is simplified. 
        	  // But we take care of not placing same block elsewhere...
	}
    }

  // If the user has specified the copy number, use that copy number
  if ( position.isSet("ncopy") )   {      copy = (Int_t)position.par("ncopy");    }


	
  // Set the only flag.  Default volumes are "only" volumes.
  Int_t myonly = AgPlacement::kOnly;
  if ( position.isSet("only") )     {      myonly = (Int_t)position.par("only");    }

	
  // Get the translation / rotation matrix
  TGeoMatrix *matrix = position.matrix();

  // Regularize the matrix G3 angles
  



  TString NAME=position.block();

  // And name it
  matrix -> SetName( Form("pos_%s_in_%s_%i", daughter->GetName(), mother->GetName(), copy ) );

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Add the daughter block to the mother block
  //
  //////////////////////////////////////////////////////////////////////////////
  //


  if ( mDebugOptions[block_name].Contains("position")  ||
       mDebugOptions[block_name].Contains("placement") ||
       mDebugOptions[block_name].Contains("place") )
    {
      std::cout << "== Debug Placement: block="<<block_name.Data() << " ==================================" << std::endl;
      std::cout << "   konly   = " << ((myonly==AgPlacement::kOnly)?"ONLY":"MANY") << std::endl;
      std::cout << "   copy    = " << copy << std::endl;
      std::cout << "   runtime = " << ((parameterized)?"TRUE":"FALSE") << std::endl;
      std::cout << "   matrix  " << std::endl;
      matrix->Print();
    }


  // Add the volume to the mother volume or the group
  TGeoVolume *target = (group)?group:mother;

  // Check validity of volume group... must be placed within mother
  if ( group ) 
    {
      TString name = group->GetName(); name+="_1";
      TGeoNode *node = mother->FindNode( name );
      if ( !node )
	{	  
	  AgBlock::module()->Warning(AgModule::module()->GetName(), Form("Placing %s in group %s.  WARNING: group is in wrong mother volume.",block_name.Data(),group->GetName() ));
	}
    }
  
  if ( group && AgPlacement::kMany==myonly )   
    {
      AgBlock::module()->Warning(AgModule::module()->GetName(), Form("Volume %s in %s, effect of MANY is ignored.",block_name.Data(),target->GetName()));
    }



    


  if ( myonly == AgPlacement::kOnly )
    { 
      assert(daughter);
      assert(daughter->IsValid());
      if ( sanityCheck(daughter) )
	target -> AddNode( daughter, copy, matrix );
    }
  else
    { 
      assert(daughter);
      assert(daughter->IsValid());
      if ( sanityCheck(daughter) )
	target -> AddNodeOverlap( daughter, copy, matrix );
    }

//   //
//   // In the case of parameterized blocks, restore the previous state of the shape
//   //
//   if ( nshape )
//     {
//       block->SetShape( sav_shape );
//     }


  // If the shape is a parameterized shape, reset all of the shape
  // paramters to zero
  if ( parameterized )
    {

      std::vector<TString> pars = block->shape()->parList();
      for ( UInt_t i=0;i<pars.size();i++ )
	{
	  TString key = pars[i];
	  block->shape()->par(key) = 0.0;
	}

    }

  return true;
};

Bool_t StarTGeoStacker::Position( AgBlock *block, AgPlacement position )
{

  assert(block);
  //
  // Get the block's name
  //
  TString block_name = block->GetName();

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Get the mother and daughter matching the current shape
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgBlock    *mother_block = AgBlock::Find( position.mother() );
  TString     mother_name  = mother_block -> nickname();
  TString     group_name   = position.group();
  TGeoVolume *mother       = mVolumeTable[ mother_name ];
  TGeoVolume *group        = mVolumeTable[ group_name ];
  TGeoVolume *daughter     = 0;

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Obtain the pointer to the shape of the block we are positioning.
  // If the shape is a division, issue a warning.  Divisions of volumes
  // do not need to be positioned.  In principle this error should throw
  // an exception in the XML.
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgShape *shape = block->shape();
  if ( shape->type() == AgShape::kDivision )
    {
      Warning("Position(...)",Form("Attempt to position %s which is a division of %s",block->GetName(),mother->GetName()));
      return true;
    }

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Obtain the pointer to the attribute of the block we are positioning.  
  // This is for ensuring the serial numbers match.
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgAttribute *attribute  = block->attribute();
  Double_t     att_serial = -999.0;
  if ( attribute -> isSet("serial") )
    {
      att_serial = attribute->par("serial");
    }


  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Search for the TGeo volume with the same name and shape as the given
  // block.  This volume should have been created in the ::Build(...)
  // phase.  If it wasn't created, we may be dealing with a parameterized
  // volume.  In that case we will need to create the volume.  Otherwise,
  // we will throw an error and bomb out of the program.
  //
  // Also ensure that serial number matches, if given.
  //
  ///////////////////////////////////////////////////////////////////////////////
  //
  TIter next( gGeoManager -> GetListOfVolumes() ),	nextG( gGeoManager->GetListOfGVolumes() );
  TGeoVolume *vol = 0;
  //Int_t count=0;

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // First we must resolve any parameters specified by the
  // position operator
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  AgShape pos_shape = *block->shape();
  AgShape sav_shape = *block->shape();
  Int_t   nshape = 0;
  for ( UInt_t i=0;i<pos_shape.parList().size();i++ )
    {
      TString key=pos_shape.parList()[i];
      if ( position.isSet(key) )
	{
	  pos_shape.par(key) = position.par(key);
	  nshape++;
	}
    }

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // If there were position arguements, perform first a sanity check and
  // then inherit any remaining parameters from the active block (i.e.
  // the one which is doing the positioning).
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  Bool_t parameterized = false;
  if ( nshape )
    {

      if ( !block->shape()->parameterized() ) // [ERROR]: Block placed with position arguements but is not a parameterized block
	{ gErrorIgnoreLevel=1; 
	  Error("Position( block, position )", Form("Block %s is not setup to accept shape arguements",block->GetName()));
	  std::cout << std::endl;
	  std::cout << "The block was defined with shape:"<< std::endl;
	  block->shape()->Print();
	  assert( block->shape()->parameterized() );
	}

      //
      // Flag this as a paramterized shape which will pick up its shape
      // parameters from the pos_shape object
      //
      parameterized = true; 


      //
      // Inheritance from the active block
      //
      std::vector<TString> pars = shape->parList();

      //
      // Inherit from the active block, unless it is a division.
      // In which case navigate back up the stack until we find
      // a non-division
      AgShape *ancestor = AgBlock::active()->shape();
      //
      if ( ancestor->type() == AgShape::kDivision )
	{
	  ancestor = AgBlock::previous()->shape();
	  UInt_t hist=0;
	  while (ancestor->type()==AgShape::kDivision)
	    {
	      ancestor=AgBlock::previous(hist++)->shape();
	      assert(ancestor);
	    }
	}

      for ( UInt_t i=0;i<pars.size();i++ )
	{
	  TString key=pars[i];
	  if (pos_shape.par(key) == 0.)
 	    {	 
 	      if ( ancestor -> isSet(key) )
 		{
 		  pos_shape.par(key) = ancestor->par(key);
 		}
 	    }
 	}
    }



      

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // We will (temporarily) set the shape to the position shape
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  if ( nshape )
    {
      block->SetShape( pos_shape );
    }

  std::vector< TGeoVolume * > sisters; // list of similar volumes for error detection/debug purposes below
  while ( (vol=(TGeoVolume*)next() ) )
    {

      TString volume_name = realname( vol->GetName() );

      // Name of the block must match the name of the volume
      if ( block_name != volume_name ) continue;
      // Add volumes with the same name to the list of sisters
      sisters.push_back(vol);

      //
      /////////////////////////////////////////////////////////////
      //
      // If the shape is a parameterized shape, we will copy/clone
      // the specified shape and break.
      //
      //    >>> I believe this fails on serialization... <<<
      //
      /////////////////////////////////////////////////////////////
      //

      // Require matching serial numbers
      Double_t vol_serial = -999.0;
      if ( GetFloatValue( "serial", vol->GetTitle(), vol_serial ) )
	{
	  if ( (att_serial != vol_serial) && att_serial > -999.0 ) continue;
	}

      if ( parameterized )
	{
	  daughter = makeCopyVolume( vol, pos_shape );
	  //	  if ( TPAD ) daughter->Print();
	  break;
	}

      // Look for volumes which match the block's shape
      if ( ! ::Compare( vol, block->shape() ) ) continue;



      daughter = vol;
    };

  if ( !daughter )

    while ( (vol=(TGeoVolume*)nextG() ) )
      {

	TString volume_name = realname( vol->GetName() );

	// Name of the block must match the name of the volume
	if ( block_name != volume_name ) continue;
	// Add volumes with the same name to the list of sisters
	sisters.push_back(vol);

	// Require matching serial numbers
	Double_t vol_serial = -999.0;
	if ( GetFloatValue( "serial", vol->GetTitle(), vol_serial ) )
	{
	  if ( (att_serial != vol_serial) && att_serial > -999.0 ) continue;
	}

	// If the shape is a parameterized shape, we will copy/clone
	// the specified shape
	if ( parameterized )
	  {
	    daughter = makeCopyVolume( vol, pos_shape );
	    break;
	  }

	// Look for volumes which match the block's shape
	if ( ! ::Compare( vol, block->shape() ) ) continue;



	daughter = vol;
      };


  if (!daughter) // ERROR: Daughter was not found, print debug information
    { gErrorIgnoreLevel=1; 
      Error("Position(AgBlock *block,AgPlacement position",
	    Form("This shouldn't happen... daughter %s was not found.  Was she built?",block_name.Data()));
      std::cout << std::endl;
      std::cout << "Some potentially useful debug information follows:" << std::endl;

      std::cout << std::endl;
      std::cout << "List of defined blocks" << std::endl;
      AgBlock::List();
      std::cout << std::endl;
      std::cout << "List of ROOT multi and runtime volumes:" << std::endl;
      gGeoManager->GetListOfGVolumes()->Print();
      std::cout << std::endl;
      std::cout << "List of ROOT volumes:" << std::endl;
      gGeoManager->GetListOfVolumes()->Print();
      std::cout << std::endl;
      std::cout << "We were loking for a volume with the shape: " << std::endl;
      shape->Print();
      std::cout << std::endl;
      std::cout << "We found " << sisters.size() << " volumes with the same name:" << std::endl;
      for ( UInt_t ii=0;ii<sisters.size(); ii++)
	{
	  sisters[ii]->InspectShape();
	}

      assert(daughter); // Because we've already created it in Build

    }

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Every positioned volume (node) gets a unique copy number.  We will count the
  // number of instances where the current block has been positioned w/in the
  // specified mother volume, and increment the copy counter.
  //
  //////////////////////////////////////////////////////////////////////////////
  //
  Int_t copy = 1;
  for ( Int_t i=0;i<mother->GetNdaughters();i++ )
    {
      TGeoNode *node   = mother->GetNode(i);
      TString   name   = node->GetVolume()->GetName();
      TString   myname = realname(name);
      if ( myname.Contains( block->GetName() ) )
	{
	  copy++; // ok... this is simplified. 
        	  // But we take care of not placing same block elsewhere...
	}
    }

  // If the user has specified the copy number, use that copy number
  if ( position.isSet("ncopy") )   {      copy = (Int_t)position.par("ncopy");    }


	
  // Set the only flag.  Default volumes are "only" volumes.
  Int_t myonly = AgPlacement::kOnly;
  if ( position.isSet("only") )     {      myonly = (Int_t)position.par("only");    }

	
  // Get the translation / rotation matrix
  TGeoCombiTrans *matrix = position.matrix();
  TString NAME=position.block();

  // And name it
  matrix -> SetName( Form("pos_%s_in_%s_%i", daughter->GetName(), mother->GetName(), copy ) );

  //
  //////////////////////////////////////////////////////////////////////////////
  //
  // Add the daughter block to the mother block
  //
  //////////////////////////////////////////////////////////////////////////////
  //


  if ( mDebugOptions[block_name].Contains("position")  ||
       mDebugOptions[block_name].Contains("placement") ||
       mDebugOptions[block_name].Contains("place") )
    {
      std::cout << "== Debug Placement: block="<<block_name.Data() << " ==================================" << std::endl;
      std::cout << "   konly   = " << ((myonly==AgPlacement::kOnly)?"ONLY":"MANY") << std::endl;
      std::cout << "   copy    = " << copy << std::endl;
      std::cout << "   runtime = " << ((parameterized)?"TRUE":"FALSE") << std::endl;
      std::cout << "   matrix  " << std::endl;
      matrix->Print();
    }


  // Add the volume to the mother volume or the group
  TGeoVolume *target = (group)?group:mother;

  // Check validity of volume group... must be placed within mother
  if ( group ) 
    {
      TString name = group->GetName(); name+="_1";
      TGeoNode *node = mother->FindNode( name );
      if ( !node )
	{	  
	  AgBlock::module()->Warning(AgModule::module()->GetName(), Form("Placing %s in group %s.  WARNING: group is in wrong mother volume.",block_name.Data(),group->GetName() ));
	}
    }
  
  if ( group && AgPlacement::kMany==myonly )   
    {
      AgBlock::module()->Warning(AgModule::module()->GetName(), Form("Volume %s in %s, effect of MANY is ignored.",block_name.Data(),target->GetName()));
    }



    


  if ( myonly == AgPlacement::kOnly )
    { 
      assert(daughter);
      assert(daughter->IsValid());
      if ( sanityCheck(daughter) )
	target -> AddNode( daughter, copy, matrix );
    }
  else
    { 
      assert(daughter);
      assert(daughter->IsValid());
      if ( sanityCheck(daughter) )
	target -> AddNodeOverlap( daughter, copy, matrix );
    }

//   //
//   // In the case of parameterized blocks, restore the previous state of the shape
//   //
//   if ( nshape )
//     {
//       block->SetShape( sav_shape );
//     }


  // If the shape is a parameterized shape, reset all of the shape
  // paramters to zero
  if ( parameterized )
    {

      std::vector<TString> pars = block->shape()->parList();
      for ( UInt_t i=0;i<pars.size();i++ )
	{
	  TString key = pars[i];
	  block->shape()->par(key) = 0.0;
	}

    }


  return true;
}

//
// ---------------------------------------------------------------------------------------------------
//
Bool_t StarTGeoStacker::SearchVolume( const AgShape &shape, const AgAttribute &attr )
{

  // Make a copy of the arglist to avoid some const restrictions
  AgAttribute myattr  = attr;
  AgShape     myshape = shape;

  // Get a pointer to the currently active block and obtain the name of the block
  AgBlock *block = AgBlock::active();
  TString  block_name = block -> GetName(); // name of the block

  // At this point, the shape should be fully realized.  If this method
  // is called at create time, then we assume that the mother/daughter
  // inheritance and any Create operator parameters have been handled.

  // Loop over all volumes and all volumes in the garbage collection
  TIter next( gGeoManager -> GetListOfVolumes() ),	
        nextG( gGeoManager->GetListOfGVolumes() );
  TGeoVolume *vol = 0;
  //Int_t count=0;  

  // We are attempting to find the volume which matches the specified 
  // shape and medium
  TGeoVolume *daughter = 0;

  // If we fail to find it, we will keep a list of sister volumes to
  // printout so that we can debug the system
  std::vector< TGeoVolume * > sisters; 

  // Loop over all volumes
  while ( (vol=(TGeoVolume*)next() ) )
    {

      // Obtain the real name for the nickname of the given volume
      TString volume_name = realname( vol->GetName() );

      // If the realname doesn't match the name of the block, move on
      if ( block_name != volume_name ) continue;

      // We have a TGeoVolume which is one instance of the current block.
      // Add this to the list of sister volumes.
      sisters.push_back(vol);

      // Check to see if the volume matches the current shape and skip 
      // to he next volume if it does not
      Bool_t comp = ::Compare(vol,&myshape);
      if ( !comp ) continue;

      // Check to see if the volume matches the medium specified in the
      // current block, and skip to the next volume if it does not.
      comp        = ::Compare(vol,block->medium()); // Medium is same as requested     
#if __REQUIRE_MEDIUM__
      if ( ! comp ) continue;
#endif

      // Check the serial numbers specified in the attributes
      Double_t vol_serial = -999.;
      Double_t att_serial = -999.;
      if ( myattr.isSet("serial") ) 
	{
	  att_serial = myattr.par("serial");
	}
      if ( GetFloatValue("serial",vol->GetTitle(),vol_serial) )
	{	  
	  if ( att_serial != vol_serial ) continue;	  	  
	}
      daughter = vol;

    };
  
  if ( !daughter ) // loop over 'garbage' volumes
    
    while ( (vol=(TGeoVolume*)nextG() ) )
      {
	
	// Obtain the real name for the nickname of the given volume
	TString volume_name = realname( vol->GetName() );
	
	// If the realname doesn't match the name of the block, move on
	if ( block_name != volume_name ) continue;
	
	// We have a TGeoVolume which is one instance of the current block.
	// Add this to the list of sister volumes.
	sisters.push_back(vol);
	
	// Check to see if the volume matches the current shape and skip 
	// to he next volume if it does not
	Bool_t comp = ::Compare(vol,&myshape);
	if ( !comp ) continue;

	// Check to see if the volume matches the medium specified in the
	// current block, and skip to the next volume if it does not.
	comp        = ::Compare(vol,block->medium()); // Medium is same as requested     	
#if __REQUIRE_MEDIUM__
	if ( ! comp ) continue;
#endif
	
	// Check the serial numbers specified in the attributes
	Double_t vol_serial = -999.;
	Double_t att_serial = -999.;
	if ( myattr.isSet("serial") ) 
	  {
	    att_serial = myattr.par("serial");
	  }
	if ( GetFloatValue("serial",vol->GetTitle(),vol_serial) )
	  {	  
	    if ( att_serial != vol_serial ) continue;	  	  
	  }
	daughter = vol;
	Warning(GetName(),"Using a garbage volume... may be related to boom at end");
	
      };


  if ( daughter ) 
    {
      return true; // A matching volume exists
    }

  return false; // No matching volume exists

}
//////////////////////////////////////////////////////////////////////////////////////////////////////
void StarTGeoStacker::AddGroup( const Char_t *name )
{
  
  Info(GetName(),Form("Adding reference group %s",name));

  // Get the active block
  AgBlock *block = AgBlock::active();

  // Retrieve corresponding volume
  TGeoVolume *volume = gGeoManager -> FindVolumeFast( block->nickname() );
  assert(volume);


  TGeoVolume *group = new TGeoVolumeAssembly(name);
  mVolumeTable[ name ] = group;
  volume->AddNode( group, 1, gGeoIdentity );

  return;
  
}


//////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////
struct _StarTGeoStackerDummy_
{
  _StarTGeoStackerDummy_(){

    //$$$    TGeoManager *manager = new TGeoManager("dyson","The STAR Geometry Manager");
    //$$$    manager -> BuildDefaultMaterials();
    //    TBrowser *browser = new TBrowser();
    //    browser->Add( manager->GetListOfShapes(), "Shapes" );

    StarTGeoStacker::mClassMap[ AgShape::kBbox ] = "TGeoBBox";
    StarTGeoStacker::mClassMap[ AgShape::kTrd1 ] = "TGeoTrd1";
    StarTGeoStacker::mClassMap[ AgShape::kTrd2 ] = "TGeoTrd2";
    StarTGeoStacker::mClassMap[ AgShape::kTrap ] = "TGeoTrap";
    StarTGeoStacker::mClassMap[ AgShape::kTube ] = "TGeoTube";
    StarTGeoStacker::mClassMap[ AgShape::kTubs ] = "TGeoTubeSeg";
    StarTGeoStacker::mClassMap[ AgShape::kCone ] = "TGeoCone";
    StarTGeoStacker::mClassMap[ AgShape::kCons ] = "TGeoConeSeg";
    StarTGeoStacker::mClassMap[ AgShape::kSphe ] = "TGeoSphere";
    StarTGeoStacker::mClassMap[ AgShape::kPara ] = "TGeoPara";
    StarTGeoStacker::mClassMap[ AgShape::kPgon ] = "TGeoPgon";
    StarTGeoStacker::mClassMap[ AgShape::kPcon ] = "TGeoPcon";
    StarTGeoStacker::mClassMap[ AgShape::kEltu ] = "TGeoEltu";
    StarTGeoStacker::mClassMap[ AgShape::kHype ] = "TGeoHype";
    StarTGeoStacker::mClassMap[ AgShape::kGtra ] = "TGeoGtra";
    StarTGeoStacker::mClassMap[ AgShape::kCtub ] = "TGeoCtub";


  };
} _star_tgeo_stacker_dummy;
