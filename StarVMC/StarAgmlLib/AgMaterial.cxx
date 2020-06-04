#include "AgMaterial.h"
ClassImp(AgMaterial);

#include <iostream>
#include <assert.h>
#include "AgBlock.h"
#include "AgModule.h"
#include "StMessMgr.h"

//std::vector< TString >            AgMaterial::mParameterList;
std::map< TString, AgMaterial * > AgMaterial::mMaterialTable;
std::vector< TString >            AgMaterial::mMaterialList;

struct _MaterialDummy { // Initializes material database
  _MaterialDummy(){
    /* lifted
    AgMaterial::mParameterList.push_back("a");
    AgMaterial::mParameterList.push_back("z");
    AgMaterial::mParameterList.push_back("dens");
    AgMaterial::mParameterList.push_back("radl");
    AgMaterial::mParameterList.push_back("absl");
    AgMaterial::mParameterList.push_back("isvol");
    AgMaterial::mParameterList.push_back("nelem");
    */

    AgMaterial &h = AgMaterial::Get("Hydrogen");
    h.par("a")=1.010; h.par("z")=1.000; h.par("dens")=0.071; h.par("radl")=0.865E+03; h.par("absl")=0.790E+03; h.lock();

    AgMaterial &d = AgMaterial::Get("Deuterium");
    d.par("a")=2.010;d.par("z")=1.000;d.par("dens")=0.162;d.par("radl")=0.757E+03;d.par("absl")=0.342E+03;d.lock();
    
    AgMaterial &he = AgMaterial::Get("Helium");
    he.par("a")=4.000;he.par("z")=2.000;he.par("dens")=0.125;he.par("radl")=0.755E+03;he.par("absl")=0.478E+03;he.lock();

    AgMaterial &li = AgMaterial::Get("Lithium");
    li.par("a")=6.940;li.par("z")=3.000;li.par("dens")=0.534;li.par("radl")=0.155E+03;li.par("absl")=0.121E+03;li.lock();

    AgMaterial &be = AgMaterial::Get("Berillium");
    be.par("a")=9.010;be.par("z")=4.000;be.par("dens")=1.848;be.par("radl")=0.353E+02;be.par("absl")=0.367E+02;be.lock();

    AgMaterial &c = AgMaterial::Get("Carbon");
    c.par("a")=12.010;c.par("z")=6.000;c.par("dens")=2.265;c.par("radl")=0.188E+02;c.par("absl")=0.499E+02;c.lock();

    AgMaterial &n = AgMaterial::Get("Nitrogen");
    n.par("a")=14.010;n.par("z")=7.000;n.par("dens")=0.808;n.par("radl")=0.445E+02;n.par("absl")=0.994E+02;n.lock();

    AgMaterial &ne = AgMaterial::Get("Neon");
    ne.par("a")=20.180;ne.par("z")=10.000;ne.par("dens")=1.207;ne.par("radl")=0.240E+02;ne.par("absl")=0.749E+02;ne.lock();

    AgMaterial &al = AgMaterial::Get("Aluminium");
    al.par("a")=26.980;al.par("z")=13.000;al.par("dens")=2.700;al.par("radl")=0.890E+01;al.par("absl")=0.372E+02;al.lock();

    AgMaterial &fe = AgMaterial::Get("Iron");
    fe.par("a")=55.850;fe.par("z")=26.000;fe.par("dens")=7.870;fe.par("radl")=0.176E+01;fe.par("absl")=0.171E+02;fe.lock();

    AgMaterial &cu = AgMaterial::Get("Copper");
    cu.par("a")=63.540;cu.par("z")=29.000;cu.par("dens")=8.960;cu.par("radl")=0.143E+01;cu.par("absl")=0.148E+02;cu.lock();

    AgMaterial &w = AgMaterial::Get("Tungsten");
    w.par("a")=183.850;
    w.par("z")=74.000;
    w.par("dens")=19.300;
    w.par("radl")=0.350E+00;
    w.par("absl")= 0.103E+02;
    w.lock();

    AgMaterial &pb = AgMaterial::Get("Lead");
    pb.par("a")=207.190;
    pb.par("z")=82.000;
    pb.par("dens")=11.350;
    pb.par("radl")=0.560E+00;
    pb.par("absl")=0.185E+02;
    pb.lock();

    AgMaterial &u = AgMaterial::Get("Uranium");
    u.par("a")=238.030;
    u.par("z")=92.000;
    u.par("dens")=18.950;
    u.par("radl")=0.320E+00;
    u.par("absl")=0.120E+02;
    u.lock();

    AgMaterial &air = AgMaterial::Get("Air");
    air.par("a")=14.610;
    air.par("z")=7.300;
    air.par("dens")=0.001205;
    air.par("radl")=0.304E+05;
    air.par("absl")=0.675E+05;
    air.lock();

    AgMaterial &vac = AgMaterial::Get("Vacuum");
    vac.par("a")=14.610;
    vac.par("z")=7.300;
    vac.par("dens")=0.001/1E3;
    vac.par("radl")=0.304E+05*1E3;
    vac.par("absl")=0.675E+05*1E3;
    vac.lock();


    AgMaterial &si = AgMaterial::Get("Silicon");
    si.par("a")=28.090;
    si.par("z")=14.000;
    si.par("dens")=2.330;
    si.par("radl")=0.936E+01;
    si.par("absl")=0.455E+02;    
    si.lock();

//        18 LIQUID_ARGON            39.950    18.000     1.400 0.140E+02 0.837E+02   1

    AgMaterial &ar = AgMaterial::Get("Argon_gas");
    ar.par("a")=39.950;
    ar.par("z")=18.000;
    ar.par("dens")=0.002;
    ar.par("radl")=0.118E+05;
    ar.par("absl")=0.707E+05;
    ar.lock();

    AgMaterial &ngas = AgMaterial::Get("Nitrogen_gas");
    ngas.par("a")=14.010;
    ngas.par("z")=7.000;
    ngas.par("dens")=0.001;
    ngas.par("radl")=0.326E+05;
    ngas.par("absl")=0.754E+05;
    ngas.lock();

    AgMaterial &o = AgMaterial::Get("Oxygen_gas");
    o.par("a")=16.000;
    o.par("z")=8.000;
    o.par("dens")=0.001;
    o.par("radl")=0.239E+05;
    o.par("absl")=0.675E+05;
    o.lock();

    AgMaterial &ch = AgMaterial::Get("Polystyren");
    ch.par("dens")=1.032;
    ch.Component("C",12.0,6.0,0.0+0.923);
    ch.Component("H", 1.0,1.0,1.0-0.923);
    ch.lock();

    AgMaterial &ch2 = AgMaterial::Get("Polyethylene");
    ch2.par("dens")=0.930;
    ch2.Component("C",12.0,6.0,0.0+0.857);
    ch2.Component("H", 1.0,1.0,1.0-0.857);
    ch2.lock();

    AgMaterial &mylar = AgMaterial::Get("Mylar");
    mylar.par("dens")=1.390;
    mylar.Component("C",12.0,6.0,0.625);
    mylar.Component("H", 1.0,1.0,0.042);
    mylar.Component("O",16.0,8.0,0.333);
    mylar.lock();

    //$$$    AgMaterial::List();

  };

} _material_dummy;


// ==============================================================================================================
AgMaterial::AgMaterial( const Char_t *name ) : TNamed(name,""), mLock(false), mType(AgMaterial::kMaterial)
{
  par("nelem")=1;
  const char* pars[]={"a","z","dens","radl","absl","isvol","nelem"};
  for ( int i=0;i<7;i++ ) mParameterList.push_back(pars[i]);
}

AgMaterial::~AgMaterial()
{
  /* nada */ 
}
// ==============================================================================================================
Double_t &AgMaterial::par( const Char_t *name ){
  static Double_t dummy;
  if ( mLock && isSet(name) )
    {
      dummy=mParameters[name];
      return dummy; // DO NOT allow user to write locked parameters.
                    // Silent because we will be reading them this way
                    // as well.
    }
  else if ( mLock )
    {
      dummy=0;
      return dummy; // DO NOT allow an unset parameter to be set, return zero
    }
  return mParameters[name]; 
}

#if 0
Bool_t AgMaterial::isSet( const Char_t *par ) const
{
  TString key=par;
  return ( mParameters.find(key) != mParameters.end() );
}

Bool_t AgMaterial::hasPar(const Char_t *par ) const
{
  const char* key=par;
  std::vector<std::string> parlist = mParameterList;
  for ( UInt_t i=0;i<parlist.size();i++ )
    if ( parlist[i] == key ) return true;
  return false;
}
#endif

Bool_t AgMaterial::hasComponent( const Char_t *comp )const
{
	if ( mType == AgMaterial::kMaterial )
		return false;
	TString key = comp;
	return mComponentTable.find(key) != mComponentTable.end();
}
// ==============================================================================================================
void AgMaterial::Print( Option_t *opts ) const
{
  TString name   = GetName();
  TString output = "";
  //LOG_INFO << "["<< ((mLock)?"-":"+") << "] " << Form("%20s:",name.Data()) << " ";
  output = TString("[") + TString((mLock)?"-":"+") + TString("] ") + Form("%20s:",name.Data()) + TString(" ");
  std::map<std::string, Double_t > mypar=mParameters;
  for ( UInt_t j=0;j<mParameterList.size();j++ )
    {
      const char* key=mParameterList[j].c_str();
      if ( isSet(key) ) 
	{ 
	  output += Form(" %s=%9.5g",key,mypar[key]); 
	}
      else
	{ 
	  output += Form(" %s= <unset> ",key); 
	}
    }
  LOG_INFO << output.Data() << endm;

  if ( mType > kMaterial ) {
    LOG_INFO << Form("\t\t\t\t\t\t\t\t\t\t           A           Z         W")<<endm;
    for ( UInt_t i=0;i<mC.size();i++ )
      {
	TString comp = mC[i];
	Double_t a = mA[i];
	Double_t z = mZ[i];
	Double_t w = mW[i];
	LOG_INFO << Form("\t\t\t\t\t\t\t\t\t\t %4s   %6.3f     %6.3f    %6.3f",comp.Data(),a,z,w)<<endm;
      }
  }

}

// ==============================================================================================================
AgMaterial &AgMaterial::Get( const Char_t *name )
{

  AgModule   *module = AgBlock::module();
  AgMaterial *material = 0;

  TString modname="None";
  if ( module ) modname = module->GetName();

  //  std::cout << "Info in <AgMaterial::Get(name)>: module="<<modname.Data()<<" material="<<name<<std::endl;

  //////////////////////////////////////////////////////////////
  //
  // If there is no module, probably being intialized above.
  //
  //////////////////////////////////////////////////////////////
  if ( modname == "None" )
    {
      material = Find(name);
      if ( !material )
	{	  
	  material = new AgMaterial(name);
	  mMaterialList.push_back(name);
	  mMaterialTable[name]=material;
	}
      return (*mMaterialTable[name]);

    }

  //////////////////////////////////////////////////////////////
  //
  // Lookup material by name MODULE_MATERIAL and return
  // it if it exists
  //
  //////////////////////////////////////////////////////////////
  
  TString   matname = modname+"_"+name;

  material = Find(matname);
  if ( material )
    {

      return (*material);
    }

  //////////////////////////////////////////////////////////////
  //
  // If it doesn't exist, lookup the named material and
  // copy it into MODULE_MATERIAL.  Then return.
  //
  //////////////////////////////////////////////////////////////  

  material = Find(name);
  if ( material )
    {

      AgMaterial nm(matname);
      nm = *material;

      mMaterialTable[matname]=new AgMaterial(nm);
      mMaterialTable[matname]->SetName(matname);
      mMaterialTable[matname]->SetTitle(Form("Material %s created in module %s",matname.Data(),modname.Data()));
      mMaterialList.push_back(matname);
      return (*mMaterialTable[matname]);
    }

  //////////////////////////////////////////////////////////////
  //
  // Otherwise we cannot find an appropriate material so we
  // will be creating a new one which inherits all of its
  // properties from the mother block
  //
  ////////////////////////////////////////////////////////////// 

  //std::cout << "Returning new material " << matname.Data() << std::endl;

  material = new AgMaterial(matname);

  if ( AgBlock::previous() )                    // Inherit from creating block
    material->Inherit( AgBlock::previous() ); 
  else if ( AgBlock::module() )                 // Or from the current material loaded in module    
    material->Inherit( AgBlock::module() );

  //std::cout << "++ After inheritance from mother block" << std::endl;

  if ( AgBlock::active() )                      // And finally the active block
    material->Inherit( AgBlock::active() );   

  //std::cout << "++ After inheritance from active block" << std::endl;
  //material->Print();

  material->SetName(matname);
  mMaterialTable[matname]=material;
  mMaterialList.push_back(matname); 

  return (*mMaterialTable[matname]); 

}

// ==============================================================================================================
AgMaterial AgMaterial::CopyMaterial( const Char_t *name )
{ 

  AgModule   *module   = AgBlock::module();
  AgBlock    *current = AgBlock::active();
  AgMaterial *material = 0;

  TString modname="None";
  TString volname="None";
  if ( module  ) modname = module->GetName();
  if ( current ) volname = current->GetName();
  

  //  std::cout << "Info in <AgMaterial::Get(name)>: module="<<modname.Data()<<" material="<<name<<std::endl;

  //////////////////////////////////////////////////////////////
  //
  // If there is no module, probably being intialized above.
  //
  //////////////////////////////////////////////////////////////
  if ( modname == "None" )
    {
      material = Find(name);
      if ( !material )
	{	  
	  material = new AgMaterial(name);
	  mMaterialList.push_back(name);
	  mMaterialTable[name]=material;
	}
      return (*mMaterialTable[name]);

    }

  //////////////////////////////////////////////////////////////
  //
  // Lookup material by name MODULE_MATERIAL and return
  // it if it exists
  //
  //////////////////////////////////////////////////////////////
  
  TString   matname = modname+"_"+name;

  material = Find(matname);
  if ( material )
    {

      return (*material);
    }

  //////////////////////////////////////////////////////////////
  //
  // If it doesn't exist, lookup the named material and
  // copy it into MODULE_MATERIAL.  Then return.
  //
  //////////////////////////////////////////////////////////////  

  material = Find(name);
  if ( material )
    {

      AgMaterial nm(matname);
      nm = *material;

      mMaterialTable[matname]=new AgMaterial(nm);
      mMaterialTable[matname]->SetName(matname);
      mMaterialTable[matname]->SetTitle(Form("Material %s created in module %s",matname.Data(),modname.Data()));
      mMaterialList.push_back(matname);
      return (*mMaterialTable[matname]);
    }

  cout << "AgML Error detected.  Copy of nonexistant material." << endl;
  cout << Form("<Material name=\"%s\" />",name) << endl;
  cout << Form("  In module %s volume %s", modname.Data(), volname.Data() ) << endl;

  assert(0); // should never get here
}

// ==============================================================================================================
Bool_t AgMaterial::IsDefined( const Char_t *name )
{
  TString key = name;
  return mMaterialTable.find(key) != mMaterialTable.end();
}

// ==============================================================================================================
void AgMaterial::List( Option_t *opts )
{
  TString key=opts;
  if ( TString(opts) == "ALL" ) 
    for ( UInt_t i=0;i<mMaterialList.size();i++ )  {
	mMaterialTable[ mMaterialList[i] ]->Print();
      }
  else if ( IsDefined(key) ) {
      mMaterialTable[ key ]->Print();
    }
  else  {
      std::cout << Form("Material %s not found",opts) << std::endl;
    }
}

AgMaterial *AgMaterial::Find( const Char_t *name )
{
  if (!IsDefined(name)) 
    { 
      return NULL; 
    }
  return mMaterialTable[name];
}

// ==============================================================================================================
void AgMaterial::Inherit( AgBlock *block )  
{ if ( !block ) return; // Cannot copy from nothing

  // Get the material from the specified block
  AgMaterial *other = block->material();

  // Keep a list of properties which we inherit from the parent block
  TString inlist;

  // Loop over the list of parameters
  for ( UInt_t i=0;i<mParameterList.size();i++ )
    { TString key=mParameterList[i];

      //
      // If the other material has the current key
      // and is set, then we set that material property
      //
      if ( other->hasPar(key) && other->isSet(key) )
	{
	  par(key) = other->par(key);
	  inlist += Form("%s ",key.Data());
	  //%%%	  std::cout << Form("Material %s inherits %s=%9.3f from %s",GetName(),key.Data(),par(key),block->GetName()) << std::endl;
	}
    }

  // Copy mixture parameters
  /*
  mType=other->mType;
  if ( mType == kMixture ) 
    {
      mC = other->mC;
      mA = other->mA;
      mZ = other->mZ;
      mW = other->mW;
    }
  */

  //
  // Set the name of this material.  If the current name is None,
  // then the user did not specify a material, and we should have
  // done a complete copy of the parent's material
  //
  TString name = GetName();
  if ( name == "None" )
    {
      SetName  ( other->GetName() );
    }
  SetTitle ( Form("Material %s inheriting %s from block %s",GetName(),inlist.Data(),block->GetName()) );

  /***
  Print();
  ***/

}

// TODO: Switch components to an STL map rather than an STL vector, keyed by the name of the component. 
// This solves the repeat filling of the material...
void AgMaterial::Component( const Char_t *name, Double_t a, Double_t z, Double_t weight )
{

  if ( a<0||z<0||weight<0 )
    {
      Fatal("Component",Form("Component %s of material %s cannot have negative a,z,weight",name,GetName()));
      assert(2+2==5);
    }

  if ( mLock )
    { // Return silently here
      return;
    }

  if ( mType==kMaterial )
    {
      mC.clear();          // Clear components, which are just copies of the original
      mA.clear(); 
      mZ.clear(); 
      mW.clear();
      mType=kMixture;      // Reset behaviour to mixture
    }

  // Do not keep adding components on each call
  //  if ( hasComponent(name) )
  //    return;

  MyComponent c = {a,z,weight};
  if ( hasComponent(name) )
    {
      MyComponent c2 = mComponentTable[ name ];
      c.w += c2.w; // Assumes A and Z are equal to original!
      assert(c.a==c2.a);
      assert(c.z==c2.z);
    }
  mComponentTable[ TString(name) ] = c;

  mC.push_back(name);
  mA.push_back(a);
  mZ.push_back(z);
  mW.push_back(weight);
  
  if ( weight >= 1.0 )       // If we detect an atomic formula, set as atomic formula
    {
      mType=kCompound;
    }

  if ( mType == kCompound && weight < 1.0 )
    {

      Warning(GetName(),Form("I think %s is a compound, but now I am not sure since %s w=%f",GetName(),name,weight));

    }

  // Update the number of elements
  par("nelem")=(Double_t)mC.size();

  // Average the properties
  Average();


}

// ==============================================================================================================
void AgMaterial::Average()
{

  Double_t a=0.0, z=0.0, weight=0.0;

  for ( UInt_t i=0;i<mC.size();i++ )
    {
      Double_t A = mA[i];
      Double_t Z = mZ[i];
      Double_t W = ( mType==kMixture )? mW[i] : A*mW[i];
            
      a += W*A;
      z += W*Z;
      weight += W;      
    }
  
  a/=weight;
  z/=weight;

  par("a")=a;
  par("z")=z;

}

// ==============================================================================================================
AgMaterial::AgMaterial( const AgMaterial &other )
{

  (*this)=other;
  SetName( other.GetName() );
  SetTitle( other.GetTitle() );
  mLock=false; // Default copy to unlocked 
  /*
  mParameters=other.mParameters;
  mC=other.mC;
  mA=other.mA;
  mZ=other.mZ;
  mW=other.mW;
  mLock=false; // Default copy to unlocked 
  mType=other.mType;
  */
}


// ==============================================================================================================
Int_t AgMaterial::numberOfComponents()
{
  Int_t nc = mC.size();
  return nc;
}

void AgMaterial::Component( Int_t ic, TString &name, Double_t &a, Double_t &z, Double_t &w )
{
  name=mC[ic]; a=mA[ic]; z=mZ[ic]; w=mW[ic];
  return;
}

Double_t AgMaterial::sumWeights()
{
  Int_t nc = mC.size();
  Double_t sumw = 0.0;
  for ( Int_t i =0;i<nc;i++ )
    {
      sumw += mW[i];
    }
  return sumw;
}
// ==============================================================================================================
