#include "AgPlacement.h"
ClassImp(AgPlacement);
#include <iostream>

//#define __DEBUG_MATRIX__

#include "TGeoMatrix.h"

AgPlacement::AgPlacement(const Char_t *block, const Char_t *mother, const Char_t *group):
  TNamed(block,Form("Place %s into %s",block,mother)), mHasRotm(false),mType(kUnknown)
{
	mBlock=block;
	mMother=mother;
	mGroup=group;
}

const Char_t *AgPlacement::block(){ return mBlock.Data(); }
const Char_t *AgPlacement::mother(){ return mMother.Data(); }
const Char_t *AgPlacement::group(){ return mGroup.Data(); }

void AgPlacement::AlphaX( Double_t ax )
{
	Rotation r;
	r.rot3.alpha=ax;
	r.rot3.axis=kAlphaX;
	mRotation.push_back(r);
	mType.push_back( kRot3 );
}

void AgPlacement::AlphaY( Double_t ay )
{
	Rotation r;
	r.rot3.alpha=ay;
	r.rot3.axis=kAlphaY;
	mRotation.push_back(r);
	mType.push_back( kRot3 );
}

void AgPlacement::AlphaZ( Double_t az )
{
	Rotation r;
	r.rot3.alpha=az;
	r.rot3.axis=kAlphaZ;
	mRotation.push_back(r);
	mType.push_back( kRot3 );
}

void AgPlacement::Reference( Double_t thetax, Double_t phix, Double_t thetay, Double_t phiy, Double_t thetaz, Double_t phiz )
{
	Rotation r;
	r.rot6.thetax=thetax;
	r.rot6.thetay=thetay;
	r.rot6.thetaz=thetaz;
	r.rot6.phix=phix;
	r.rot6.phiy=phiy;
	r.rot6.phiz=phiz;
	mRotation.push_back(r);
	mType.push_back( kRot6 );
}

void AgPlacement::Ortho( const Char_t *ort )
{  
	TString myort=ort;
	myort.ToUpper(); // Should be upcased for below
	Rotation r;
	// Decode ORT
	Short_t dir=+1;
	Int_t axis=0;
	for ( Int_t i=0;i<myort.Length();i++ )
	{
		if ( myort(i,1) == "+" ) { dir =  1; continue; }
		if ( myort(i,1) == "-" ) { dir = -1; continue; }
		if ( myort(i,1) == "X" ) { dir *= 1; }
		if ( myort(i,1) == "Y" ) { dir *= 2; }
		if ( myort(i,1) == "Z" ) { dir *= 3; }
		r.ort[ axis++ ] = dir;
		dir=+1; // and reset to + direction
	}
	mRotation.push_back(r);
	mType.push_back(kRotO);
}

void AgPlacement::Print(Option_t *opts)const
{

	const Char_t *alphas[]={"alphax","alphay","alphaz"};

	std::cout << GetTitle() << std::endl;
	std::cout << TString::Format("+ x=%9.4f cm y=%9.4f cm z=%9.4f cm",mTranslation.x,mTranslation.y,mTranslation.z) << std::endl;
	for ( UInt_t i=0;i<mRotation.size();i++ )
	{
		Rotation R=mRotation[i];
		if ( mType[i]==kRot3 )
		{
			std::cout << "+ " << alphas[ R.rot3.axis ] << "="<< R.rot3.alpha << std::endl;
		}
		else if ( mType[i]==kRot6 )
		{
			std::cout << "+ ";
			std::cout << "thetax=" << R.rot6.thetax << " ";
			std::cout << "phix=" << R.rot6.phix << " ";
			std::cout << "thetay=" << R.rot6.thetay << " ";
			std::cout << "phiy=" << R.rot6.phiy << " ";
			std::cout << "thetaz=" << R.rot6.thetaz << " ";
			std::cout << "phiz=" << R.rot6.phiz << " ";
			std::cout << std::endl;
		}
		else if ( mType[i]==kRotO )
		{
			std::cout << TString::Format("+ dirX=%2i dirY=%2i dirZ=%2i",R.ort[0],R.ort[1],R.ort[2])<<std::endl;
		}
		else
		{
			std::cout << "I dunno" << std::endl;
		}

	}

}
#define __DEBUG_MATRIX__
TGeoCombiTrans *AgPlacement::matrix()
{

  //	std::cout << "=============================================================================" << std::endl;
  //	std::cout << Form("Place %s in %s",block(),mother()) << std::endl;


	// Execute rotations
	//$$$	TGeoTranslation position(mTranslation.x,mTranslation.y,mTranslation.z);
	Double_t x = mTranslation.x;
	Double_t y = mTranslation.y;
	Double_t z = mTranslation.z;
	TGeoRotation rotation;



	for ( UInt_t i=0;i<mRotation.size();i++ )
	{
		Rotation R=mRotation[i];
		if ( mType[i]==kRot3 )
		{

		  //			std::cout << "Rotation about axis="<<R.rot3.axis<<" by alpha="<<R.rot3.alpha << std::endl;

			if ( R.rot3.axis == 0 ) rotation.RotateX( R.rot3.alpha );
			if ( R.rot3.axis == 1 ) rotation.RotateY( R.rot3.alpha );
			if ( R.rot3.axis == 2 ) rotation.RotateZ( R.rot3.alpha );
		}
		else if ( mType[i]==kRot6 )
		{

		  //			std::cout << Form("G3 Rotation...") << std::endl;

			Double_t thetax = R.rot6.thetax;
			Double_t phix   = R.rot6.phix;
			Double_t thetay = R.rot6.thetay;
			Double_t phiy   = R.rot6.phiy;
			Double_t thetaz = R.rot6.thetaz;
			Double_t phiz   = R.rot6.phiz;
			TGeoRotation temp;
			temp.SetAngles(thetax,phix,thetay,phiy,thetaz,phiz);
			rotation = rotation*temp;
		}
		else if ( mType[i]==kRotO )
		{

		  //			std::cout << Form("ORT Rotation") << std::endl;

			//                          -Z   -Y   -X        +X   +Y   +Z
			const Double_t thetas[7]={ 180,  90,  90, 000,  90,  90,   0 };
			const Double_t phis[7]  ={   0, 270, 180, 000,   0,  90,   0 };
			Int_t ix=R.ort[0], iy=R.ort[1], iz=R.ort[2];

			//			std::cout << Form("    ix=%3i iy=%3i iz=%3i",ix,iy,iz)<<std::endl;

			TGeoRotation temp;
			temp.SetAngles( thetas[ix+3], phis[ix+3], thetas[iy+3], phis[iy+3], thetas[iz+3], phis[iz+3] );
			rotation = rotation*temp;
		}
		else
		{
		  //			std::cout << "I dunno" << std::endl;
		}
	}

	// Completely specified by rotation matrix provided
	if ( mHasRotm ) 
	  {
	    rotation.SetMatrix( mRotationMatrix );
	  }


	TGeoRotation *RotInHell = new TGeoRotation(rotation);
	RotInHell->SetName(Form("rot_%s_in_%s",mBlock.Data(),mMother.Data()));

	//TGeoRotation *RotInHell = (TGeoRotation*)rotation.Clone(Form("rot_%s_in_%s",mBlock.Data(),mMother.Data()));
	//TGeoCombiTrans *translation = new TGeoCombiTrans( x, y, z, 
	//	 (TGeoRotation*)rotation.Clone(Form("rot_%s_in_%s",mBlock.Data(),mMother.Data())));

	TGeoCombiTrans *translation = new TGeoCombiTrans( x, y, z, RotInHell );
	translation->SetName(Form("mat_%s_in_%s",mBlock.Data(),mMother.Data()));

	//	return (TGeoCombiTrans*)gGeoIdentity;

	return translation;

}

Double_t &AgPlacement::par( const Char_t *name )
{
	//static Double_t _dummy = 0;
	TString key=name;
	return mParameters[key];
}


Bool_t AgPlacement::isSet( const Char_t *par ) const
{
	TString key=par;
	return ( mParameters.find(key) != mParameters.end() );
}
