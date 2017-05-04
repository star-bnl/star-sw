/*
   C.Kim
   FPost geometry show / print
   Modified oleg's original version
*/

#include "TGeoManager.h"
#include "TGeoMaterial.h"
#include "TGeoMedium.h"

#include <iostream>
#include <fstream>
using namespace std;

void pos_geometry(const char *display="123456")
{
	TString* show = new TString( display );

	TGeoManager* pos = new TGeoManager("postshower", "postshower detector");

	TGeoMaterial* vacuum  = new TGeoMaterial("vacuum",  0,0,0);  
	TGeoMaterial* plastic = new TGeoMaterial("plastic", 1.02,1,1); 
	TGeoMedium* Air   = new TGeoMedium("Vacuum",       0, vacuum);
	TGeoMedium* Scint = new TGeoMedium("Scintillator", 1, plastic);

	TGeoVolume* top = pos->MakeBox("top", Air, 1000,1000,1000);
	pos->SetTopVolume(top);
	pos->SetTopVisible(0);
	TGeoVolume* marker = pos->MakeBox("marker", Scint, 1.,1.,1.);
	top->AddNodeOverlap(marker, 1, new TGeoTranslation( 120, 120,0));
	top->AddNodeOverlap(marker, 2, new TGeoTranslation(-120,-120,0));

	TGeoVolume *SL1 = pos->MakeBox("SL1", Air, 120,120,40);
	SL1->SetVisibility(0);
	top->AddNodeOverlap(SL1, 1, 0);
	TGeoVolume *SL2 = pos->MakeBox("SL2", Air, 120,120,40);
	SL2->SetVisibility(0);
	top->AddNodeOverlap(SL2, 1, 0);
	TGeoVolume *NL3 = pos->MakeBox("NL3", Air, 120,120,40);
	NL3->SetVisibility(0);
	top->AddNodeOverlap(NL3, 1, 0);

	TGeoVolume *L4 = pos->MakeBox("L4", Air, 120,120,40);
	L4->SetVisibility(0);
	top->AddNodeOverlap(L4, 1, 0);
	TGeoVolume *L5 = pos->MakeBox("L5", Air, 120,120,40);
	L5->SetVisibility(0);
	top->AddNodeOverlap(L5, 1, 0);
	TGeoVolume *L6 = pos->MakeBox("L6", Air, 120,120,40);
	L6->SetVisibility(0);
	top->AddNodeOverlap(L6, 1, 0);

	//-------------------------------------------

	const int n_x = 17;
	const int n_y = 43;
	const int n_v = 19;
	const int n_c = 6;

	const int n_tc = 7;
	const int n_tx = 9;
	const int n_tv = 7;

	// all lengths in cm
	const float swidth  = 5.0;
	const float dwrap   = 0.025;
	const float start_x = 23.0;
	const float start_y = -108.0375;
	const float fmsSmallCellOuterEdge = 750; //FMS inner edge from IP (700) + small cell depth (45) + 5

	const float length_S1 = 108.0;
	const float length_S2 = 85.0;
	const float length_S3 = (length_S2-swidth)*sqrt(2);
	float S3d_length[7] = {0};

	cout << "sintillator length:" << endl;
	cout << Form("S1 %f", length_S1) << endl;
	cout << Form("S2 %f", length_S2) << endl;
	cout << Form("S3 %f", length_S3) << endl;

	TGeoVolume *slat_S1 = pos->MakeBox("slat_S1", Scint, swidth/2., length_S1/2., 0.5);
	TGeoVolume *slat_S2 = pos->MakeBox("slat_S2", Scint, swidth/2., length_S2/2., 0.5);
	TGeoVolume *slat_S3 = pos->MakeBox("slat_S3", Scint, swidth/2., length_S3/2., 0.5);
	TGeoVolume *slat_S3c[7];
	for(int i=0; i<7; i++)
	{
		S3d_length[i] = length_S3 - 2*(swidth+dwrap)*(i+1);
		slat_S3c[i] = pos->MakeBox("slat_S3c", Scint, swidth/2., S3d_length[i]/2., 0.5);
		cout << Form("S3-%d %f ", i+1, S3d_length[i]) << endl;
	}

	//-------------------------------------------

	TGeoTranslation *trans;
	TGeoRotation    *rot;
	TGeoCombiTrans  *transrot;
	float set_x, set_y, set_z = fmsSmallCellOuterEdge;
	int side, scLayer;
	int ch = -1;

	const int scWidth = 5;
	const int scThick = 1;
	int scAngle = 0;

	ofstream out;
	out.open("fPostGeom.txt");

	//--------------------------------------------------------------------

	side = 1;

	cout << "==== SL1 (x segmentation)" << endl;
	scLayer = 1;
	scAngle = 90;
	set_x = (swidth+dwrap)*4;
	set_y = 65.5;
	for( int i=0; i<n_tx; i++ ) {
		trans = new TGeoTranslation(set_x, set_y, set_z);
		SL1->AddNodeOverlap(slat_S2,++ch,trans);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S2 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out <<Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S2",
					ch, side, scLayer, i+1, length_S2, set_x, set_y, set_z) <<endl;
					*/
		out <<Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S2, scWidth, scThick, scAngle, set_x, set_y, set_z) <<endl;

		set_x -= swidth+dwrap;
	}

	cout << "==== SL2 (south side complement, v segmentation)" << endl;
	scLayer = 2;
	scAngle = 45;
	set_x = 8.5*(swidth+dwrap)*sqrt(2);
	set_y = 65.5;
	set_z = fmsSmallCellOuterEdge + 5.0;
	rot = new TGeoRotation("SL2",-45,0,0);

	set_x -= (swidth+dwrap) * (n_tv*sqrt(2) + (n_tc-1)/sqrt(2));
	set_y += (swidth+dwrap) * (n_tc+1)/sqrt(2);
	// Top corner
	for( int i=0; i<n_tc; i++ )
	{
		set_x += (swidth+dwrap)/sqrt(2);
		set_y -= (swidth+dwrap)/sqrt(2);
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		SL2->AddNodeOverlap(slat_S3c[6-i],++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3-%d ", ch, i+1, set_x, set_y, set_z, 7-i) << endl;

		/*
		out <<Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3-%d ",
					ch, side, scLayer, i+1, S3d_length[6-i], set_x, set_y, set_z, 7-i) <<endl;
					*/
		out <<Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, S3d_length[6-i], scWidth, scThick, scAngle, set_x, set_y, set_z) <<endl;
	}
	set_x += (swidth+dwrap)/sqrt(2);
	set_y -= (swidth+dwrap)/sqrt(2);
	// Full lengths
	for( int i=0; i<n_tv; i++ )
	{
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		SL2->AddNodeOverlap(slat_S3,++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3 ", ch, i+1+n_tc, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3 ",
					ch, side, scLayer, i+1+n_tc, length_S3, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S3, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_x += (swidth+dwrap)*sqrt(2);
	}

	cout << "==== SL4 (v segmentation)" << endl;
	scLayer = 4;
	scAngle = 45;
	set_z = fmsSmallCellOuterEdge + 30.0;
	// Full lengths
	set_x = 65.5;
	set_y = start_y+(60.0+dwrap)/sqrt(2)+(swidth+dwrap)/2.0;
	rot = new TGeoRotation("L4",-45,0,0);
	set_y += (swidth+dwrap)*sqrt(2)*(n_v-1);
	for( int i=0; i<n_v; i++ )
	{
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		L4->AddNodeOverlap(slat_S3,++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3 ",
					ch, side, scLayer, i+1, length_S3, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S3, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_y -= (swidth+dwrap)*sqrt(2);
	}
	// Bottom corner
	set_x = 65.5;
	set_y = start_y+(60.0+dwrap)/sqrt(2)+(swidth+dwrap)/2.0;
	rot = new TGeoRotation("L4",-45,0,0);
	for( int i=0; i<n_c; i++ )
	{
		set_x += (swidth+dwrap)/sqrt(2);
		set_y -= (swidth+dwrap)/sqrt(2);
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		L4->AddNodeOverlap(slat_S3c[i],++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3-%d ", ch, i+1+n_v, set_x, set_y, set_z, i+1) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3-%d ",
					ch, side, scLayer, i+1+n_v, S3d_length[i], set_x, set_y, set_z, i+1) <<endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, S3d_length[i], scWidth, scThick, scAngle, set_x, set_y, set_z, i+1) <<endl;
	}

	cout << "==== SL5 (y segmentation)" << endl;
	scLayer = 5;
	scAngle = 0;
	set_x = 65.5;
	set_y = start_y+(swidth+dwrap)/2.0;
	set_z = fmsSmallCellOuterEdge + 35.0;
	rot = new TGeoRotation("L5",90,0,0);
	set_y += (swidth+dwrap)*(n_y-1);
	for( int i=0; i<n_y; i++ )
	{
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		L5->AddNodeOverlap(slat_S2,++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S2 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S2 ",
					ch, side, scLayer, i+1, length_S2, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S2, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_y -= swidth+dwrap;
	}

	cout << "==== SL6 (x segmentation)" << endl;
	scLayer = 6;
	scAngle = 90;
	// Top
	set_x = start_x+(swidth+dwrap)/2.0;
	set_y = 108.0/2.0;
	set_z = fmsSmallCellOuterEdge + 40.0;
	for( int i=0; i<n_x; i++ )
	{
		trans = new TGeoTranslation(set_x, set_y, set_z);
		L4->AddNodeOverlap(slat_S1,++ch,trans);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S1 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S1 ",
					ch, side, scLayer, i+1, length_S1, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S1, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_x += swidth+dwrap;
	}
	// Bottom (will need 180 deg rotation with light guides)
	set_x = start_x+(swidth+dwrap)/2.0;
	set_y = -108.0/2.0;
	for( int i=n_x; i<n_x*2; i++ )
	{
		trans = new TGeoTranslation(set_x, set_y, set_z);
		L4->AddNodeOverlap(slat_S1,++ch,trans);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S1 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S1 ",
					ch, side, scLayer, i+1, length_S1, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S1, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_x += swidth+dwrap;
	}

	//--------------------------------------------------------------------

	side = 2;

	cout << "==== NL3 (north side complement, v segmentation)" << endl;
	scLayer = 3;
	scAngle = 135;
	set_x = -8.5*(swidth+dwrap)*sqrt(2);
	set_y = 65.5;
	set_z = fmsSmallCellOuterEdge + 10.0;
	rot = new TGeoRotation("NL3",45,0,0);

	set_x += (swidth+dwrap) * (n_tv*sqrt(2) + (n_tc-1)/sqrt(2));
	set_y += (swidth+dwrap) * (n_tc+1)/sqrt(2);
	for( int i=0; i<n_tc; i++ )
	{
		set_x -= (swidth+dwrap)/sqrt(2);
		set_y -= (swidth+dwrap)/sqrt(2);
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		NL3->AddNodeOverlap(slat_S3c[6-i],++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3-%d ", ch, i+1, set_x, set_y, set_z, 7-i) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3-%d ",
					ch, side, scLayer, i+1, S3d_length[6-i], set_x, set_y, set_z, 7-i) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, S3d_length[6-i], scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;
	}
	set_x -= (swidth+dwrap)/sqrt(2);
	set_y -= (swidth+dwrap)/sqrt(2);
	for( int i=0; i<n_tv; i++ )
	{
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		NL3->AddNodeOverlap(slat_S3,++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3 ", ch, i+1+n_tc, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3 ",
					ch, side, scLayer, i+1+n_tc, length_S3, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S3, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_x -= (swidth+dwrap)*sqrt(2);
	}

	cout << "==== NL4 (v segmentation)" << endl;
	scLayer = 4;
	scAngle = 135;
	set_z = fmsSmallCellOuterEdge + 30.0;
	//Full lengths
	set_x = -65.5;
	set_y = start_y+(60.0+dwrap)/sqrt(2)+(swidth+dwrap)/2.0;
	rot = new TGeoRotation("L4",45,0,0);
	set_y += (swidth+dwrap)*sqrt(2)*(n_v-1);
	for( int i=0; i<n_v; i++ )
	{
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		L4->AddNodeOverlap(slat_S3,++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3 ",
					ch, side, scLayer, i+1, length_S3, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S3, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_y -= (swidth+dwrap)*sqrt(2);
	}
	//Bottom corner
	set_x = -65.5;
	set_y = start_y+(60.0+dwrap)/sqrt(2)+(swidth+dwrap)/2.0;
	rot = new TGeoRotation("L4",45,0,0);
	for( int i=0; i<n_c; i++ )
	{
		set_x -= (swidth+dwrap)/sqrt(2);
		set_y -= (swidth+dwrap)/sqrt(2);
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		L4->AddNodeOverlap(slat_S3c[i],++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S3-%d ", ch, i+1+n_v, set_x, set_y, set_z, i+1) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S3-%d ",
					ch, side, scLayer, i+1+n_v, S3d_length[i], set_x, set_y, set_z, i+1) <<endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, S3d_length[i], scWidth, scThick, scAngle, set_x, set_y, set_z) <<endl;
	}

	cout << "==== NL5 (y segmentation)" << endl;
	scLayer = 5;
	scAngle = 0;
	set_x = -65.5;
	set_y = start_y+(swidth+dwrap)/2.0;
	set_z = fmsSmallCellOuterEdge + 35.0;
	rot = new TGeoRotation("L5",-90,0,0);
	set_y += (swidth+dwrap)*(n_y-1);
	for( int i=0; i<n_y; i++ )
	{
		transrot = new TGeoCombiTrans( set_x, set_y, set_z, rot );
		L5->AddNodeOverlap(slat_S2,++ch,transrot);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S2 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S2 ",
					ch, side, scLayer, i+1, length_S2, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S2, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_y -= swidth+dwrap;
	}

	cout << "==== NL6 (x segmentation)" << endl;
	scLayer = 6;
	scAngle = 90;
	set_x = -start_x-(swidth+dwrap)/2.0;
	set_y = 108.0/2.0;
	set_z = fmsSmallCellOuterEdge + 40.0;
	for( int i=0; i<n_x; i++ )
	{
		// Top
		set_y = 108.0/2.0;
		trans = new TGeoTranslation(set_x, set_y,set_z);
		L4->AddNodeOverlap(slat_S1,++ch,trans);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S1 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S1 ",
					ch, side, scLayer, i+1, length_S1, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S1, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_x += -swidth+dwrap;

	}
	set_x = -start_x-(swidth+dwrap)/2.0;
	set_y = -108.0/2.0;
	for( int i=n_x; i<n_x*2; i++ )
	{
		// Bottom (will need 180 deg rotation with light guides)
		set_y = -108.0/2.0;
		trans = new TGeoTranslation(set_x, set_y, set_z);
		L4->AddNodeOverlap(slat_S1,++ch,trans);
		cout << Form("%3d: %2d (%8.3f, %8.3f, %8.3f) S1 ", ch, i+1, set_x, set_y, set_z) << endl;

		/*
		out << Form("%3d %d %d %2d %8.3f %8.3f %8.3f %8.3f S1 ",
					ch, side, scLayer, i+1, length_S1, set_x, set_y, set_z) << endl;
					*/
		out << Form("%3d %7.3f %i %i %3i %8.3f %8.3f %7.3f",
					ch, length_S1, scWidth, scThick, scAngle, set_x, set_y, set_z) << endl;

		set_x += -swidth+dwrap;
	}

	out.close();
	if( !show->Contains("1") ) SL1->SetVisDaughters( kFALSE );
	if( !show->Contains("2") ) SL2->SetVisDaughters( kFALSE );
	if( !show->Contains("3") ) NL3->SetVisDaughters( kFALSE );
	if( !show->Contains("4") )  L4->SetVisDaughters( kFALSE );
	if( !show->Contains("5") )  L5->SetVisDaughters( kFALSE );
	if( !show->Contains("6") )  L6->SetVisDaughters( kFALSE );
	top->Draw();
	//top->Draw("ogl");
}
