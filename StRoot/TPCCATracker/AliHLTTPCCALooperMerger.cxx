
#include "AliHLTTPCCATracker.h"
#include "AliHLTTPCCAGBHit.h"

#include "AliHLTTPCCALooperMerger.h"

#include "AliHLTTPCCAMath.h"
#include "Stopwatch.h"

#include "AliHLTTPCCAMergedTrack.h"
#include "AliHLTTPCCAMergerOutput.h"
#include "AliHLTTPCCADataCompressor.h"

//#define DRAW_L
#ifdef DRAW_L
#include "AliHLTTPCCADisplay.h"
#endif

#include <iostream>
using std::cout;
using std::endl;

void AliHLTTPCCALooperMerger::FillSegments()
{
  int nRecoTracks = fOutput.NTracks();
  for( int irt = 0; irt < nRecoTracks; irt++ ) {
      const AliHLTTPCCAMergedTrack &track = fOutput.Track( irt );
      if( track.Used() ) continue;
    if( fabs( track.InnerParam().QPt() ) < 5 && fabs( track.OuterParam().QPt() ) < 5 ) continue;
    int h1(0), h2((track.NClusters()-1)/2), h3(track.NClusters()-1);
    if( track.NClusters() > 7 ) { h1++; h3--; }
    const DataCompressor::SliceRowCluster &iDsrc1 = fOutput.ClusterIDsrc( track.FirstClusterRef() + h1 );
    const DataCompressor::SliceRowCluster &iDsrc2 = fOutput.ClusterIDsrc( track.FirstClusterRef() + h2 );
    const DataCompressor::SliceRowCluster &iDsrc3 = fOutput.ClusterIDsrc( track.FirstClusterRef() + h3 );
    int iHit1 = fFirstSliceHit[iDsrc1.Slice()] + slices[iDsrc1.Slice()]->ClusterData().RowOffset( iDsrc1.Row() ) + iDsrc1.Cluster();
    int iHit2 = fFirstSliceHit[iDsrc2.Slice()] + slices[iDsrc2.Slice()]->ClusterData().RowOffset( iDsrc2.Row() ) + iDsrc2.Cluster();
    int iHit3 = fFirstSliceHit[iDsrc3.Slice()] + slices[iDsrc3.Slice()]->ClusterData().RowOffset( iDsrc3.Row() ) + iDsrc3.Cluster();
    const AliHLTTPCCAGBHit &hit1r = fGBHits[iHit1];
    const AliHLTTPCCAGBHit &hit2r = fGBHits[iHit2];
    const AliHLTTPCCAGBHit &hit3r = fGBHits[iHit3];

    float x_seg_1(hit1r.X()), x_seg_2(hit2r.X()), x_seg_3(hit3r.X());
    float y_seg_1(hit1r.Y()), y_seg_2(hit2r.Y()), y_seg_3(hit3r.Y());
    float z_seg_1(hit1r.Z()), z_seg_2(hit2r.Z()), z_seg_3(hit3r.Z());
    //
    float cos11(slices[hit1r.ISlice()]->Param().SinAlpha());
    float sin11(slices[hit1r.ISlice()]->Param().CosAlpha());
    float cos22(slices[hit2r.ISlice()]->Param().SinAlpha());
    float sin22(slices[hit2r.ISlice()]->Param().CosAlpha());
    float cos33(slices[hit3r.ISlice()]->Param().SinAlpha());
    float sin33(slices[hit3r.ISlice()]->Param().CosAlpha());
    float x_seg_1_g = -(y_seg_1 * cos11 - x_seg_1 * sin11);
    float y_seg_1_g = x_seg_1 * cos11 + y_seg_1 * sin11;
    float x_seg_2_g = -(y_seg_2 * cos22 - x_seg_2 * sin22);
    float y_seg_2_g = x_seg_2 * cos22 + y_seg_2 * sin22;
    float x_seg_3_g = -(y_seg_3 * cos33 - x_seg_3 * sin33);
    float y_seg_3_g = x_seg_3 * cos33 + y_seg_3 * sin33;
      // Center of the circle in global coords
    float cos1(slices[hit2r.ISlice()]->Param().SinAlpha());
    float sin1(slices[hit2r.ISlice()]->Param().CosAlpha());
    float A_g = x_seg_2_g - x_seg_1_g;
    float B_g = y_seg_2_g - y_seg_1_g;
    float C_g = x_seg_3_g - x_seg_1_g;
    float D_g = y_seg_3_g - y_seg_1_g;
    float E_g = A_g * ( x_seg_1_g + x_seg_2_g ) + B_g * ( y_seg_1_g + y_seg_2_g );
    float F_g = C_g * ( x_seg_1_g + x_seg_3_g ) + D_g * ( y_seg_1_g + y_seg_3_g );
    float G_g = 2 * ( A_g * ( y_seg_3_g - y_seg_2_g ) - B_g * ( x_seg_3_g - x_seg_2_g ) );
    float Cxg(0.), Cyg(0.), Cr(0.);
    if( G_g != 0 ) {
      Cxg = ( D_g * E_g - B_g * F_g ) / G_g;
      Cyg = ( A_g * F_g - C_g * E_g ) / G_g;
      Cr = sqrt( (x_seg_1_g-Cxg)*(x_seg_1_g-Cxg) + (y_seg_1_g-Cyg)*(y_seg_1_g-Cyg) );
    }
      // Nearest end farthest points of the circle to (0;0)
    float k_cl_g = Cyg / Cxg;
    float b_cl_g = Cyg - k_cl_g*Cxg;
    float d_cl_g = (pow((2*k_cl_g*b_cl_g - 2*Cxg-2*Cyg*k_cl_g),2)-(4+4*k_cl_g*k_cl_g)*(b_cl_g*b_cl_g-Cr*Cr+Cxg*Cxg+Cyg*Cyg-2*Cyg*b_cl_g));
    float x_dn_r_g = ((-(2*k_cl_g*b_cl_g - 2*Cxg - 2*Cyg*k_cl_g)-sqrt(d_cl_g))/(2+2*k_cl_g*k_cl_g));
    float x_up_r_g = ((-(2*k_cl_g*b_cl_g - 2*Cxg - 2*Cyg*k_cl_g)+sqrt(d_cl_g))/(2+2*k_cl_g*k_cl_g));
    float y_dn_r_g = k_cl_g*x_dn_r_g + b_cl_g;
    float y_up_r_g = k_cl_g*x_up_r_g + b_cl_g;
    float rr_dn = sqrt( x_dn_r_g*x_dn_r_g + y_dn_r_g*y_dn_r_g );
    float rr_up = sqrt( x_up_r_g*x_up_r_g + y_up_r_g*y_up_r_g );
    if( rr_dn > rr_up ) {
      std::swap( x_dn_r_g, x_up_r_g );
      std::swap( y_dn_r_g, y_up_r_g );
    }
    if( ( fabs(x_dn_r_g) < 3 && fabs(y_dn_r_g) < 3 && Cr > 100 ) || ( fabs(x_up_r_g) < 3 && fabs(y_up_r_g) < 3 && Cr > 100 ) ) continue;

    float AB_xy = sqrt( (x_seg_1_g-x_seg_3_g)*(x_seg_1_g-x_seg_3_g) + (y_seg_1_g-y_seg_3_g)*(y_seg_1_g-y_seg_3_g) );
    float BC_xy_dn = sqrt( (x_dn_r_g-x_seg_1_g)*(x_dn_r_g-x_seg_1_g) + (y_dn_r_g-y_seg_1_g)*(y_dn_r_g-y_seg_1_g) );
    float BC_xy_up = sqrt( (x_up_r_g-x_seg_3_g)*(x_up_r_g-x_seg_3_g) + (y_up_r_g-y_seg_3_g)*(y_up_r_g-y_seg_3_g) );
    float AB_xy_curve = 2*Cr*asin( 0.5*AB_xy/Cr );
    float BC_xy_dn_curve = 2*Cr*asin( 0.5*BC_xy_dn/Cr );
    float BC_xy_up_curve = 2*Cr*asin( 0.5*BC_xy_up/Cr );
    float AB_z = fabs( z_seg_1 - z_seg_3 );
    float BC_z_dn = AB_z*BC_xy_dn/AB_xy;
    float BC_z_up = AB_z*BC_xy_up/AB_xy;
    float dz_dn = ( AB_z*BC_xy_dn_curve ) / AB_xy_curve;
    float dz_up = ( AB_z*BC_xy_up_curve ) / AB_xy_curve;
    float h = 2*3.14*Cr*AB_z/AB_xy_curve;
    float z_dn_r = z_seg_1+dz_dn;
    float z_up_r = z_seg_3-dz_up;
    if( track.InnerParam().Z() < track.OuterParam().Z() ) {
      z_dn_r = z_seg_1-dz_dn;
      z_up_r = z_seg_3+dz_up;
    }
    LooperSegment* segment = new LooperSegment;
    segment->iTr = irt;
    segment->QPt_abs = fabs( track.InnerParam().QPt() );
    segment->DzDs_abs = fabs( track.InnerParam().DzDs() );
    segment->Cx = Cxg;
    segment->Cy = Cyg;
    segment->Cr = Cr;
    segment->x_up = x_up_r_g;
    segment->y_up = y_up_r_g;
    segment->z_up = z_up_r;
    segment->x_dn = x_dn_r_g;
    segment->y_dn = y_dn_r_g;
    segment->z_dn = z_dn_r;
    segment->x_h_up = x_seg_3_g;
    segment->y_h_up = y_seg_3_g;
    segment->z_h_up = z_seg_3;
    segment->x_h_dn = x_seg_1_g;
    segment->y_h_dn = y_seg_1_g;
    segment->z_h_dn = z_seg_1;
    segment->h = h;
    segment->slice_mid = hit2r.ISlice();
    segment->iLooper = -1;
    segment->isUsed = false;
    fSegments.push_back( *segment );
  }
//  std::sort( fSegments.begin(), fSegments.end(), LooperSegment::CompareL );
  std::cout<<" ----- fSegments.size(): "<<fSegments.size()<<"\n";
}

void AliHLTTPCCALooperMerger::CheckSegments()
{
#ifdef DRAW_L
AliHLTTPCCADisplay &disp = AliHLTTPCCADisplay::Instance();
disp.SetTPC( slices[0]->Param() );
disp.SetTPCView();
disp.DrawTPC();
#endif
//  std::sort( fSegments.begin(), fSegments.end(), LooperSegment::CompareL );
  vector<int> loopers;
  for( int iSeg = 0; iSeg < fSegments.size(); iSeg++ ) {
    bool newLooper = true;
    if( fSegments[iSeg].isUsed ) {
      newLooper = false;
    } else {
      fSegments[iSeg].isUsed = true;
      fSegments[iSeg].iLooper = fNLoopers;
      loopers.push_back(1);
      fNLoopers++;
    }
    for( int jSeg = iSeg+1; jSeg < fSegments.size(); jSeg++ ) {
//      if( fSegments[jSeg].isUsed ) continue;
      if( fSegments[iSeg].x_h_up == fSegments[jSeg].x_h_up && fSegments[iSeg].y_h_up == fSegments[jSeg].y_h_up
	  && fSegments[iSeg].x_h_dn == fSegments[jSeg].x_h_dn && fSegments[iSeg].y_h_dn == fSegments[jSeg].y_h_dn ) continue;
//      if( fabs(fSegments[iSeg].z_dn - fSegments[jSeg].z_dn) < 0.3*fSegments[iSeg].h ) continue;
      if( fabs(fSegments[iSeg].z_dn - fSegments[jSeg].z_up) < 0.25*fSegments[iSeg].h ) continue;
      if( fabs(fSegments[iSeg].z_up - fSegments[jSeg].z_dn) < 0.25*fSegments[iSeg].h ) continue;
//      if( fSegments[iSeg].z_h_up > fSegments[jSeg].z_h_dn && fSegments[iSeg].z_h_dn < fSegments[jSeg].z_h_up ) continue;
//      if( fSegments[iSeg].z_h_up < fSegments[jSeg].z_h_dn && fSegments[iSeg].z_h_dn > fSegments[jSeg].z_h_up ) continue;
      float i_left = std::min( fSegments[iSeg].z_h_up, fSegments[iSeg].z_h_dn );
      float i_right = std::max( fSegments[iSeg].z_h_up, fSegments[iSeg].z_h_dn );
      float j_left = std::min( fSegments[jSeg].z_h_up, fSegments[jSeg].z_h_dn );
      float j_right = std::max( fSegments[jSeg].z_h_up, fSegments[jSeg].z_h_dn );
//      if( std::min( fSegments[iSeg].z_h_up, fSegments[iSeg].z_h_dn ) < std::max( fSegments[jSeg].z_h_up, fSegments[jSeg].z_h_dn )
//      && std::max( fSegments[iSeg].z_h_up, fSegments[iSeg].z_h_dn ) > std::min( fSegments[jSeg].z_h_up, fSegments[jSeg].z_h_dn ) ) continue;
      if( i_right > j_left && i_left < j_right ) continue;
      if( fabs(fSegments[iSeg].DzDs_abs - fSegments[jSeg].DzDs_abs) > 0.4 ) continue;
      float z_i_mid = (fSegments[iSeg].z_up + fSegments[iSeg].z_dn)*0.5;
      float z_j_mid = (fSegments[jSeg].z_up + fSegments[jSeg].z_dn)*0.5;
      if( (fabs( z_i_mid - z_j_mid ) > fSegments[iSeg].h*2 || fabs( z_i_mid - z_j_mid ) > fSegments[jSeg].h*2) && fSegments[iSeg].Cr > 15 ) continue;
      float r_mid = (fSegments[iSeg].Cr + fSegments[jSeg].Cr)/2;
//      	float r_lim = 0.25*r_mid;
      float r_lim = 0.25*fSegments[iSeg].Cr;
      if( fabs(fSegments[iSeg].Cr - fSegments[jSeg].Cr) < r_lim ) {
      	float xy_lim = 5;//0.2*r_mid;
      	if( 0.2*r_mid > 5 ) xy_lim = 0.2*r_mid;
      	if( 0.2*r_mid > 10 ) xy_lim = 10;
        if( fabs(fSegments[iSeg].x_dn - fSegments[jSeg].x_dn) < xy_lim
	    && fabs(fSegments[iSeg].x_up - fSegments[jSeg].x_up) < xy_lim*1.5
	    /*&& fabs(fSegments[iSeg].Cx - fSegments[jSeg].Cx) < 5*/ ) {
	  if( fabs(fSegments[iSeg].y_dn - fSegments[jSeg].y_dn) < xy_lim
	      && fabs(fSegments[iSeg].y_up - fSegments[jSeg].y_up) < xy_lim*1.5
	      /*&& fabs(fSegments[iSeg].Cy - fSegments[jSeg].Cy) < 5*/ ) {

	    if( fSegments[jSeg].isUsed ) fSegments[iSeg].iLooper = fSegments[jSeg].iLooper;
	    else fSegments[jSeg].iLooper = fSegments[iSeg].iLooper;
	    fSegments[jSeg].isUsed = true;
	    loopers[fSegments[iSeg].iLooper]++;
	  }
	}
      }
    }
  }
#ifdef DRAW_L
  int nRecoTracks = fOutput.NTracks();
  for( int irt = 0; irt < nRecoTracks; irt++ ) {
    const AliHLTTPCCAMergedTrack &track = fOutput.Track( irt );
//        if( track.Used() ) continue;
    if( fabs( track.InnerParam().QPt() ) > 5 || fabs( track.OuterParam().QPt() ) > 5 ) continue;
    float x0, y0, z0;
    for( int ih = 0; ih < track.NClusters(); ih++ ) {
      const DataCompressor::SliceRowCluster &iDsrc1 = fOutput.ClusterIDsrc( track.FirstClusterRef() + ih );
      int iHit1 = fFirstSliceHit[iDsrc1.Slice()] + slices[iDsrc1.Slice()]->ClusterData().RowOffset( iDsrc1.Row() ) + iDsrc1.Cluster();
      const AliHLTTPCCAGBHit &hit1r = fGBHits[iHit1];
      float x_seg_1(hit1r.X());
      float y_seg_1(hit1r.Y());
      float z_seg_1(hit1r.Z());
      float cos11(slices[hit1r.ISlice()]->Param().SinAlpha());
      float sin11(slices[hit1r.ISlice()]->Param().CosAlpha());
      float x_seg_1_g = -(y_seg_1 * cos11 - x_seg_1 * sin11);
      float y_seg_1_g = x_seg_1 * cos11 + y_seg_1 * sin11;
//      disp.DrawHitGlobal( x_seg_1_g, y_seg_1_g, z_seg_1, kGreen, 0.25 );
      if( ih > 0 ) disp.SpecDrawLineG( x_seg_1_g, y_seg_1_g, z_seg_1, x0, y0, z0, 26, 0.15 );
      x0 = x_seg_1_g;
      y0 = y_seg_1_g;
      z0 = z_seg_1;
    }
  }
  //
  for( int i = 0; i < fNLoopers; i++ ) {
    if( loopers[i] != 1 ) continue;
    for( int iSeg = 0; iSeg < fSegments.size(); iSeg++ ) {
      if( fSegments[iSeg].iLooper != i ) continue;
const AliHLTTPCCAMergedTrack &track = fOutput.Track( fSegments[iSeg].iTr );
float x0, y0, z0;
if( track.NClusters() > 65 ) continue;
for( int ih = 0; ih < track.NClusters(); ih++ ) {
  const DataCompressor::SliceRowCluster &iDsrc1 = fOutput.ClusterIDsrc( track.FirstClusterRef() + ih );
  int iHit1 = fFirstSliceHit[iDsrc1.Slice()] + slices[iDsrc1.Slice()]->ClusterData().RowOffset( iDsrc1.Row() ) + iDsrc1.Cluster();
  const AliHLTTPCCAGBHit &hit1r = fGBHits[iHit1];
  float x_seg_1(hit1r.X());
  float y_seg_1(hit1r.Y());
  float z_seg_1(hit1r.Z());
  float cos11(slices[hit1r.ISlice()]->Param().SinAlpha());
  float sin11(slices[hit1r.ISlice()]->Param().CosAlpha());
  float x_seg_1_g = -(y_seg_1 * cos11 - x_seg_1 * sin11);
  float y_seg_1_g = x_seg_1 * cos11 + y_seg_1 * sin11;
  disp.DrawHitGlobal( x_seg_1_g, y_seg_1_g, z_seg_1, kGreen, 0.25 );
//  if( ih > 0 ) disp.SpecDrawLineG( x_seg_1_g, y_seg_1_g, z_seg_1, x0, y0, z0, 45, 0.15 );
  if( ih > 0 ) disp.SpecDrawLineG( x_seg_1_g, y_seg_1_g, z_seg_1, x0, y0, z0, kGreen, 0.15 );
//  if( ih == 0 && counter > 0 ) disp.SpecDrawLineG( x_seg_1_g, y_seg_1_g, z_seg_1, x0s, y0s, z0s, kRed, 0.15 );
  x0 = x_seg_1_g;
  y0 = y_seg_1_g;
  z0 = z_seg_1;
//  counter++;
//  if( ih == track.NClusters()-1 ) {
//    x0s = x_seg_1_g;
//    y0s = y_seg_1_g;
//    z0s = z_seg_1;
//  }
}
    }
  }
  //
  for( int i = 0; i < fNLoopers; i++ ) {
    std::cout<<"> looper: "<<i<<";   nSegments: "<<loopers[i]<<"\n";
//disp.DrawTPC();
    if( loopers[i] < 2 ) continue;
float x0s(-1000), y0s(-1000), z0s(-1000);
int counter = 0;
    for( int iSeg = 0; iSeg < fSegments.size(); iSeg++ ) {
      if( fSegments[iSeg].iLooper != i ) continue;
//if( !(fSegments[iSeg].QPt_abs > 14.33 && fSegments[iSeg].QPt_abs < 14.34) && !(fSegments[iSeg].QPt_abs > 13.89 && fSegments[iSeg].QPt_abs < 13.91) ) continue;
      std::cout<<" --- iSeg: "<<iSeg<<";   Cx: "<<fSegments[iSeg].Cx<<";   Cy: "<<fSegments[iSeg].Cy<<";   Cr: "<<fSegments[iSeg].Cr<<";   DzDs: "<<fSegments[iSeg].DzDs_abs<<"\n";
      std::cout<<"         x_up: "<<fSegments[iSeg].x_up<<";   x_dn: "<<fSegments[iSeg].x_dn<<";   y_up: "<<fSegments[iSeg].y_up<<";   y_dn: "<<fSegments[iSeg].y_dn<<"\n";
      std::cout<<"         iLooper: "<<fSegments[iSeg].iLooper<<"\n";
//disp.DrawHitGlobal( fSegments[iSeg].Cx, fSegments[iSeg].Cy, fSegments[iSeg].z_dn, kBlack, 0.5 );
//disp.DrawHitGlobal( fSegments[iSeg].x_dn, fSegments[iSeg].y_dn, fSegments[iSeg].z_dn, kGreen, 0.75 );
//disp.DrawHitGlobal( fSegments[iSeg].x_up, fSegments[iSeg].y_up, fSegments[iSeg].z_up, kOrange, 0.75 );
const AliHLTTPCCAMergedTrack &track = fOutput.Track( fSegments[iSeg].iTr );
float x0, y0, z0;
for( int ih = 0; ih < track.NClusters(); ih++ ) {
  const DataCompressor::SliceRowCluster &iDsrc1 = fOutput.ClusterIDsrc( track.FirstClusterRef() + ih );
  int iHit1 = fFirstSliceHit[iDsrc1.Slice()] + slices[iDsrc1.Slice()]->ClusterData().RowOffset( iDsrc1.Row() ) + iDsrc1.Cluster();
  const AliHLTTPCCAGBHit &hit1r = fGBHits[iHit1];
  float x_seg_1(hit1r.X());
  float y_seg_1(hit1r.Y());
  float z_seg_1(hit1r.Z());
  float cos11(slices[hit1r.ISlice()]->Param().SinAlpha());
  float sin11(slices[hit1r.ISlice()]->Param().CosAlpha());
  float x_seg_1_g = -(y_seg_1 * cos11 - x_seg_1 * sin11);
  float y_seg_1_g = x_seg_1 * cos11 + y_seg_1 * sin11;
  disp.DrawHitGlobal( x_seg_1_g, y_seg_1_g, z_seg_1, kBlue, 0.25 );
  if( ih > 0 ) disp.SpecDrawLineG( x_seg_1_g, y_seg_1_g, z_seg_1, x0, y0, z0, kBlue, 0.15 );
//  disp.DrawHitGlobal( x_seg_1_g, y_seg_1_g, z_seg_1, kGreen, 0.25 );
//  if( ih > 0 ) disp.SpecDrawLineG( x_seg_1_g, y_seg_1_g, z_seg_1, x0, y0, z0, kGreen, 0.15 );
//  if( ih == 0 && counter > 0 ) disp.SpecDrawLineG( x_seg_1_g, y_seg_1_g, z_seg_1, x0s, y0s, z0s, kRed, 0.15 );
  x0 = x_seg_1_g;
  y0 = y_seg_1_g;
  z0 = z_seg_1;
  counter++;
  if( ih == track.NClusters()-1 ) {
    x0s = x_seg_1_g;
    y0s = y_seg_1_g;
    z0s = z_seg_1;
  }
}
    }
//disp.Ask();
  }
  disp.SaveCanvasToFile( "DrawLoopersAll1.pdf" );
  disp.Ask();
#endif
}

void AliHLTTPCCALooperMerger::SaveSegments()
{
  if( !fSegments.size() ) return;
  struct SortSegments {
//    SortSegments( LooperSegment* seg ) {
//      iLooper = seg->iLooper;
//      iTrack = seg->iTr;
//      z_h_dn = seg->z_h_dn;
//    }
    int iOrigSeg;
    int iLooper;
    int iTrack;
    float z_h_dn;
    bool grow;
    bool revers;

    static bool comp( const SortSegments &a, const SortSegments &b ) {
      if ( a.iLooper < b.iLooper ) return 1;
      if ( a.iLooper > b.iLooper ) return 0;
      if ( a.revers ) return ( a.z_h_dn > b.z_h_dn );
      return ( a.z_h_dn < b.z_h_dn );
    }
  };
  SortSegments segments[fSegments.size()];
  for( int iSeg = 0; iSeg < fSegments.size(); iSeg++ ) {
    segments[iSeg].iLooper = fSegments[iSeg].iLooper;
    segments[iSeg].iTrack = fSegments[iSeg].iTr;
    segments[iSeg].z_h_dn = fSegments[iSeg].z_h_dn;
    segments[iSeg].iOrigSeg = iSeg;
    if( fSegments[iSeg].z_h_dn < fSegments[iSeg].z_h_up ) segments[iSeg].grow = true;
    else segments[iSeg].grow = false;
    segments[iSeg].revers = false;
  }
  std::sort( &(segments[0]), &(segments[fSegments.size()-1]), SortSegments::comp );
  int tLooper = segments[0].iLooper;
  float tQPt = fSegments[segments[0].iOrigSeg].QPt_abs;
  int tNSeg = 1;
  for( int iSeg = 0; iSeg < fSegments.size()-1; iSeg++ ) {
    if( segments[iSeg].iLooper != tLooper ) {
      if( tQPt > fSegments[segments[iSeg-1].iOrigSeg].QPt_abs ) {
	for( int i = iSeg - tNSeg; i < iSeg; i++ ) segments[i].revers = true;
      }
      tLooper = segments[iSeg].iLooper;
      tQPt = fSegments[segments[iSeg].iOrigSeg].QPt_abs;
      tNSeg = 0;
    }
    tNSeg++;
  }
  std::sort( &(segments[0]), &(segments[fSegments.size()-1]), SortSegments::comp );
  int iLooper = 0;
  for( int iSeg = 1; iSeg < fSegments.size()-1; iSeg++ ) {
    int prevTr = -1;
    iLooper = segments[iSeg].iLooper;
    while( segments[iSeg].iLooper == iLooper ) {
      int nextTr = -1;
      if( segments[iSeg+1].iLooper == iLooper ) nextTr = segments[iSeg+1].iTrack;
      AliHLTTPCCAMergedTrack &track = fOutput.Track( segments[iSeg].iTrack );
      if( prevTr != nextTr ) {
	track.SetLooper( prevTr, nextTr );
	if( segments[iSeg].grow ) track.SetGrow();
	if( segments[iSeg].revers ) track.SetRevers();
      }
      prevTr = segments[iSeg].iTrack;
      iSeg++;
    }
  }
}
