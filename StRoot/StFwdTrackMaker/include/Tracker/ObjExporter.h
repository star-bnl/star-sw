
#ifndef __OBJEXPORTER_H__
#define __OBJEXPORTER_H__
#include "GenFit/FitStatus.h"
#include "GenFit/GFRaveVertexFactory.h"
#include <math.h>

#include "StEvent/StEvent.h"
#include "StEvent/StFttCollection.h"
#include "StEvent/StFttCluster.h"

class StEvent;


class ObjExporter {
public:

    // extra output for info and debug
    const static bool verbose = true;

    // write a single vertex
    void vert( ofstream &of, float x, float y, float z ){
        of << "v " << x << " " << y << " " << z << endl;
        numVertices++;
    }
    void vert( ofstream &of, TVector3 v ){
        of << "v " << v.X() << " " << v.Y() << " " << v.Z() << endl;
        numVertices++;
    }

    // write a sphere with given sub-divsions to output file
    void sphere(TVector3 pt, float radius, int nLatitude, int nLongitude, ofstream &fout){
        int p, s, i, j;
        float x, y, z, out;
        int nPitch = nLongitude + 1;
        const float DEGS_TO_RAD = 3.14159f/180.0f;

        float pitchInc = (180. / (float)nPitch) * DEGS_TO_RAD;
        float rotInc   = (360. / (float)nLatitude) * DEGS_TO_RAD;

        //## PRINT VERTICES:
        vert(fout, pt.X(), pt.Y()+radius, pt.Z());    // Top vertex.
        vert(fout, pt.X(), pt.Y()-radius, pt.Z());    // Bottom vertex.

        int fVert = numVertices;    // Record the first vertex index for intermediate vertices.
        for(p=1; p<nPitch; p++) {    // Generate all "intermediate vertices":
            out = fabs(radius * sin((float)p * pitchInc));
            y   = radius * cos(p * pitchInc);
            for(s=0; s<nLatitude; s++) {
                x = out * cos(s * rotInc);
                z = out * sin(s * rotInc);
                fout << TString::Format("v %g %g %g\n", x+pt.X(), y+pt.Y(), z+pt.Z()).Data();
                numVertices++;
            }
        }

      //## OUTPUT FACES BETWEEN INTERMEDIATE POINTS:

      for(p=1; p<nPitch-1; p++) {
        for(s=0; s<nLatitude; s++) {
          i = p*nLatitude + s;
          j = (s==nLatitude-1) ? i-nLatitude : i;
          fout << TString::Format("f %d %d %d %d\n",
                  (i+1-nLatitude)+fVert, (j+2-nLatitude)+fVert, (j+2)+fVert, (i+1)+fVert);
        }
      }

      //## OUTPUT TRIANGULAR FACES CONNECTING TO TOP AND BOTTOM VERTEX:

      int offLastVerts  = fVert + (nLatitude * (nLongitude-1));
      for(s=0; s<nLatitude; s++)
      {
        j = (s==nLatitude-1) ? -1 : s;
        fout << TString::Format("f %d %d %d\n", fVert-1, (j+2)+fVert,        (s+1)+fVert       ).Data();
        fout << TString::Format("f %d %d %d\n", fVert,   (s+1)+offLastVerts, (j+2)+offLastVerts).Data();
      }
    } // sphere

    // output a single face
    void face( ofstream &fout, int anchor, int v0, int v1, int v2 ){
        fout << TString::Format("f %d %d %d\n", anchor+v0, anchor+v1, anchor+v2).Data();
    }
    void face( ofstream &fout, int anchor, int v0, int v1, int v2, int v3 ){
        fout << TString::Format("f %d %d %d %d\n", anchor+v0, anchor+v1, anchor+v2, anchor+v3).Data();
    }
    // output a general prism
    void prism( ofstream &fout, TVector3 po, TVector3 ds ){
        int fVert = numVertices;    // Record the first vertex index for intermediate vertices.
        // x-y at z=0
        vert( fout, po.X(), po.Y(), po.Z() ); // 1
        vert( fout, po.X() + ds.X(), po.Y(), po.Z() ); // 2
        vert( fout, po.X() + ds.X(), po.Y() + ds.Y(), po.Z() ); // 3
        vert( fout, po.X(), po.Y() + ds.Y(), po.Z() ); // 4

        // x-y at z=+dz
        vert( fout, po.X(), po.Y(), po.Z() + ds.Z() ); // 5
        vert( fout, po.X() + ds.X(), po.Y(), po.Z() + ds.Z() ); // 6
        vert( fout, po.X() + ds.X(), po.Y() + ds.Y(), po.Z() + ds.Z() ); // 7
        vert( fout, po.X(), po.Y() + ds.Y(), po.Z() + ds.Z() ); // 8

        face( fout, fVert, 1, 2, 3, 4 ); // bottom face
        face( fout, fVert, 5, 6, 7, 8 ); // top face
        face( fout, fVert, 1, 2, 6, 5 ); // side 1
        face( fout, fVert, 2, 3, 7, 6 ); // side 2
        face( fout, fVert, 3, 4, 8, 7 ); // side 3
        face( fout, fVert, 1, 4, 8, 5 ); // side 4
    }

    // output a tri
    void tri( ofstream &fout, TVector3 v0, float dz, float w, float h, float phi ) {

        int fVert = numVertices;
        float dphi = 0.1/2;
        float yp = v0.Y() + (sin( phi + dphi ) + cos( phi + dphi ) ) * h;
        float yn = v0.Y() + (sin( phi - dphi ) + cos( phi - dphi ) ) * h;
        float xp = v0.X() + (cos( phi + dphi ) - sin( phi + dphi)) * h;
        float xn = v0.X() + (cos( phi - dphi ) - sin( phi - dphi)) * h;
        // top face
        vert( fout, v0.X(), v0.Y(), v0.Z() ); // 1
        vert( fout, xp, yp, v0.Z() );
        vert( fout, xn, yn, v0.Z() );

        // top face
        vert( fout, v0.X(), v0.Y(), v0.Z() - dz ); // 4
        vert( fout, xp, yp, v0.Z() - dz );
        vert( fout, xn, yn, v0.Z() - dz );

        face( fout, fVert, 1, 2, 3 ); // top
        face( fout, fVert, 4, 5, 6 ); // bottom
        face( fout, fVert, 1, 2, 5, 4 ); // side 1
        face( fout, fVert, 2, 3, 6, 5 ); // side 2
        face( fout, fVert, 3, 1, 4, 6 ); // side 3
    }

    // Compute a track projection as a linear (straight) extrapolation from two points from full projections
  static TVector3 projectAsStraightLine( genfit::Track * t, float* xyz0, float* xyz1, float* xyzf, float* planenorm_xyz, float * cov, TVector3 &mom ) {
    TVector3 tv3A = trackPosition( t, xyz0, planenorm_xyz, cov, mom );
    TVector3 tv3B = trackPosition( t, xyz1, planenorm_xyz, cov, mom );

    if (verbose){
      LOG_INFO << "Straight Line Projection using" << endm;
      LOG_INFO << "A.x = " << tv3A.X() << endm;
      LOG_INFO << "B.x = " << tv3B.X() << endm;
    }
    //Assuming tv3A is starting point of line and tv3B is ending point of line and we are projecting to plane located at xyzf with normal planenorm_xyz
    TVector3 projlinedir = tv3B-tv3A; //particle straight line direction
    //Solution of intersection of line and plane where line has direction {xdir,ydir,zdir}*t and starts at {xorigin,yorigin,zorigin} and a plane that has some normal with a point on the plane; "t" is the free parameter in the parametric equation of the line.
    double tintersection =
      (planenorm_xyz[0]*(xyzf[0]-tv3A[0])+planenorm_xyz[1]*(xyzf[1]-tv3A[1])+planenorm_xyz[2]*(xyzf[2]-tv3A[2])) /
      (planenorm_xyz[0]*projlinedir[0]+planenorm_xyz[1]*projlinedir[1]+planenorm_xyz[2]*projlinedir[2]);
  
    return TVector3( projlinedir[0]*tintersection+tv3A[0], projlinedir[1]*tintersection+tv3A[1], projlinedir[2]*tintersection+tv3A[2] );
    /*
        double dxdz = ( tv3B.X() - tv3A.X() ) / ( tv3B.Z() - tv3A.Z() );
        double dydz = ( tv3B.Y() - tv3A.Y() ) / ( tv3B.Z() - tv3A.Z() );

        double dx = dxdz * ( zf - z1 );
        double dy = dydz * ( zf - z1 );
        TVector3 r( tv3B.X() + dx, tv3B.Y() + dy, zf );
        return r;
    */
    }

    // Project a track to a given xyz-plane with a normal and return its position, momentum, and cov matrix
  static TVector3 trackPosition( genfit::Track * t, float* xyz, float* norm, float * cov, TVector3 &mom ){
    if (verbose){
        LOG_INFO << "trackPosition(" << t << ", " << xyz << ", " << norm << ", " << cov << " )" << endm;
        LOG_INFO << "Projecting to: " << xyz[0] << " " << xyz[1] << " " << xyz[2] << endm;
        LOG_INFO << "Normal: " << norm[0] << " " << norm[1] << " " << norm[2] << endm;
        LOG_INFO << "track has n points: " << t->getNumPointsWithMeasurement() << endm;
    }
    const int iPoint = 0;
    // do not attempt projection if no points with measurements
    // for some reason we can still get here even though we require convergence earlier
    if ( t->getNumPointsWithMeasurement() == 0 ){
        LOG_WARN << "Track has no points with measurement, cannot project" << endm;
        return TVector3( -990, -990, -990 );
    }
    try {
        auto plane = genfit::SharedPlanePtr(
				new genfit::DetPlane(TVector3(xyz), TVector3(norm) )
			);

        genfit::MeasuredStateOnPlane tst = t->getFittedState(iPoint);
        auto TCM = t->getCardinalRep()->get6DCov(tst);
      //  returns the track length if needed
      t->getCardinalRep()->extrapolateToPlane(tst, plane, false, true);
      
      TCM = t->getCardinalRep()->get6DCov(tst);
      
      // can get the projected positions if needed
      float x = tst.getPos().X();
      float y = tst.getPos().Y();
      float _z = tst.getPos().Z();

      mom.SetXYZ( tst.getMom().X(), tst.getMom().Y(), tst.getMom().Z() );
      if (verbose){
        LOG_INFO << "Projected to: " << x << " " << y << " " << _z << endm;
      }
      if ( cov ){
        cov[0] = TCM(0,0); cov[1] = TCM(1,0); cov[2] = TCM(2,0);
        cov[3] = TCM(0,1); cov[4] = TCM(1,1); cov[5] = TCM(2,1);
        cov[6] = TCM(0,2); cov[7] = TCM(1,2); cov[8] = TCM(2,2);
      }


      return TVector3( x, y, _z );
    } catch ( genfit::Exception &e ){
      LOG_INFO << "Track projection Failed from trackPoint " << iPoint  << " E: " << e.what() << endm;
      return TVector3( -990, -990, -990 );
    }


    return TVector3( -990, -990, -990 );
  }

    // Utility method
    static TVector3 trackPosition( genfit::Track * t, float z ){
      float cov[9] = {0}; //Force intialize array to 0 
      TVector3 mom;
      float detpos[3] = {0,0,z};
      float detnorm[3] = {0,0,1};
      return trackPosition( t, detpos, detnorm, cov, mom);
    }

    // Output the sTGC strips into a useful format for event display
    void output_ftt_strips(
        ofstream &fout,
        StEvent * event ){
        fout << "\n" << endl;
        fout << "o fttStrips" << endl;
        fout << "usemtl stgc_hits\n" << endl;
        float pz[] = {280.90499, 303.70498, 326.60501, 349.40499};
        TVector3 cp;
        const float SCALE = 0.1;
        

        for ( size_t i = 0; i < event->fttCollection()->numberOfClusters(); i++ ){
            StFttCluster* c = event->fttCollection()->clusters()[i];
            if ( c->nStrips() < 2 ) continue;
            float dw = 0.05, dlh = 60.0, dlv = 60.0;
            float mx = 0.0, my = 0.0;
            float sx = 1.0, sy = 1.0;


            if ( c->quadrant() == kFttQuadrantA ){
                mx = 0; my = 0;
                sx = 1.0; sy = 1.0;
            } else if ( c->quadrant() == kFttQuadrantB ){
                mx = 10.16*SCALE; my = 0.0*SCALE;
                sy = -1;
                dlv = -dlv;

            } else if ( c->quadrant() == kFttQuadrantC ){
                mx = -10.16*SCALE ; my = -00.0*SCALE;
                sx = -1.0; sy = -1.0;
                dlh = -dlh; dlv = -dlv;

            } else if ( c->quadrant() == kFttQuadrantD ){
                sx = -1;
                dlh = -dlh;
            }

            cp.SetZ( -pz[ c->plane() ] * SCALE );
            if ( c->orientation() == kFttHorizontal ){
                cp.SetY( my + sy * c->x()/10.0 * SCALE );
                cp.SetX( mx );
                prism( fout, cp, TVector3( dlh * SCALE, dw, dw ) );
            } else if ( c->orientation() == kFttVertical ){
                cp.SetX( mx + sx * c->x()/10.0 * SCALE );
                cp.SetY( my );
                prism( fout, cp, TVector3( dw, dlv * SCALE, dw ) );
            }
        }
    } // ftt_strips

    // Output the event info into a useful format for event display
    void output( std::string filename,
            StEvent * event,
            std::vector< Seed_t> seeds,
            std::vector< genfit::Track *> tracks,
            const std::vector< genfit::GFRaveVertex *> &vertices,
            std::vector<TVector3> &fttHits,
            std::vector<TVector3> &fstHits,
            std::vector<TVector3> &fcsPreHits, // EPD = preshower
            std::vector<TVector3> &fcsClusters,
            std::vector<float>    &fcsClusterEnergy
            ){
		
        const float SCALE = 0.1;
		LOG_INFO << "Writing: " << filename << endm;
        numVertices = 0;
        // OPEN output
        ofstream ofile( (filename + ".obj" ).c_str() );

        ofile << "\nmtllib materials.mtl\n\n" << endl;


        output_ftt_strips( ofile, event );

        // Write FWD vertices
        TVector3 startPos;
        if ( vertices.size() > 0 ){
            ofile << "o FwdVertices" << endl;
            for ( auto v : vertices ) {
                startPos.SetXYZ( v->getPos().X(), v->getPos().Y(), v->getPos().Z() );
                sphere( TVector3( v->getPos().X() * SCALE, v->getPos().Y() * SCALE, -v->getPos().Z() * SCALE ), 0.5, 10, 10, ofile );
            }
        }

        //  Write FTT hits (points)
        if ( verbose ){
            LOG_INFO << "Viz has " << fttHits.size() << " FTT Hits" << endm;
        }
        if ( fttHits.size() > 0 ){
            ofile << "\n" << endl;
            ofile << "o fttHits" << endl;
            ofile << "usemtl stgc_hits\n" << endl;
            for ( auto p : fttHits ){
                sphere( TVector3( p.X() * SCALE, p.Y() * SCALE, -p.Z() * SCALE ), 0.15, 12, 12, ofile );
            }
        }

        // write FST hits
        if ( verbose ){
            LOG_INFO << "Viz has " << fstHits.size() << " FST Hits" << endm;
        }
        if ( fstHits.size() > 0 ) {
            ofile << "\n" << endl;
            ofile << "o fstHits" << endl;
            ofile << "usemtl fst_hits\n" << endl;
            for ( auto p : fstHits ){

                // float fstphi = TMath::ATan2( p.Y(), p.X() );
                // printf( "FST PHI: %f \n", fstphi );
                // tri( ofile, TVector3( p.X() * SCALE, p.Y() * SCALE, -p.Z() * SCALE ), 0.1f, 0.1f, 3.0f, fstphi );
                sphere( TVector3( p.X() * SCALE, p.Y() * SCALE, -p.Z() * SCALE ), 0.3, 10, 10, ofile );
            }
        }

        // Output the EPD hits
        if (verbose){
            LOG_INFO << "Viz has " << fcsPreHits.size() << " EPD Hits" << endm;
        }
        if ( fcsPreHits.size() > 0 ){
            ofile << "\n" << endl;
            ofile << "o epd" << endl;
            ofile << "usemtl fcs_hits\n" << endl;
            for ( auto p : fcsPreHits ){

                sphere( TVector3( p.X() * SCALE, p.Y() * SCALE, -p.Z() * SCALE ), 0.25, 10, 10, ofile );
            }
        }

        if (verbose){
            LOG_INFO << "Viz has " << fcsClusters.size() << " FCS Hits" << endm;
        }
        if ( fcsClusters.size() > 0 ){
            ofile << "\n" << endl;
            ofile << "o fcs" << endl;
            ofile << "usemtl fcs_hits\n" << endl;
            int i = 0;
            for ( auto p : fcsClusters ){
                // sphere( TVector3( p.X() * SCALE, p.Y() * SCALE, -p.Z() * SCALE ), 0.75, 10, 10, ofile );
                TVector3 ds = TVector3( 1, 1, fcsClusterEnergy[i] );
                prism( ofile, TVector3( p.X() * SCALE, p.Y() * SCALE, -p.Z() * SCALE ), ds );
                i++;
            }
        }

        // Write the track seeds
        if (verbose){
            LOG_INFO << "Viz has " << seeds.size() << " seeds" << endm;
        }
        if ( seeds.size() > 0 ){
            ofile << "\n\no FwdSeeds\n" << endl;
            ofile << "usemtl seeds\n" << endl;
            // numVertices = 0;
            for ( auto s : seeds ) {
                size_t vStart = numVertices;
                for ( auto h : s ){

                    vert( ofile, h->getX() * SCALE, h->getY() * SCALE, -h->getZ() * SCALE );

                }

                ofile << "l ";
                for ( size_t i = vStart; i < numVertices; i++){
                    ofile << i+1 << " ";
                }
                ofile << endl;
            }
        }

        // uncomment if you want a separate file for tracks
        // ofile.open( (filename + "_tracks.obj" ).c_str()  );
        // numVertices = 0;
        // EXPORT TRACKS

        if (verbose){
            LOG_INFO << "Viz has " << tracks.size() << " tracks Hits" << endm;
        }
        if ( tracks.size() > 0 ){
            ofile << "\n\no FwdTracks\n" << endl;
            ofile << "usemtl tracks\n" << endl;
            float zStep = 5.0; // cm
            for ( auto t : tracks ) {
                size_t vStart = numVertices;


                TVector3 lpoint;
                for ( float z = startPos.Z(); z < 875; z += zStep ){
                    TVector3 point = trackPosition( t, z );
                    if ( point.X() < -900 && point.Y() < -900 ) break;
                    if ( point.X() < -90 && point.Y() < -90 ) { z+= 50; continue;}

                    vert( ofile, point.X() * SCALE, point.Y() * SCALE, -point.Z() * SCALE );
                    lpoint = point;
                }

                ofile << "l ";
                for ( size_t i = vStart; i < numVertices; i++){
                    ofile << i+1 << " ";
                }
                ofile << endl;
            } // for t in tracks
        } // if tracks.size() > 0

        ofile.close();
    }

    size_t numVertices;

};


#endif
