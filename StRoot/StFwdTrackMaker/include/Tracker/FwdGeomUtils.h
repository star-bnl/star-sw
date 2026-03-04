#ifndef FWD_GEOM_UTILS_H
#define FWD_GEOM_UTILS_H

#include "TGeoVolume.h"
#include "TGeoNode.h"
#include "TGeoMatrix.h"
#include "TGeoNavigator.h"

class FwdGeomUtils {
    public:



        FwdGeomUtils( TGeoManager * gMan ) {
            if ( gMan != nullptr ){
                _navigator = gMan->AddNavigator();
                _gMan = gMan;
            }
        }

        ~FwdGeomUtils(){
            if ( _gMan != nullptr && _navigator != nullptr){
                _gMan->RemoveNavigator( _navigator );
            }
        }

        bool cd( const char* path ){
            // Change to the specified path
            bool ret = _navigator -> cd(path);
            // If successful, set the node, the volume, and the GLOBAL transformation
            // for the requested node.  Otherwise, invalidate these
            if ( ret ) {
                _matrix = _navigator->GetCurrentMatrix();
                _node   = _navigator->GetCurrentNode();
                _volume = _node->GetVolume();
            } else {
                _matrix = 0; _node = 0; _volume = 0;
            }
            return ret;
        }

        vector<double> fttZ( vector<double> defaultZ ) {
            double z0 = fttZ(0);
            if ( z0 > 1.0 ) { // returns 0 on faiure
                vector<double> z = {z0, fttZ(1), fttZ(2), fttZ(3)};
                return z;
            }
            return defaultZ;
        }
        double fttZ( int index ) {

            // This ftt_z_delta is needed to match the z location of hits (midpint of active volume?) to the z location of the mother volume.
            // NOTE: It may be possible to improve this when the higher precision FTT geometry model is added
            const double ftt_z_delta = -0.5825245;
            stringstream spath;
            spath << "/HALL_1/CAVE_1/STGM_1/STFM_" << (index + 1) * 4 << "/";
            bool can = cd( spath.str().c_str() );
            if ( can && _matrix != nullptr ){
                return _matrix->GetTranslation()[2] + ftt_z_delta;
            }
            return 0.0;
        }


        vector<double> fstZ( vector<double> defaultZ ) {
            double z0 = fstZ(0);
            if ( z0 > 1.0 ) { // returns 0 on faiure
                vector<double> z = {z0, fstZ(1), fstZ(2)};
                return z;
            }
            return defaultZ;
        }

        double fstZ( int index ) {
            // starting in FtsmGeom v1.16? or 1.17
            // the index are now 4,5,6
            // hence +4 below
            // also fixed typo, previously was incorrectly FTSD_
            const double z_delta = 1.755;
            stringstream spath;
            spath << "/HALL_1/CAVE_1/FSTM_1/FSTD_" << (index + 4) << "/";
            bool can = cd( spath.str().c_str() );
            if ( can && _matrix != nullptr ){
                return _matrix->GetTranslation()[2] + z_delta;
            }
            return 0.0;
        }

        TVector3 getFttQuadrant( int index, TVector3 &u, TVector3 &v){
            // 0 - 15 is the front face
            // 16 - 31 is the back face

            int iquad = index % 16 + 1; // geometry is 1 - 16
            int iplane = index / 16 + 1; // geometry is 1 - 2

            stringstream spath;
            spath << "/HALL_1/CAVE_1/STGM_1/STFM_" << (iquad) << "/STMG_" << iplane << "/";
            bool can = cd( spath.str().c_str() );
            if ( can && _matrix != nullptr ){
                double x = _matrix->GetTranslation()[0];
                double y = _matrix->GetTranslation()[1];
                double z = _matrix->GetTranslation()[2];
                // Column 0 of R = local x-axis in global space = u
                // Column 1 of R = local y-axis in global space = v
                // GetRotationMatrix() is row-major: element [i*3+j] = R[i][j]
                // Column j is elements [0*3+j], [1*3+j], [2*3+j]
                u.SetXYZ(_matrix->GetRotationMatrix()[0], _matrix->GetRotationMatrix()[3], _matrix->GetRotationMatrix()[6]);
                v.SetXYZ(_matrix->GetRotationMatrix()[1], _matrix->GetRotationMatrix()[4], _matrix->GetRotationMatrix()[7]);
                return TVector3(x, y, z);
            }
            std ::cerr << "Failed to get FTT quadrant origin for index " << index << std::endl;
            return TVector3(0,0,0);
        }

        TVector3 getFstSensorOrigin (int index, TVector3 &u, TVector3 &v) {
            // Maps per-disk electronic wedge index (0–11) to AGML FSTW copy number (1–12).
            // Electronic wedge k has phi-center = (kFstphiStart[k]+kFstphiStop[k])/2 * 30°.
            // AGML places even wedges first (FSTW_1–6, αz=15°,75°,...,315°) then odd
            // wedges (FSTW_7–12, αz=45°,105°,...,345°), so the copy-number ordering
            // does NOT follow azimuthal phi order.
            // Disks 1 and 3 (FSTD_4, FSTD_6) have no disk-level rotation.
            // Disk 2 (FSTD_5) has an additional alphaz=30° in the AGML geometry, so each
            // electronic wedge maps to the GEANT copy one 30°-step earlier in the base-angle
            // sequence — a cyclic left-shift of the standard table by one entry.
            static const int kElecToGeantWedge[12]      = {2, 7, 1, 12, 6, 11, 5, 10, 4, 9, 3, 8};
            static const int kElecToGeantWedgeDisk2[12] = {7, 1, 12, 6, 11, 5, 10, 4, 9, 3, 8, 2};

            // retrive the sensor index that goes from 1-3 from global sensor index
            int sensorIndex = (index % 3) + 1;
            // retrive the wedge index that goes from 1-12 from global sensor index
            int electronicWedge = (index / 3) % 12;  // 0-indexed per-disk electronic wedge
            // retrive the plane index that goes from 4-6 from global sensor index
            int planeIndex = (index / 36) + 4;
            const int *wedgeMap = (planeIndex == 5) ? kElecToGeantWedgeDisk2 : kElecToGeantWedge;
            int wedgeIndex = wedgeMap[electronicWedge];
            // construct the path to the sensor
            stringstream spath;
            spath << "/HALL_1/CAVE_1/FSTM_1/FSTD_" << planeIndex << "/FSTW_" << wedgeIndex << "/FTUS_" << sensorIndex;
            
            if (_verbose ){
                LOG_INFO << "Getting FST sensor origin for index " << index << " with path " << spath.str() << endm;
            }

            bool can = cd( spath.str().c_str() );
            if ( can && _matrix != nullptr ){
                double x = _matrix->GetTranslation()[0];
                double y = _matrix->GetTranslation()[1];
                double z = _matrix->GetTranslation()[2];
                // Column 0 of R = local x-axis in global space = u
                // Column 1 of R = local y-axis in global space = v
                // GetRotationMatrix() is row-major: element [i*3+j] = R[i][j]
                // Column j is elements [0*3+j], [1*3+j], [2*3+j]
                u.SetXYZ(_matrix->GetRotationMatrix()[0], _matrix->GetRotationMatrix()[3], _matrix->GetRotationMatrix()[6]);
                v.SetXYZ(_matrix->GetRotationMatrix()[1], _matrix->GetRotationMatrix()[4], _matrix->GetRotationMatrix()[7]);
                // Even GEANT wedges (FSTW_1–6, placed with alphax=180°) have V pointing
                // clockwise; odd wedges have V counterclockwise.  Normalize V to always
                // point counterclockwise so that hitOnPlane[1] = r·sin(dphi) is consistent
                // across all sensors.  Test: (U × V)·ẑ < 0 means V is clockwise.
                if (u.Cross(v).Z() < 0) v = -v;
                if ( _verbose ){
                    LOG_INFO << "FST Sensor " << index << " origin: " << x << ", " << y << ", " << z << endm;
                    LOG_INFO << "\tSensor " << index << " U = " << u.X() << ", " << u.Y() << ", " << u.Z() << endm;
                    LOG_INFO << "\tSensor " << index << " V = " << v.X() << ", " << v.Y() << ", " << v.Z() << endm;
                }

                return TVector3(x, y, z);
            }
            std ::cerr << "Failed to get FST sensor origin for index " << index << std::endl;
            return TVector3(0,0,0);
        }
    protected:
    TGeoVolume    *_volume    = nullptr;
    TGeoNode      *_node      = nullptr;
    TGeoHMatrix   *_matrix    = nullptr;
    TGeoIterator  *_iter      = nullptr;
    TGeoNavigator *_navigator = nullptr;
    TGeoManager   *_gMan      = nullptr;

    const int _verbose = 1;
};

#endif
