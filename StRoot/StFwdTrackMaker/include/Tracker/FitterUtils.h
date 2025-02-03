#ifndef FITTERUTILS_H
#define FITTERUTILS_H

#include "GenFit/KalmanFitter.h"
#include "GenFit/KalmanFitterInfo.h"
#include "GenFit/KalmanFitterRefTrack.h"
#include "GenFit/MaterialEffects.h"
#include "GenFit/PlanarMeasurement.h"
#include "GenFit/RKTrackRep.h"
#include "GenFit/SpacepointMeasurement.h"
#include "GenFit/StateOnPlane.h"
#include "GenFit/TGeoMaterialInterface.h"
#include "GenFit/Track.h"
#include "GenFit/TrackPoint.h"

#include "TVector3.h"


class FitSeedMaker {
    public:
        FitSeedMaker() {}
        virtual ~FitSeedMaker() {}    
        virtual void makeSeed(Seed_t seed, TVector3 &posSeed, TVector3 &momSeed, int &q ) = 0;
};
class ConstFitSeeder : public FitSeedMaker {
    public:
        ConstFitSeeder() {}
        virtual ~ConstFitSeeder() {}
        virtual void makeSeed(Seed_t seed, TVector3 &posSeed, TVector3 &momSeed, int &q ) {
            posSeed.SetXYZ(0,0,0);
            momSeed.SetXYZ(0,0,10);
            q = 1;
        }
};
class GenericFitSeeder : public FitSeedMaker {
    bool isValid = false;
    public:
        GenericFitSeeder() {}
        virtual ~GenericFitSeeder() {}
        // Simple function to calculate the determinant of a 2x2 matrix
        inline double determinant(double a, double b, double c, double d) {
            return a * d - b * c;
        }
        struct Point {
            double x, y;
        };
        // Function to compute the curvature of a circle given 3 points
        double computeSignedCurvature(const Point& p1, const Point& p2, const Point& p3) {
            // Calculate the lengths of the sides of the triangle
            double A = std::sqrt(std::pow(p2.x - p1.x, 2) + std::pow(p2.y - p1.y, 2));
            double B = std::sqrt(std::pow(p3.x - p2.x, 2) + std::pow(p3.y - p2.y, 2));
            double C = std::sqrt(std::pow(p1.x - p3.x, 2) + std::pow(p1.y - p3.y, 2));

            // Calculate the determinant of the matrix formed by the points
            double det = determinant(p2.x - p1.x, p2.y - p1.y, p3.x - p1.x, p3.y - p1.y);
            // LOG_INFO << "Det: " << det << endm;
            double charge = det > 0 ? -1 : 1;
            // Area of the triangle formed by the three points
            double area = std::abs(det) / 2.0;

            if (area == 0) {
                std::cerr << "The points are collinear, curvature is undefined." << std::endl;
                return -1; // Curvature is undefined for collinear points
            }

            // Calculate the radius of the circumcircle using the formula:
            // R = (A * B * C) / (4 * area)
            double radius = (A * B * C) / (4 * area);
            // LOG_INFO << "Radius: " << radius << endm;
            // Curvature is the inverse of the radius
            return charge / radius;
        }
        // Function to compute the average curvature for all combinations of 3 points
        double averageCurvature( const Seed_t points ) {
            // const std::vector<Point>& points;
            int numPoints = points.size();
            if (numPoints < 3) {
                std::cerr << "Not enough points to form a circle." << std::endl;
                return -1;
            }

            double totalCurvature = 0.0;
            int validCombinations = 0;

            // Iterate over all combinations of 3 points
            for (int i = 0; i < numPoints - 2; ++i) {
                for (int j = i + 1; j < numPoints - 1; ++j) {
                    for (int k = j + 1; k < numPoints; ++k) {
                        Point p0 = {points[i]->getX(), points[i]->getY()};
                        Point p1 = {points[j]->getX(), points[j]->getY()};
                        Point p2 = {points[k]->getX(), points[k]->getY()};
                        double curvature = computeSignedCurvature(p0, p1, p2);
                        if (curvature != -1) {  // Exclude invalid (collinear) combinations
                            totalCurvature += curvature;
                            ++validCombinations;
                        }
                    }
                }
            }

            if (validCombinations == 0) {
                std::cerr << "No valid curvature calculations possible." << std::endl;
                return -1;  // No valid triangles were found
            }

            return totalCurvature / validCombinations;
        }

        template <typename T> int sgn(T val) {
            return (T(0) < val) - (val < T(0));
        }
        virtual void makeSeed(Seed_t seed, TVector3 &posSeed, TVector3 &momSeed, int &q ) {
            const double qc = averageCurvature(seed);
            posSeed.SetXYZ(0,0,0);
            momSeed.SetXYZ(0,0,10);
        
            const double BStrength = 0.5; // 0.5 T
            const double C = 0.3 * BStrength; //C depends on the units used for momentum and Bfield (here GeV and Tesla)
            const double K = 0.00029979; // K depends on the units used for Bfield and momentum (here Gauss and GeV)
            double pt = fabs((K*5)/qc); // pT from average measured curv
            
            // set the momentum seed's transverse momentum
            momSeed.SetPerp(pt);
            // compute the seed's eta from seed points
            TVector3 p0 = TVector3(seed[0]->getX(), seed[0]->getY(), seed[0]->getZ());
            TVector3 p1 = TVector3(seed[1]->getX(), seed[1]->getY(), seed[1]->getZ());
            double dx = (p1.X() - p0.X());
            double dy = (p1.Y() - p0.Y());
            double dz = (p1.Z() - p0.Z());
            double phi = TMath::ATan2(dy, dx);
            double Rxy = sqrt(dx * dx + dy * dy);
            double theta = TMath::ATan2(Rxy, dz);
            if (abs(dx) < 1e-6 || abs(dy) < 1e-6){
                phi = TMath::ATan2( p1.Y(), p1.X() );
            }

            // momSeed.SetPhi(phi);
            // momSeed.SetTheta(theta);
            
            // assign charge based on sign of curvature
            q = sgn<double>(qc);
        }
};

#endif