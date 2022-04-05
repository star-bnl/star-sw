//StiPolygon.h
//M.L. Miller (Yale Software)
//06/01

#ifndef StiPolygon_HH
#define StiPolygon_HH

/*
  Simple class to represent the skeleton information of a polygon relevant for tracking.
  By no means is this a full representation of a Polygon.

  Define the polygon concentric around the z-axis in a right handed system by:

  1) number of sides
  2) offset of first side w.r.t. x-axis
  3) Radial distance to all sides 

          y
          |            Side Number one at radius=mradius
          |            /
          |         /
          |      /   \    phi0
          |   /        \
          |/             \
          |---------------------------- x

	  Sides are numbered from 0 to n-1 in order of increasing phi.  All angles are in degrees
*/

class StiPolygon
{
public:
    StiPolygon();
    StiPolygon(unsigned int nsides, double phi0, double r);
    virtual ~StiPolygon();

    //Access
    unsigned int numberOfSides() const;
    double phi0() const;
    double radius() const;
    double deltaPhi() const; //Return the azimuthal angle subtended  by each side (2*pi/nsides)

    virtual void setNumberOfSides(unsigned int); //These might be more complicated in derived class
    virtual void setPhi0(double);
    virtual void setRadius(double);

    virtual void write(const char* file) const;
    virtual void build(const char* file);
    
protected:

    unsigned int mnsides; //Number of sides
    double mphi0; //Offset w.r.t x-axis (radians)
    double mradius; //Radial distance to the center of all sides (cm)

private:

};

//inlines--------------------------------------

#endif

