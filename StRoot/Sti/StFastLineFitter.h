//StFastLineFitter.h
//M.L. Milelr (Yale Software)
//03/01

/*! \class StFastLineFitter
  Adapted from uitLineFit.c.
  This class performs a linear-least squares regression in two dimensions.
  It assumes that there is no error on the x-component and that all error
  can be projected onto the y-component.
  
  \author Jawluen Tang, Physics department, UT-Austin \n
  \author J. T. Mitchell - adapted for PHENIX use. Converted to C. \n
  \author M. L. Miller   - adapted for STAR use, Converted to C++ \n
  
*/

/** \example StFastLineFitter_ex.cxx
 */

#ifndef StFastLineFitter_HH
#define StFastLineFitter_HH

#include <vector>

using std::vector;

class StFastLineFitter
{
public:
    
    ///Default Constructor
    StFastLineFitter();
    
    ///Default Destructor
    virtual ~StFastLineFitter();
    
    //Gets
    
    ///Return the slope of fit
    double slope() const;
    
    ///Return the intercept of fit
    double intercept() const;
    
    ///Return the chi2 of fit
    double chiSquared() const;
    
    ///Return error on slope
    double sigmaA() const;
    
    ///Return error on intercept
    double sigmaB() const;
    
    ///Return number of points to be fit
    int numberOfPoints() const;
    
    ///Return code of fit
    int rc() const ;
    
    //Sets
    
    ///Add a point to be used in fit
    void addPoint(double x, double y, double weight);
    
    //Action

    ///Clear points stored to be fit
    void clear(); // full reset

    ///Perform the fit.
    bool fit();

    ///Stream the points to be fit to the screen.
    void print() const;
    
private:
    int dofit();
    
    vector<double> mx;
    vector<double> my;
    vector<double> mw;
    
    double mchisq;
    double msiga;
    double msigb;
    double mslope;
    double mintercept;
    int mcode; //Fit return code
};

//inlines

inline double StFastLineFitter::slope() const
{
    return mslope;
}

inline double StFastLineFitter::intercept() const
{
    return mintercept;
}
    
inline double StFastLineFitter::chiSquared() const
{
    return mchisq;
}

inline double StFastLineFitter::sigmaA() const
{
    return msiga;
}

inline double StFastLineFitter::sigmaB() const
{
    return msigb;
}

inline int StFastLineFitter::numberOfPoints() const
{
    return (mx.size()==my.size() && mx.size()==mw.size() ) ? mx.size() : 999;
}

/*! rc = 0: The fit was successful \n
  rc = 1: There were too few points for the fit.  Aborted. \n
  rc = 2: There was a zero determinant.  Aborted
*/
inline int StFastLineFitter::rc() const
{
    return mcode;
}

/*!
  It is assumed that there is no error on x, that all error can be
  projected onto the y ordinate.
*/
inline void StFastLineFitter::addPoint(double x, double y, double weight)
{
    mx.push_back(x);
    my.push_back(y);
    mw.push_back(weight);
}


#endif
