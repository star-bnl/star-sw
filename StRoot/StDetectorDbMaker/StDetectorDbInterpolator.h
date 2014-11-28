#ifndef StDetectorDbInterpolator_h
#define StDetectorDbInterpolator_h

/*!
  This acts as the interpolator using two arrays. The first array is an sorted unsigned int array of times and the second is a templated array that holds the values corresponding to the times array.

  The time array must be sorted in ascending order.
  
  The interpolater either does a straight line interpolation or it can return the value at the first known tme before the user queryed time.

  This should never be used by the user and is internal the other memebers in this librbary

  \author Jon Gans

  

*/
template<class T>
class StDetectorDbInterpolator{
public:
    StDetectorDbInterpolator(unsigned int numEntries,unsigned int* times,T* array);
    T interpolate(unsigned int time);
    T getLowerValue(unsigned int time);
private:
    unsigned int mNumEntries;
    unsigned int* mTimes;
    T* mArray;
};

///Constructor for time interpolator.
template<class T>
StDetectorDbInterpolator<T>::StDetectorDbInterpolator(unsigned int numEntries,unsigned int* times,T* array){
    mNumEntries = numEntries;
    mTimes = times;
    mArray = array;
};

/// Interpolates value based on timestamp
template<class T>
T StDetectorDbInterpolator<T>::interpolate(unsigned int time){

    if( mNumEntries == 0)
	return 0;
    
    if(time <= mTimes[0])
	return mArray[0];
    if(time >= mTimes[mNumEntries-1])
	return mArray[mNumEntries-1];
    unsigned int i = 0;
    while(mTimes[i] < time && i < mNumEntries)
	i++;
    double relTime = (time - mTimes[i-1]);
    double denominator = (mTimes[i]-mTimes[i-1]);

    if(denominator == 0)
	return mArray[i];
    else
	return (relTime/denominator)*(mArray[i]-mArray[i-1])+mArray[i-1];
    
};

/// Gets the lower value of the passed in timestamp
///  So, if there is a value at 1 and 10 and the user
///  enters time 6, the function returns the value at 1
template<class T>
T StDetectorDbInterpolator<T>::getLowerValue(unsigned int time){

    if( mNumEntries == 0)
	return 0;
    
    if(time <= mTimes[0])
	return mArray[0];
    if(time >= mTimes[mNumEntries-1])
	return mArray[mNumEntries-1];

    unsigned int i = 0;
    
    
    while(mTimes[i] < time && i < mNumEntries){
      i++;
    }
    
    if(i > 0){
      i--;
    }

    return mArray[i];
        
};

#endif
