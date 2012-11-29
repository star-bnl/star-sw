#include "StHyperLock.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>

StHyperLock::StHyperLock(const std::string& filename) 
	: mIsLocked(false), mFileName(filename), mFileDescriptor(-1)
{

}

StHyperLock::~StHyperLock() 
{
	unlock();
}

bool StHyperLock::try_lock(useconds_t usec) { // 1 second = 1000000 usec
	if ( usec < 0 || mFileName.empty() ) return false;

	while(mIsLocked == false && usec > 0) {
		mFileDescriptor = open(mFileName.c_str(), O_RDONLY, 0666 );
		if( mFileDescriptor >= 0 && flock( mFileDescriptor, LOCK_EX | LOCK_NB ) < 0 ) {
    	    close( mFileDescriptor );
			mFileDescriptor = -1;
			mIsLocked = false;
			usleep(1000); // sleep for 1 millisecond, then retry
			usec -= 1000;
	    } else {
			mIsLocked = true;
		}
	}

	return mIsLocked;
}

void StHyperLock::unlock() {
	if (!mIsLocked) { 
		return;
	} else if (mFileDescriptor == -1) {
		mIsLocked = false;
	} else if (mIsLocked && mFileDescriptor >= 0) {
		flock( mFileDescriptor, LOCK_UN );
    	close( mFileDescriptor );
		mIsLocked = false;
		mFileDescriptor = -1;
	}
	return;
}
