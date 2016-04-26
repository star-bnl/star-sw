#include "StHyperLock.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>

StHyperLock::StHyperLock(const std::string& filename, bool create_lock_file) 
	: mIsLocked(false), mCreateLockFile(create_lock_file), mFileName(filename), mFileDescriptor(-1)
{

}

StHyperLock::~StHyperLock() 
{
	unlock();
}

bool StHyperLock::try_lock(int32_t usec) { // 1 second = 1000000 usec
	if ( usec < 0 || mFileName.empty() ) return false;
	int flags = O_RDONLY;
	if (mCreateLockFile) {
		flags = O_RDWR|O_CREAT;
	}
	while(mIsLocked == false && usec > 0) {
		mFileDescriptor = open(mFileName.c_str(), flags, 0666 );			
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



