#ifndef __ST_HYPERLOCK_H
#define __ST_HYPERLOCK_H

#include <string>
#include <ctime>
#include <unistd.h>
#include <stdint.h>

class StHyperLock {
	public:
		StHyperLock(const std::string& filename, bool create_lock_file = false);
		~StHyperLock();

		bool try_lock(int32_t usec = 0); // 1 sec = 1000000 usec
		void unlock();

	private:
		StHyperLock(const StHyperLock&);
  		StHyperLock& operator=(const StHyperLock&);

		bool mIsLocked;
		bool mCreateLockFile;
		std::string mFileName;
		int mFileDescriptor;
};

#endif
