#include "StHyperUtilFilesystem.h"

#include "StHyperUtilGeneric.h"
#include "StHyperLock.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <cstdlib>
#include <cstdio>
#include <sstream>
#include <deque>
#include <algorithm>
#include <limits.h>
#include <sys/statvfs.h>
#include <sys/stat.h>
#include <iostream>

namespace {
	struct cached_file_st {
		std::string name;
		size_t size;
		time_t time;
	};
    bool operator < (const cached_file_st& a, const cached_file_st& b) {
          return a.time < b.time;
    }
}

namespace StHyperUtilFilesystem
{

double get_free_space_percentage(const char* path, size_t& bytes_free, size_t& bytes_total) {
    double percentage = 0;
    struct statvfs info;
    if ( -1 == statvfs ( path, &info) ) {
		bytes_free = 0; bytes_total = 0;
    } else {
        bytes_free = info.f_bsize * info.f_bfree;
        bytes_total = info.f_bsize * info.f_blocks;
		if (bytes_total > 0) {
        	percentage = (double) ( ( (double)bytes_free / (double)bytes_total ) * 100.0 );
		}
    }
    return percentage;
}

bool remove_dir_fifo(std::string unlink_path, const std::string& base_path, double bytes_free) {
    std::string path_resolved = resolve_path(unlink_path);
	StHyperUtilGeneric::rtrim(unlink_path, " /\\\n\r\t");
    if (path_resolved != unlink_path) { return false; }
    DIR *dp = NULL;
    struct dirent *ep = NULL;
    dp = NULL;
    dp = opendir(path_resolved.c_str());
    if (dp == NULL) { return false; }
	std::deque<cached_file_st> file_queue;
    struct stat st;
    int ierr;
    while ( (ep = readdir(dp) ) ) {
        if (ep->d_type != DT_REG) { continue; }
        std::string name(ep->d_name);
        if (name == std::string(".") || name == std::string("..")) { continue; }
        std::ostringstream fullname;
        fullname << path_resolved << "/" << name;
        std::string fullname_resolved = resolve_path(fullname.str());
        if (fullname_resolved == "/") { continue; }
        if (fullname_resolved.compare(0, base_path.size(), base_path) != 0) { continue; }
		cached_file_st tmp;
		tmp.name = fullname_resolved;
		tmp.size = StHyperUtilFilesystem::filesize(fullname_resolved.c_str());
		ierr = stat(fullname_resolved.c_str(), &st);
		if ( ierr == 0 ) {
		    tmp.time = st.st_mtime;
			file_queue.push_back(tmp);
		}
    }
    closedir(dp);
	std::sort(file_queue.begin(), file_queue.end());
	// start erasing files until free space reaches desired 'bytes_free'
	size_t cur_bytes_free = 0, cur_bytes_total = 0;
	get_free_space_percentage(path_resolved.c_str(), cur_bytes_free, cur_bytes_total);
	if (cur_bytes_free > bytes_free) return true;
	while (!file_queue.empty() && cur_bytes_free < bytes_free) {
		cached_file_st tmp = file_queue.front();
		cur_bytes_free += tmp.size;
		if ( remove(tmp.name.c_str()) != 0 ) {
			// FIXME: error, file was not removed for some reason
		}
	}
	get_free_space_percentage(path_resolved.c_str(), cur_bytes_free, cur_bytes_total);
	if (cur_bytes_free > bytes_free) { return true; }
	return false;
}

bool remove_dir_lru(std::string unlink_path, const std::string& base_path, double bytes_free) {
    std::string path_resolved = resolve_path(unlink_path);
	StHyperUtilGeneric::rtrim(unlink_path, " /\\\n\r\t");
    if (path_resolved != unlink_path) { return false; }
    DIR *dp = NULL;
    struct dirent *ep = NULL;
    dp = NULL;
    dp = opendir(path_resolved.c_str());
    if (dp == NULL) { return false; }
	std::deque<cached_file_st> file_queue;
    struct stat st;
    int ierr;
    while ( (ep = readdir(dp) ) ) {
        if (ep->d_type != DT_REG) { continue; }
        std::string name(ep->d_name);
        if (name == std::string(".") || name == std::string("..")) { continue; }
        std::ostringstream fullname;
        fullname << path_resolved << "/" << name;
        std::string fullname_resolved = resolve_path(fullname.str());
        if (fullname_resolved == "/") { continue; }
        if (fullname_resolved.compare(0, base_path.size(), base_path) != 0) { continue; }
		cached_file_st tmp;
		tmp.name = fullname_resolved;
		tmp.size = StHyperUtilFilesystem::filesize(fullname_resolved.c_str());
		ierr = stat(fullname_resolved.c_str(), &st);
		if ( ierr == 0 ) {
		    tmp.time = st.st_atime;
			file_queue.push_back(tmp);
		}
    }
    closedir(dp);
	std::sort(file_queue.begin(), file_queue.end());
	// start erasing files until free space reaches desired 'bytes_free'
	size_t cur_bytes_free = 0, cur_bytes_total = 0;
	get_free_space_percentage(path_resolved.c_str(), cur_bytes_free, cur_bytes_total);
	if (cur_bytes_free > bytes_free) return true;
	while (!file_queue.empty() && cur_bytes_free < bytes_free) {
		cached_file_st tmp = file_queue.front();
		cur_bytes_free += tmp.size;
		if ( remove(tmp.name.c_str()) != 0 ) {
			// FIXME: error, file was not removed for some reason
		}
	}
	get_free_space_percentage(path_resolved.c_str(), cur_bytes_free, cur_bytes_total);
	if (cur_bytes_free > bytes_free) { return true; }
	return false;
}


bool path_exists (const std::string& file)
{
    struct stat buffer;
    if (stat(file.c_str(), &buffer) == -1) {
        return false;
    }
    return true;
}

std::string resolve_path(const std::string& path)
{
    char resolved_path [PATH_MAX];
    realpath(path.c_str(), resolved_path);
    return std::string(resolved_path);
}

void create_dir_recursive(std::string path) {
		char *opath = strdup(path.c_str());
        size_t len = strlen(opath);
        if (opath[len - 1] == '/') {
                opath[len - 1] = '\0';
		}
        for (char* p = opath; *p; p++) {
            if (*p == '/') {
                *p = '\0';
                if(access(opath, F_OK)) {
                    if ( mkdir(opath, S_IRWXU) != 0 ) {
						// FIXME: directory was not created for some reason
					}
				}
                *p = '/';
            }
		}
        if ( access(opath, F_OK)) {        /* if path is not terminated with / */
            if ( mkdir(opath, S_IRWXU) != 0 ) {
				// FIXME: directory was not created for some reason
			}
		}
		free (opath);
}

unsigned long remove_dir_recursive(std::string unlink_path, const std::string& base_path)
{
    std::string path_resolved = resolve_path(unlink_path);
	StHyperUtilGeneric::rtrim(unlink_path, " /\\\n\r\t");

    if (path_resolved != unlink_path) {
        //std::cerr << "ERR: RESOLVED PATH != UNLINK_PATH. " << path_resolved << " != " << unlink_path << std::endl; //FIXME
        return 0;
    } // path should be fully specified, no relative paths allowed
    unsigned long count = 1;
    DIR *dp = NULL;
    struct dirent *ep = NULL;
    dp = NULL;
    dp = opendir(path_resolved.c_str());
    if (dp == NULL) {
        //std::cerr << "ERR: BASE DIRECTORY CANNOT BE OPENED. " << path_resolved << std::endl; // FIXME
        return count;
    }
    while ( (ep = readdir(dp) ) ) {
        if (ep->d_type != DT_DIR && ep->d_type != DT_REG) {
            continue;
        }
        std::string name(ep->d_name);
        if (name == std::string(".") || name == std::string("..")) continue;
        std::ostringstream fullname;
        fullname << path_resolved << "/" << name;
        std::string fullname_resolved = resolve_path(fullname.str());
        if (fullname_resolved == "/") {
            continue;
        }

        if (ep->d_type == DT_DIR) {
            count += remove_dir_recursive(fullname_resolved, base_path);
        } else {
            if (fullname_resolved.compare(0, base_path.size(), base_path) != 0) {
                //std::cerr << "ATTEMPT TO DELETE OUTSIDE OF BASE PATH: " << fullname_resolved << std::endl; // FIXME
                continue;
            }
			StHyperLock lock(fullname_resolved);
			if (lock.try_lock(0)) {
				lock.unlock();
            	if ( remove(fullname_resolved.c_str()) != 0 ) {
					// FIXME: error, file was not removed for some reason
				}
			}
			
        }
    }
    closedir(dp);
    if (path_resolved != "/") {
        if (path_resolved.compare(0, base_path.size(), base_path) == 0) {
            if ( remove(path_resolved.c_str()) != 0 ) {
				// FIXME: error, file was not removed for some reason
			}
        } else {
            //std::cerr << "ATTEMPT TO DELETE OUTSIDE OF BASE PATH: " << path_resolved << std::endl; // FIXME
        }
    }
    return count;
}

std::ifstream::pos_type filesize(const char* filename)
{
    std::ifstream in(filename, std::ifstream::in | std::ifstream::binary);
    in.seekg(0, std::ifstream::end);
    return in.tellg(); 
}

} // namespace StHyperUtilFilesystem

