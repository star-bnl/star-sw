#include "StHyperUtilFilesystem.h"

#include "StHyperUtilGeneric.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <cstdlib>
#include <cstdio>
#include <sstream>
#include <limits.h>
#include <sys/statvfs.h>

#include <iostream>

namespace StHyperUtilFilesystem
{

float get_free_space_percentage(const char* path) {
    size_t size_free = 0;
    size_t size_total = 0;
    float percentage = 0;
    struct statvfs info;
    if ( -1 == statvfs ( path, &info) ) {
    } else {
        size_free = info.f_bsize * info.f_bfree;
        size_total = info.f_bsize * info.f_blocks;
        percentage = (float) ( ( (double)size_free / (double)size_total ) * 100.0 );
    }
    return percentage;
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
        char opath[256];
        char *p;
        size_t len;
        strncpy(opath, path.c_str(), sizeof(opath));
        len = strlen(opath);
        if(opath[len - 1] == '/')
                opath[len - 1] = '\0';
        for(p = opath; *p; p++)
                if(*p == '/') {
                        *p = '\0';
                        if(access(opath, F_OK))
                                mkdir(opath, S_IRWXU);
                        *p = '/';
                }
        if(access(opath, F_OK))         /* if path is not terminated with / */
                mkdir(opath, S_IRWXU);
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
            remove(fullname_resolved.c_str());
        }
    }
    closedir(dp);
    if (path_resolved != "/") {
        if (path_resolved.compare(0, base_path.size(), base_path) == 0) {
            remove(path_resolved.c_str());
        } else {
            //std::cerr << "ATTEMPT TO DELETE OUTSIDE OF BASE PATH: " << path_resolved << std::endl; // FIXME
        }
    }
    return count;
}

} // namespace StHyperUtilFilesystem

