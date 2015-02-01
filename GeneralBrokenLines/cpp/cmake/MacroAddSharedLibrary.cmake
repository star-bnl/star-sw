# create symbolic lib target for calling library targets
#ADD_CUSTOM_TARGET( lib )

MACRO( ADD_SHARED_LIBRARY _name )

    ADD_LIBRARY( ${_name} SHARED ${ARGN} )
    
    #ADD_DEPENDENCIES( lib ${_name} )
   
    # change lib_target properties
    SET_TARGET_PROPERTIES( ${_name} PROPERTIES
        # create *nix style library versions + symbolic links
        VERSION ${${PROJECT_NAME}_VERSION}
        SOVERSION ${${PROJECT_NAME}_SOVERSION}
    )
    
    # install library
    #INSTALL( TARGETS ${_name} DESTINATION lib PERMISSIONS
    #        OWNER_READ OWNER_WRITE OWNER_EXECUTE
    #        GROUP_READ GROUP_EXECUTE
    #        WORLD_READ WORLD_EXECUTE
    #)

ENDMACRO( ADD_SHARED_LIBRARY )

