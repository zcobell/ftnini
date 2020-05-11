
###########################################################################
#                     CMake Build File for FTNINI
#
#    Written By: Zach Cobell
#
###########################################################################
#
# The CMake build system enable FtnIni to be deployed and built
# in a cross platform environment. 
#
###########################################################################
INCLUDE (CheckIncludeFiles)
INCLUDE (CheckLibraryExists) 
INCLUDE (CheckFunctionExists)
INCLUDE (GNUInstallDirs)
INCLUDE (CMakePackageConfigHelpers)

#...Set the default build type
IF(DEFINED CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of
        build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug
        Release RelWithDebInfo MinSizeRel.")
ELSEIF(COVERAGE)
        SET(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build,
            options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
            RelWithDebInfo MinSizeRel.")
ELSE()
    SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build,
        options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
        RelWithDebInfo MinSizeRel.")
ENDIF()

###########################################################################
#  GENERAL OPTIONS
###########################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12)
PROJECT(ftnini CXX Fortran)
###########################################################################


###########################################################################
# Enable running tests
###########################################################################
IF(UNIX OR CYGWIN)
    ENABLE_TESTING()
ENDIF(UNIX OR CYGWIN)
###########################################################################


###########################################################################
# Enable Coverage
###########################################################################
#OPTION(COVERAGE "Export Code Coverage report from tests" OFF)
SET(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMakeModules)
IF(COVERAGE)
    IF(CMAKE_COMPILER_IS_GNUCXX) 
        INCLUDE(CodeCoverage)
        setup_target_for_coverage(ftnini_coverage tests coverage)
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic -pthread -g -O0 -fprofile-arcs -ftest-coverage")
        SET(CMAKE_Fortran_FLAGS "${CMAKE_CXX_FLAGS} -Wall -pedantic -pthread -g -O0 -fprofile-arcs -ftest-coverage")
    ENDIF(CMAKE_COMPILER_IS_GNUCXX)
ENDIF(COVERAGE)
###########################################################################


###########################################################################
#  Compiler flags 
###########################################################################
SET(CMAKE_POSITION_INDEPENDENT_CODE ON)
MARK_AS_ADVANCED( CLEAR CMAKE_CXX_FLAGS_RELEASE )
MARK_AS_ADVANCED( CLEAR CMAKE_CXX_FLAGS_DEBUG )
MARK_AS_ADVANCED( CLEAR CMAKE_C_FLAGS_RELEASE )
MARK_AS_ADVANCED( CLEAR CMAKE_C_FLAGS_DEBUG )
MARK_AS_ADVANCED( CLEAR CMAKE_CXX_COMPILER )
MARK_AS_ADVANCED( CLEAR CMAKE_C_COMPILER )
###########################################################################


###########################################################################
#  LIBRARY VERSION
###########################################################################
set(FTNINI_VERSION_MAJOR 0)
set(FTNINI_VERSION_MINOR 3)
set(FTNINI_VERSION_PATCH 0)
set(FTNINI_VERSION_STRING ${FTNINI_VERSION_MAJOR}.${FTNINI_VERSION_MINOR}.${FTNINI_VERSION_PATCH})
###########################################################################

OPTION( FTNINI_BUILDSHARED "Build shared object version of FtnIni" OFF )
IF( FTNINI_BUILDSHARED )
    SET( FTNINI_LIBTYPE SHARED )
ELSE( FTNINI_BUILDSHARED )
    SET( FTNINI_LIBTYPE STATIC )
ENDIF( FTNINI_BUILDSHARED )

###########################################################################
# CODE VERSION (GIT)
###########################################################################
EXECUTE_PROCESS( COMMAND git describe --always --tags
                 WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                 OUTPUT_VARIABLE GIT_VERSION
                 RESULT_VARIABLE GIT_RETURN_VALUE
                 OUTPUT_STRIP_TRAILING_WHITESPACE )
IF( NOT "${GIT_RETURN_VALUE}" STREQUAL "0" )
    SET(GIT_VERSION "${FTNINI_VERSION_STRING}.cv")
ENDIF()
MESSAGE(STATUS "FtnIni Version: ${GIT_VERSION}")
###########################################################################

###########################################################################
# TESTING AND BENCHMARKING
###########################################################################
OPTION(FTNINI_BUILD_TESTS "Build test cases" OFF)
###########################################################################

###########################################################################
# ABSEIL SWISS TABLES
###########################################################################
#IF(NOT CYGWIN)
#    OPTION(FTNINI_USE_ABSEIL_FLAT_MAP "Use the Abseil Swiss Tables to increase speed" OFF)
#    IF(FTNINI_USE_ABSEIL_FLAT_MAP)
#        if(MSVC)
#          # /wd4005  macro-redefinition
#          # /wd4068  unknown pragma
#          # /wd4244  conversion from 'type1' to 'type2'
#          # /wd4267  conversion from 'size_t' to 'type2'
#          # /wd4800  force value to bool 'true' or 'false' (performance warning)
#          add_compile_options(/wd4005 /wd4068 /wd4244 /wd4267 /wd4800)
#          add_definitions(/DNOMINMAX /DWIN32_LEAN_AND_MEAN=1 /D_CRT_SECURE_NO_WARNINGS)
#        endif(MSVC)
#        add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/abseil-cpp EXCLUDE_FROM_ALL)
#        MESSAGE(STATUS "Using the Abseil Swiss Tables instead of unordered_map for performance")
#    ENDIF(FTNINI_USE_ABSEIL_FLAT_MAP)    
#ELSE(NOT CYGWIN)
#    SET(FTNINI_USE_ABSEIL_FLAT_MAP FALSE)
#ENDIF(NOT CYGWIN)
###########################################################################

###########################################################################
#  SET THE LOCATION OF TEMPORARY STATIC LIBS
###########################################################################
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles)
###########################################################################

###########################################################################
#  FtnInik Library
###########################################################################
SET( FTNINI_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/ftnini.cpp ${CMAKE_CURRENT_SOURCE_DIR}/ftnini.F90 )

ADD_LIBRARY( ftnini ${FTNINI_LIBTYPE} ${FTNINI_SOURCES} )

SET_TARGET_PROPERTIES( ftnini PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/ftnini )

TARGET_COMPILE_DEFINITIONS( ftnini PRIVATE GIT_VERSION="${GIT_VERSION}")
TARGET_COMPILE_DEFINITIONS( ftnini PRIVATE FTNINI_LIBRARY )

SET_TARGET_PROPERTIES( ftnini PROPERTIES VERSION ${FTNINI_VERSION_STRING} SOVERSION ${FTNINI_VERSION_MAJOR} )

SET_TARGET_PROPERTIES( ftnini PROPERTIES PUBLIC_HEADER "${HEADER_LIST}" ) 

IF(${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local" OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/local/" OR
   ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/" OR ${CMAKE_INSTALL_PREFIX} STREQUAL "/usr/")
    SET(HEADER_DEST "${CMAKE_INSTALL_INCLUDEDIR}/ftnini")
ELSE()
    SET(HEADER_DEST ${CMAKE_INSTALL_INCLUDEDIR})
ENDIF()

WRITE_BASIC_PACKAGE_VERSION_FILE( ftniniConfigVersion.cmake VERSION ${FTNINI_VERSION_STRING} COMPATIBILITY SameMajorVersion )
INSTALL( TARGETS ftnini        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT FTNINI_RUNTIME
                               LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT FTNINI_RUNTIME
                               ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT FTNINI_DEVELOPMENT
                               PUBLIC_HEADER DESTINATION ${HEADER_DEST}    COMPONENT FTNINI_DEVELOPMENT )
INSTALL( FILES ${CMAKE_CURRENT_BINARY_DIR}/ftniniConfigVersion.cmake DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake )

SET_TARGET_PROPERTIES(ftnini PROPERTIES CMAKE_CXX_VISIBILITY_PRESET hidden)
SET_TARGET_PROPERTIES(ftnini PROPERTIES CMAKE_CXX_INLINES_HIDDEN YES)

IF(APPLE)
    SET(CMAKE_MACOSX_RPATH 0)
    SET_TARGET_PROPERTIES(ftnini PROPERTIES INSTALL_NAME_DIR "ftnini")
    SET_TARGET_PROPERTIES(ftnini PROPERTIES MACOSX_RPATH "ftnini")
ENDIF(APPLE)

###########################################################################

ADD_EXECUTABLE( ftn_testing ${CMAKE_CURRENT_SOURCE_DIR}/ftnTest.F90 )
TARGET_LINK_LIBRARIES( ftn_testing ftnini )
TARGET_INCLUDE_DIRECTORIES( ftn_testing PRIVATE ${CMAKE_BINARY_DIR}/CMakeFiles/ftnini )

ADD_EXECUTABLE( c_testing ${CMAKE_CURRENT_SOURCE_DIR}/cTest.cpp )


#IF(SMARTSTACK_BUILD_TESTS)
#    SET(TEST_LIST ftn_testSmartStack.F90 ftn_testSmartStackParallel.F90 cxx_testSmartStack.cpp )
#    FOREACH(TESTFILE ${TEST_LIST} ) 
#        GET_FILENAME_COMPONENT( TESTNAME ${TESTFILE} NAME_WE )
#        ADD_EXECUTABLE( ${TESTNAME} ${CMAKE_SOURCE_DIR}/tests/${TESTFILE} )
#        ADD_DEPENDENCIES( ${TESTNAME} ftnini )
#        TARGET_INCLUDE_DIRECTORIES( ${TESTNAME} PRIVATE ${CMAKE_SOURCE_DIR}/libsmartstack ${CMAKE_BINARY_DIR}/CMakeFiles/smartstack ) 
#        TARGET_LINK_LIBRARIES( ${TESTNAME} smartstack )
#        SET_TARGET_PROPERTIES( ${TESTNAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/tests )
#
#        GET_FILENAME_COMPONENT( TEST_EXTENSION ${TESTFILE} EXT ) 
#
#        IF( ${TEST_EXTENSION} STREQUAL ".F90" )
#            SET_TARGET_PROPERTIES(${TESTNAME} PROPERTIES LINKER_LANGUAGE Fortran)
#        ENDIF()
#    
#        ADD_TEST( NAME TEST_${TESTNAME} COMMAND ${CMAKE_BINARY_DIR}/CMakeFiles/tests/${TESTNAME}
#            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/tests )
#        IF(CYGWIN)
#           SET_TESTS_PROPERTIES( TEST_${TESTNAME} PROPERTIES ENVIRONMENT "PATH=$ENV{PATH}:${CMAKE_BINARY_DIR}")
#        ELSE(CYGWIN)
#           SET_TESTS_PROPERTIES( TEST_${TESTNAME} PROPERTIES ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_BINARY_DIR}")
#        ENDIF(CYGWIN)
#    ENDFOREACH()
#ENDIF(SMARTSTACK_BUILD_TESTS)

