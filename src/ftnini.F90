!------------------------------GPL---------------------------------------!
! This file is part of FtnIni.
!
! (c) 2020 Zachary Cobell
!
! FtnIni is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! FtnIni is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with FtnIni.  If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------!
        MODULE FTNINIMODULE
          USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_PTR,C_INT,C_DOUBLE, &
              C_FLOAT,C_CHAR,C_BOOL,C_NULL_CHAR
          IMPLICIT NONE

          TYPE FTNINI
              TYPE(C_PTR),PRIVATE :: ptr
          END TYPE FTNINI

          INTERFACE

            TYPE(C_PTR) FUNCTION c_createFtnIni(functionName,ierr) &
                    BIND(C,NAME="ftnini_init")
                USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR,C_CHAR,C_INT
                IMPLICIT NONE
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: functionName
                INTEGER(C_INT)                    :: ierr
            END FUNCTION c_createFtnIni

            SUBROUTINE c_deleteFtnIni(ptr) BIND(C,NAME="ftnini_deinit")
                USE,INTRINSIC :: ISO_C_BINDING,ONLY:C_PTR
                IMPLICIT NONE
                TYPE(C_PTR),VALUE,INTENT(IN) :: ptr
            END SUBROUTINE c_deleteFtnIni

            INTEGER(KIND=C_INT) FUNCTION c_ftnini_getbool(ptr,section,name,value) &
                BIND(C,NAME="ftnini_getBool") RESULT(rslt)
                USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_BOOL,C_CHAR,C_PTR,C_INT
                IMPLICIT NONE
                TYPE(C_PTR),VALUE,INTENT(IN)      :: ptr
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: section
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: name
                LOGICAL(KIND=C_BOOL),INTENT(OUT)  :: value
            END FUNCTION c_ftnini_getbool

            INTEGER(KIND=C_INT) FUNCTION c_ftnini_getDouble(ptr,section,name,value) &
                BIND(C,NAME="ftnini_getDouble") RESULT(rslt)
                USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_DOUBLE,C_CHAR,C_PTR,C_INT
                IMPLICIT NONE
                TYPE(C_PTR),VALUE,INTENT(IN)      :: ptr
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: section
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: name
                REAL(C_DOUBLE),INTENT(OUT)        :: value
            END FUNCTION c_ftnini_getDouble

            INTEGER(KIND=C_INT) FUNCTION c_ftnini_getFloat(ptr,section,name,value) &
                BIND(C,NAME="ftnini_getDouble") RESULT(rslt)
                USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_FLOAT,C_CHAR,C_PTR,C_INT
                IMPLICIT NONE
                TYPE(C_PTR),VALUE,INTENT(IN)      :: ptr
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: section
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: name
                REAL(C_FLOAT),INTENT(OUT)         :: value
            END FUNCTION c_ftnini_getFloat

            INTEGER(KIND=C_INT) FUNCTION c_ftnini_getInteger(ptr,section,name,value,ok) &
                BIND(C,NAME="ftnini_getInteger") RESULT(rslt)
                USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_BOOL,C_DOUBLE,C_CHAR,C_PTR,C_INT
                IMPLICIT NONE
                TYPE(C_PTR),VALUE,INTENT(IN)      :: ptr
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: section
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: name
                INTEGER(C_INT),INTENT(OUT)        :: value
                LOGICAL(C_BOOL),INTENT(OUT)       :: ok
            END FUNCTION c_ftnini_getInteger

            INTEGER(KIND=C_INT) FUNCTION c_ftnini_getString(ptr,section,name) &
                BIND(C,NAME="ftnini_getString")
                USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_CHAR,C_PTR,C_INT
                IMPLICIT NONE
                TYPE(C_PTR),VALUE,INTENT(IN)      :: ptr
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: section
                CHARACTER(KIND=C_CHAR),INTENT(IN) :: name
            END FUNCTION c_ftnini_getString

            SUBROUTINE c_ftnini_errorString(ptr) &
                BIND(C,NAME="ftnini_errorString")
                USE,INTRINSIC :: ISO_C_BINDING,ONLY: C_PTR
                IMPLICIT NONE
                TYPE(C_PTR),VALUE,INTENT(IN) :: ptr
            END SUBROUTINE c_ftnini_errorString

          END INTERFACE

          CHARACTER(:),ALLOCATABLE,PRIVATE :: c_string_buffer
          INTEGER,PRIVATE                  :: c_string_length

        CONTAINS

          INTEGER FUNCTION ftnini_init(filename, this) RESULT(ierr)
              TYPE(FTNINI),INTENT(OUT)   :: this
              CHARACTER(*),INTENT(IN)    :: filename
              this%ptr = c_createFtnIni(filename//C_NULL_CHAR,ierr)
          END FUNCTION ftnini_init

          INTEGER FUNCTION ftnini_deinit(this) RESULT(ierr)
              IMPLICIT NONE
              TYPE(FTNINI),INTENT(IN) :: this
              CALL c_deleteFtnIni(this%ptr)
              ierr = 0
          END FUNCTION ftnini_deinit

          SUBROUTINE c2f_copyStringToFortran(c_string,c_string_len) BIND(C,NAME="c2f_copyStringToFortran")
              IMPLICIT NONE
              CHARACTER(KIND=C_CHAR),INTENT(IN)    :: c_string(*)
              INTEGER(KIND=C_INT),INTENT(IN),VALUE :: c_string_len
              INTEGER                              :: I
              IF(ALLOCATED(c_string_buffer))DEALLOCATE(c_string_buffer)
              c_string_length = c_string_len
              ALLOCATE(CHARACTER(c_string_length) :: c_string_buffer)
              FORALL(i=1:c_string_length)c_string_buffer(I:I) = c_string(I)
          END SUBROUTINE c2f_copyStringToFortran

          INTEGER FUNCTION ftnini_getBool(ptr, section, name, rslt) RESULT(ierr)
              IMPLICIT NONE
              TYPE(FTNINI),INTENT(IN) :: ptr
              CHARACTER(*),INTENT(IN) :: section
              CHARACTER(*),INTENT(IN) :: name
              LOGICAL,INTENT(OUT)     :: rslt
              LOGICAL(C_BOOL)         :: c_rslt
              ierr = c_ftnini_getbool(ptr%ptr,section//C_NULL_CHAR,name//C_NULL_CHAR,c_rslt)
              rslt = c_rslt
          END FUNCTION ftnini_getBool

          INTEGER FUNCTION ftnini_getInteger(ptr, section, name, rslt, ok) RESULT(ierr)
              IMPLICIT NONE
              TYPE(FTNINI),INTENT(IN)              :: ptr
              CHARACTER(*),INTENT(IN)              :: section
              CHARACTER(*),INTENT(IN)              :: name
              INTEGER,INTENT(OUT)                  :: rslt
              LOGICAL,INTENT(OUT),OPTIONAL         :: ok
              INTEGER(C_INT)                       :: c_rslt
              LOGICAL(C_BOOL)                      :: isok

              ierr = c_ftnini_getInteger(ptr%ptr,section//C_NULL_CHAR,&
                                         name//C_NULL_CHAR,c_rslt,isok)
              rslt = INT(c_rslt)
              IF(PRESENT(ok))THEN
                ok = isok
              ENDIF
          END FUNCTION ftnini_getInteger

          INTEGER FUNCTION ftnini_getFloat(ptr, section, name, rslt) RESULT(ierr)
              IMPLICIT NONE
              TYPE(FTNINI),INTENT(IN) :: ptr
              CHARACTER(*),INTENT(IN) :: section
              CHARACTER(*),INTENT(IN) :: name
              REAL(4)                 :: rslt
              REAL(C_FLOAT)           :: c_rslt
              ierr = c_ftnini_getFloat(ptr%ptr,section//C_NULL_CHAR,name//C_NULL_CHAR,c_rslt)
              rslt = c_rslt
          END FUNCTION ftnini_getFloat

          INTEGER FUNCTION ftnini_getDouble(ptr, section, name, rslt) RESULT(ierr)
              IMPLICIT NONE
              TYPE(FTNINI),INTENT(IN) :: ptr
              CHARACTER(*),INTENT(IN) :: section
              CHARACTER(*),INTENT(IN) :: name
              REAL(8),INTENT(OUT)     :: rslt
              REAL(C_DOUBLE)          :: c_rslt
              ierr = c_ftnini_getDouble(ptr%ptr,section//C_NULL_CHAR,name//C_NULL_CHAR,c_rslt)
              rslt = DBLE(c_rslt)
          END FUNCTION ftnini_getDouble

          INTEGER FUNCTION ftnini_getString(ptr, section, name, buffer) RESULT(ierr)
              IMPLICIT NONE
              TYPE(FTNINI),INTENT(IN)           :: ptr
              CHARACTER(*),INTENT(IN)           :: section
              CHARACTER(*),INTENT(IN)           :: name
              CHARACTER(*),INTENT(OUT)          :: buffer
              ierr = c_ftnini_getString(ptr%ptr,section//C_NULL_CHAR,name//C_NULL_CHAR)
              IF(LEN(buffer).LT.LEN(c_string_buffer))THEN
                  WRITE(*,'(A)') "FtnIni Error: String buffer overflow detected."//&
                                 " Increase buffer size"
                  buffer = ""
                  RETURN
              ENDIF
              buffer = c_string_buffer
              IF(ALLOCATED(c_string_buffer))DEALLOCATE(c_string_buffer)
          END FUNCTION ftnini_getString

          SUBROUTINE ftnini_errorString(ptr)
            IMPLICIT NONE
            TYPE(FTNINI),INTENT(IN) :: ptr
            CALL c_ftnini_errorString(ptr%ptr)
          END SUBROUTINE ftnini_errorString

        END MODULE FTNINIMODULE
