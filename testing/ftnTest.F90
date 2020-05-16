
        MODULE TstMod
            USE FTNINIMODULE
            CONTAINS
            SUBROUTINE CHECK_ERR(IERR,INI)
                IMPLICIT NONE
                INTEGER,INTENT(IN)      :: IERR
                TYPE(FTNINI),INTENT(IN) :: INI
                IF(IERR.NE.0)THEN
                    CALL ftnini_errorString(INI)
                    STOP
                ENDIF
            END SUBROUTINE CHECK_ERR
        END MODULE TstMod

        PROGRAM ftnTest
            USE TstMod
            USE FTNINIMODULE
            IMPLICIT NONE

            TYPE(FTNINI) :: ini
            REAL(8)      :: version
            INTEGER      :: version_int
            CHARACTER(200) :: wind_forcing

            CALL CHECK_ERR(ftnini_init("fort15.ini",ini),ini)
            CALL CHECK_ERR(ftnini_getDouble(ini,"global","version",version),ini)
            CALL CHECK_ERR(ftnini_getInteger(ini,"global","version",version_int),ini)
            !CALL CHECK_ERR(ftnini_getDouble(ini,"global","myfakekey",version),ini)
            CALL CHECK_ERR(ftnini_getString(ini,"forcing","wind_forcing",wind_forcing),ini)
            WRITE(*,'(A,F0.2)') "Version (Double): ",version
            WRITE(*,'(A,I0)') "Version (Int): ",version_int
            WRITE(*,'(2A)') "Wind Forcing: ",TRIM(wind_forcing)

            CALL CHECK_ERR(ftnini_deinit(ini),ini)

        END PROGRAM ftnTest

