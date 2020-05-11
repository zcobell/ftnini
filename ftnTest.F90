
        MODULE TstMod
            USE FTNINIMODULE
            CONTAINS
            SUBROUTINE CHECK_ERR(IERR)
                IMPLICIT NONE
                INTEGER,INTENT(IN) :: IERR
                IF(IERR.NE.0)THEN
                    CALL ftnini_errorString()
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

            CALL CHECK_ERR(ftnini_init("fort15.ini",ini))
            CALL CHECK_ERR(ftnini_getDouble(ini,"global","version",version))
            CALL CHECK_ERR(ftnini_getInteger(ini,"global","version",version_int))
            CALL CHECK_ERR(ftnini_getString(ini,"forcing","wind_forcing",wind_forcing))
            WRITE(*,'(A,F0.2)') "Version (Double): ",version
            WRITE(*,'(A,I0)') "Version (Int): ",version_int
            WRITE(*,'(2A)') "Wind Forcing: ",TRIM(wind_forcing)

        END PROGRAM ftnTest

