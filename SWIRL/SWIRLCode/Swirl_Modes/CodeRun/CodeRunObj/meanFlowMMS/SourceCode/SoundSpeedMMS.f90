PROGRAM MAIN  

    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE

    INTEGER, PARAMETER :: rDef = REAL64

    
    CHARACTER(30) :: Format
    CHARACTER(100) :: &
        file_place

    INTEGER :: &
        i,&
        numberOfGridPoints

    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        deltaR,&
        epsilon_hat,&
        alpha


    FORMAT = "(F10.3,F10.3)"
    WRITE(file_place,*) &
        "/home/jeff-severino/SWIRL/ObjectOrientedSwirlCode/CodeRunObj/"

    ! open file for extraction
     OPEN(UNIT = 266,&
         FILE =TRIM(ADJUSTL(file_place))//"SoundSpeedL2MMS")
     ! making A file for calculation output
     OPEN(UNIT = 267,&
         FILE =TRIM(ADJUSTL(file_place))//"SoundSpeedL2MMS.post")

    READ(266,*) numberOfGridPoints

    ALLOCATE(deltaR(numberOfGridPoints),&
              alpha(numberOfGridPoints-1),&
        epsilon_hat(numberOfGridPoints))

    DO i = 1,numberOfGridPoints
    

    READ(266,*) deltaR(i), epsilon_hat(i)

    ENDDO

    DO i = 1,numberOfGridPoints-1
    alpha(i) = (LOG(epsilon_hat(i+1)) -&
                LOG(epsilon_hat(i  )))/&
                LOG(0.5_rDef)
    WRITE(267,FORMAT) deltaR(i),alpha(i)
    WRITE(6,FORMAT) deltaR(i),alpha(i)
    
    ENDDO

    DEALLOCATE(deltaR,&
        alpha,&
        epsilon_hat)
    CLOSE(266)
    CLOSE(267)
     
    
END PROGRAM MAIN



